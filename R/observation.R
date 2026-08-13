required_observation_fields <- function() c("observation_id","person_id","assessment_id","episode_id","observed_at",
  "source_timezone","time_imputed","instrument_id","instrument_version","item_id","value_raw","value_num","unit",
  "missing_reason","language","rater_type","source_type","source_system","setting","record_id","ingested_at")

resolve_mapping <- function(x, mapping, key, default = NULL) {
  spec <- mapping[[key]]
  if (is.null(spec)) return(rep(default, NROW(x)))
  if (length(spec)==1L && is.character(spec) && spec %in% names(x)) return(x[[spec]])
  if (length(spec)==1L) return(rep(spec, NROW(x)))
  if (length(spec)==NROW(x)) return(spec)
  psy_abort(sprintf("Mapping for '%s' must name a column or supply one value per row.", key), "psy_error_schema")
}

#' Convert Data to Standard Psychiatric Observations
#'
#' Creates an item-level longitudinal data object while retaining source values.
#' Direct identifiers should be removed before calling this function.
#' @param x A data frame.
#' @param mapping Named list mapping standard fields to source column names or constants.
#' @param timezone IANA source timezone when source times are not already POSIXct.
#' @param source_system Non-empty source-system identifier.
#' @param strict If `TRUE`, stop when schema errors are present.
#' @param keep_unmapped Retain source columns in an `unmapped` attribute.
#' @return A `psy_observation` data frame.
#' @export
#' @examples
#' raw <- data.frame(id="p1", assessment="a1", when="2026-01-01 09:00:00",
#'                   item="dms_1", value=2)
#' obs <- as_psy_observation(raw,
#'   mapping=list(person_id="id", assessment_id="assessment", observed_at="when",
#'                item_id="item", value_num="value", instrument_id="demo_mood_9",
#'                instrument_version="1.0.0", record_id="assessment"),
#'   timezone="UTC", source_system="example")
#' obs
as_psy_observation <- function(x, mapping, timezone = NULL, source_system, strict = TRUE, keep_unmapped = FALSE) {
  if (!is.data.frame(x)) psy_abort("x must be a data frame.", "psy_error_schema")
  if (missing(source_system) || length(source_system)!=1L || is.na(source_system) || !nzchar(source_system))
    psy_abort("source_system must be a non-empty scalar string.", "psy_error_schema")
  if (!is.list(mapping)) psy_abort("mapping must be a named list.", "psy_error_schema")
  n <- NROW(x)
  observed_raw <- resolve_mapping(x, mapping, "observed_at")
  tz <- timezone %||% mapping$source_timezone %||% if (inherits(observed_raw,"POSIXct")) attr(observed_raw,"tzone") else NULL
  tz <- tz %||% "UTC"
  observed <- as_utc(observed_raw, tz)
  out <- data.frame(
    observation_id=rep(NA_character_,n),
    person_id=as.character(resolve_mapping(x,mapping,"person_id")),
    assessment_id=as.character(resolve_mapping(x,mapping,"assessment_id")),
    episode_id=as.character(resolve_mapping(x,mapping,"episode_id",NA_character_)),
    observed_at=observed,
    source_timezone=as.character(resolve_mapping(x,mapping,"source_timezone",tz)),
    time_imputed=as.logical(resolve_mapping(x,mapping,"time_imputed",inherits(observed_raw,"Date"))),
    instrument_id=as.character(resolve_mapping(x,mapping,"instrument_id")),
    instrument_version=as.character(resolve_mapping(x,mapping,"instrument_version")),
    item_id=as.character(resolve_mapping(x,mapping,"item_id")),
    value_raw=as.character(resolve_mapping(x,mapping,"value_raw",NA_character_)),
    value_num=suppressWarnings(as.numeric(resolve_mapping(x,mapping,"value_num",NA_real_))),
    unit=as.character(resolve_mapping(x,mapping,"unit",NA_character_)),
    missing_reason=as.character(resolve_mapping(x,mapping,"missing_reason",NA_character_)),
    language=as.character(resolve_mapping(x,mapping,"language",NA_character_)),
    rater_type=as.character(resolve_mapping(x,mapping,"rater_type","self")),
    source_type=as.character(resolve_mapping(x,mapping,"source_type","imported")),
    source_system=rep(as.character(source_system),n),
    setting=as.character(resolve_mapping(x,mapping,"setting",NA_character_)),
    record_id=as.character(resolve_mapping(x,mapping,"record_id")),
    ingested_at=rep(now_utc(),n), stringsAsFactors=FALSE)
  if (all(is.na(out$value_raw))) out$value_raw <- ifelse(is.na(out$value_num), NA_character_, as.character(out$value_num))
  out$observation_id <- vapply(seq_len(n), function(i) stable_id("obs",out$source_system[i],out$assessment_id[i],out$item_id[i],format(out$observed_at[i],tz="UTC",usetz=TRUE)), character(1))
  out <- new_psy_df(out,"psy_observation")
  q <- validate_observation(out, level="schema")
  attr(out,"problems") <- q
  attr(out,"audit") <- new_audit("as_psy_observation",x,out,mapping,NROW(q))
  if (keep_unmapped) attr(out,"unmapped") <- x[setdiff(names(x),unname(unlist(mapping))),drop=FALSE]
  if (strict && any(q$severity=="error")) psy_abort(sprintf("Observation conversion produced %d schema error(s). Use strict=FALSE and problems() to inspect them.",sum(q$severity=="error")),"psy_error_schema",problems=q)
  out
}

#' Validate Standard Observations
#' @param x A `psy_observation` or data frame.
#' @param registry Optional registry for item and range validation.
#' @param level Validation level.
#' @return A `psy_quality` data frame.
#' @export
validate_observation <- function(x, registry = NULL, level = c("schema","registered")) {
  level <- match.arg(level); q <- list(); k <- 0L
  miss <- setdiff(required_observation_fields(),names(x))
  if (length(miss)) {k<-k+1L;q[[k]]<-make_quality("schema_missing_field","error",field=paste(miss,collapse=", "),message="Required observation fields are missing.",suggestion="Add or map every required field.")}
  if (length(miss)) return(append_quality(q))
  mandatory <- c("observation_id","person_id","assessment_id","observed_at","source_timezone","instrument_id","instrument_version","item_id","rater_type","source_type","source_system","record_id","ingested_at")
  for (nm in mandatory) if (any(is.na(x[[nm]]) | (is.character(x[[nm]]) & !nzchar(x[[nm]])))) {k<-k+1L;q[[k]]<-make_quality("schema_missing_value","error",field=nm,message=sprintf("Field '%s' contains missing or empty values.",nm),suggestion="Supply a valid value during mapping.")}
  no_value <- is.na(x$value_raw) & is.na(x$value_num) & is.na(x$missing_reason)
  if (any(no_value)) {k<-k+1L;q[[k]]<-make_quality("schema_no_value","error",row_id=paste(which(no_value),collapse=","),field="value",message="Rows have neither a raw/numeric value nor a missing reason.",suggestion="Map values or a controlled missing reason.")}
  bad_missing <- !is.na(x$missing_reason) & !(x$missing_reason %in% .psy_missing_reasons)
  if (any(bad_missing)) {k<-k+1L;q[[k]]<-make_quality("schema_missing_reason","error",row_id=paste(which(bad_missing),collapse=","),field="missing_reason",message="Unknown missing-reason code.",suggestion=paste("Use one of:",paste(.psy_missing_reasons,collapse=", ")))}
  dup <- duplicated(x[c("source_system","assessment_id","item_id")]) | duplicated(x[c("source_system","assessment_id","item_id")],fromLast=TRUE)
  if (any(dup)) {k<-k+1L;q[[k]]<-make_quality("duplicates","error",row_id=paste(which(dup),collapse=","),message="Duplicate source_system + assessment_id + item_id keys.",suggestion="Resolve duplicate source records before scoring.")}
  if (!inherits(x$observed_at,"POSIXct")) {k<-k+1L;q[[k]]<-make_quality("schema_time_type","error",field="observed_at",message="observed_at must be POSIXct.",suggestion="Use as_psy_observation() with an explicit timezone.")}
  if (level=="registered" && !is.null(registry)) {
    keys <- unique(paste(x$instrument_id,x$instrument_version,sep="@")); known <- paste(list_instruments(registry,active=FALSE)$instrument_id,list_instruments(registry,active=FALSE)$version,sep="@")
    unknown <- setdiff(keys,known)
    if (length(unknown)) {k<-k+1L;q[[k]]<-make_quality("unregistered_instrument","error",field="instrument_id",message=paste("Unregistered instrument versions:",paste(unknown,collapse=", ")),suggestion="Register the instrument definition first.")}
  }
  append_quality(q)
}
