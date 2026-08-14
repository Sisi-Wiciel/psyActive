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

observation_item_bound <- function(item, side) {
  direct <- if (side == "lower") c("minimum", "min") else c("maximum", "max")
  for (name in direct) {
    value <- item[[name]]
    if (!is.null(value) && length(value)) return(value[1L])
  }
  range <- item$range
  if (is.list(range)) {
    name <- if (side == "lower") "minimum" else "maximum"
    value <- range[[name]]
    if (is.null(value)) value <- range[[if (side == "lower") "min" else "max"]]
    if (!is.null(value) && length(value)) return(value[1L])
  }
  NULL
}

validate_observation_mapping <- function(mapping) {
  if (!is.list(mapping)) {
    psy_abort("mapping must be a named list.", "psy_error_schema")
  }
  mapping_names <- names(mapping)
  if (is.null(mapping_names) || anyNA(mapping_names) ||
      any(!nzchar(mapping_names)) || anyDuplicated(mapping_names)) {
    psy_abort(
      "mapping must have unique, non-empty names.",
      "psy_error_schema"
    )
  }
  required <- c(
    "person_id", "assessment_id", "observed_at", "instrument_id",
    "instrument_version", "item_id", "record_id"
  )
  missing <- required[!required %in% mapping_names |
                        vapply(required, function(key) {
                          is.null(mapping[[key]]) || length(mapping[[key]]) == 0L
                        }, logical(1))]
  if (length(missing)) {
    psy_abort(
      paste0("mapping is missing required field(s): ", paste(missing, collapse = ", "), "."),
      "psy_error_schema"
    )
  }
  value_fields <- c("value_raw", "value_num", "missing_reason")
  if (!any(value_fields %in% mapping_names)) {
    psy_abort(
      "mapping must include value_raw, value_num, or missing_reason.",
      "psy_error_schema"
    )
  }
  invisible(TRUE)
}

source_timezones <- function(x, mapping, observed_raw, timezone) {
  if (!is.null(timezone) &&
      (length(timezone) != 1L || is.na(timezone) || !nzchar(timezone) ||
       !(timezone %in% OlsonNames()))) {
    psy_abort("timezone must be a non-empty scalar IANA timezone.", "psy_error_schema")
  }

  has_mapped_timezone <- "source_timezone" %in% names(mapping)
  mapped <- if (has_mapped_timezone) {
    as.character(resolve_mapping(x, mapping, "source_timezone"))
  } else {
    rep(NA_character_, NROW(x))
  }
  input_timezone <- attr(observed_raw, "tzone")
  input_timezone <- if (length(input_timezone) &&
                        !is.na(input_timezone[1L]) &&
                        nzchar(input_timezone[1L])) {
    as.character(input_timezone[1L])
  } else {
    NULL
  }
  if (inherits(observed_raw, "POSIXt") && !is.null(input_timezone)) {
    if (!(input_timezone %in% OlsonNames())) {
      psy_abort(
        "POSIXct/POSIXlt timestamps must carry a valid IANA timezone attribute.",
        "psy_error_schema"
      )
    }
    if (!is.null(timezone) && !identical(as.character(timezone), input_timezone)) {
      psy_abort(
        paste0(
          "timezone conflicts with the timezone attribute carried by the ",
          "POSIXct/POSIXlt timestamps."
        ),
        "psy_error_schema"
      )
    }
  }
  fallback <- timezone %||% input_timezone
  explicit_offset <- if (inherits(observed_raw, "POSIXt")) {
    rep(FALSE, NROW(x))
  } else {
    values <- as.character(observed_raw)
    !is.na(values) & grepl(
      "(?:Z|[+-][0-9]{2}:?[0-9]{2})$", values, perl = TRUE
    )
  }
  missing <- is.na(mapped) | !nzchar(mapped)
  if (any(missing) && !is.null(fallback)) mapped[missing] <- fallback
  # An explicit ISO-8601 offset is sufficient to identify the instant. Keep a
  # valid canonical timezone in the normalized schema when no IANA source
  # timezone was supplied for such a row.
  missing <- is.na(mapped) | !nzchar(mapped)
  if (any(missing & explicit_offset)) mapped[missing & explicit_offset] <- "UTC"

  if (NROW(x) && any(is.na(mapped) | !nzchar(mapped))) {
    psy_abort(
      paste0(
        "A source timezone is required for timestamps without explicit timezone metadata. ",
        "Supply timezone or map source_timezone."
      ),
      "psy_error_schema"
    )
  }
  bad <- !is.na(mapped) & (!(mapped %in% OlsonNames()) | !nzchar(mapped))
  if (any(bad)) {
    psy_abort(
      paste0(
        "source_timezone must contain valid IANA timezone names. Invalid value(s): ",
        paste(unique(mapped[bad]), collapse = ", ")
      ),
      "psy_error_schema"
    )
  }
  if (inherits(observed_raw, "POSIXt") && !is.null(input_timezone)) {
    mapped_unique <- unique(mapped)
    if (length(mapped_unique) != 1L ||
        !identical(mapped_unique, input_timezone)) {
      psy_abort(
        paste0(
          "A POSIXct/POSIXlt vector represents absolute instants with one timezone attribute; ",
          "it cannot be reinterpreted using a different or row-specific source_timezone."
        ),
        "psy_error_schema"
      )
    }
  }
  mapped
}

mapped_source_names <- function(mapping, source_names) {
  specs <- unname(mapping)
  unique(unlist(lapply(specs, function(spec) {
    if (length(spec) == 1L && is.character(spec) && spec %in% source_names) spec else character()
  }), use.names = FALSE))
}

observation_identity_key <- function(x) {
  observed_at_utc <- if (inherits(x$observed_at, "POSIXt")) {
    as.numeric(x$observed_at)
  } else {
    as.character(x$observed_at)
  }
  data.frame(
    person_id = as.character(x$person_id),
    record_id = as.character(x$record_id),
    assessment_id = as.character(x$assessment_id),
    instrument_id = as.character(x$instrument_id),
    instrument_version = as.character(x$instrument_version),
    item_id = as.character(x$item_id),
    source_system = as.character(x$source_system),
    observed_at_utc = observed_at_utc,
    stringsAsFactors = FALSE
  )
}

#' Convert Data to Standard Psychiatric Observations
#'
#' Creates an item-level longitudinal data object while retaining source values.
#' Direct identifiers should be removed before calling this function.
#' @param x A data frame.
#' @param mapping Named list mapping standard fields to source column names or constants.
#' @param timezone Optional scalar IANA source timezone. It is required for
#'   naive character or `Date` timestamps unless `source_timezone` is mapped.
#'   For `POSIXct`/`POSIXlt` input, it must agree with any non-empty timezone
#'   attribute already carried by the object.
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
  validate_observation_mapping(mapping)
  n <- NROW(x)
  observed_raw <- resolve_mapping(x, mapping, "observed_at")
  source_timezone <- source_timezones(x, mapping, observed_raw, timezone)
  observed <- as_utc(observed_raw, source_timezone)
  out <- data.frame(
    observation_id=rep(NA_character_,n),
    person_id=as.character(resolve_mapping(x,mapping,"person_id")),
    assessment_id=as.character(resolve_mapping(x,mapping,"assessment_id")),
    episode_id=as.character(resolve_mapping(x,mapping,"episode_id",NA_character_)),
    observed_at=observed,
    source_timezone=source_timezone,
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
  fill_raw <- is.na(out$value_raw) & !is.na(out$value_num)
  out$value_raw[fill_raw] <- as.character(out$value_num[fill_raw])
  identity_key <- observation_identity_key(out)
  out$observation_id <- vapply(seq_len(n), function(i) {
    stable_id(
      "obs", identity_key$person_id[i], identity_key$record_id[i],
      identity_key$assessment_id[i], identity_key$instrument_id[i],
      identity_key$instrument_version[i], identity_key$item_id[i],
      identity_key$source_system[i], identity_key$observed_at_utc[i]
    )
  }, character(1))
  out <- new_psy_df(out,"psy_observation")
  q <- validate_observation(out, level="schema")
  attr(out,"problems") <- q
  attr(out,"audit") <- new_audit("as_psy_observation",x,out,mapping,NROW(q))
  if (keep_unmapped) {
    attr(out,"unmapped") <- x[setdiff(names(x), mapped_source_names(mapping, names(x))), drop=FALSE]
  }
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
  mandatory <- c("observation_id","person_id","assessment_id","observed_at","source_timezone","time_imputed","instrument_id","instrument_version","item_id","rater_type","source_type","source_system","record_id","ingested_at")
  for (nm in mandatory) {
    missing_value <- is.na(x[[nm]])
    if (is.character(x[[nm]])) missing_value <- missing_value | !nzchar(x[[nm]])
    if (any(missing_value)) {k<-k+1L;q[[k]]<-make_quality("schema_missing_value","error",row_id=paste(which(missing_value),collapse=","),field=nm,message=sprintf("Field '%s' contains missing or empty values.",nm),suggestion="Supply a valid value during mapping.")}
  }
  no_value <- is.na(x$value_raw) & is.na(x$value_num) & is.na(x$missing_reason)
  if (any(no_value)) {k<-k+1L;q[[k]]<-make_quality("schema_no_value","error",row_id=paste(which(no_value),collapse=","),field="value",message="Rows have neither a raw/numeric value nor a missing reason.",suggestion="Map values or a controlled missing reason.")}
  bad_missing <- !is.na(x$missing_reason) & !(x$missing_reason %in% .psy_missing_reasons)
  if (any(bad_missing)) {k<-k+1L;q[[k]]<-make_quality("schema_missing_reason","error",row_id=paste(which(bad_missing),collapse=","),field="missing_reason",message="Unknown missing-reason code.",suggestion=paste("Use one of:",paste(.psy_missing_reasons,collapse=", ")))}
  non_finite <- !is.na(x$value_num) & !is.finite(x$value_num)
  if (any(non_finite)) {k<-k+1L;q[[k]]<-make_quality("schema_non_finite","error",row_id=paste(which(non_finite),collapse=","),field="value_num",message="value_num must contain finite values or NA.",suggestion="Replace infinite and NaN values with valid values or a missing reason.")}
  identity_key <- observation_identity_key(x)
  dup <- duplicated(identity_key) |
    duplicated(identity_key, fromLast = TRUE)
  if (any(dup)) {k<-k+1L;q[[k]]<-make_quality("duplicates","error",row_id=paste(which(dup),collapse=","),message="Duplicate observation identity keys, including the exact UTC observation time.",suggestion="Resolve exact duplicate observations before scoring.")}
  if (!inherits(x$observed_at,"POSIXct")) {k<-k+1L;q[[k]]<-make_quality("schema_time_type","error",field="observed_at",message="observed_at must be POSIXct.",suggestion="Use as_psy_observation() with an explicit timezone.")}
  bad_timezone <- !is.na(x$source_timezone) &
    (!nzchar(x$source_timezone) | !(x$source_timezone %in% OlsonNames()))
  if (any(bad_timezone)) {k<-k+1L;q[[k]]<-make_quality("schema_timezone","error",row_id=paste(which(bad_timezone),collapse=","),field="source_timezone",message="source_timezone contains invalid IANA timezone names.",suggestion="Map an IANA timezone such as UTC or Asia/Shanghai.")}
  if (level=="registered") {
    if (is.null(registry)) {
      k<-k+1L;q[[k]]<-make_quality("registry_required","error",field="registry",message="Registered validation requires a registry.",suggestion="Supply the registry containing the applicable instrument versions.")
    } else {
      files <- list.files(registry$instruments, pattern="\\.rds$", full.names=TRUE)
      definitions <- lapply(files, readRDS)
      definition_keys <- vapply(definitions, function(definition) {
        paste(definition$instrument_id, definition$version, sep="@")
      }, character(1))
      row_keys <- paste(x$instrument_id, x$instrument_version, sep="@")
      unknown <- !(row_keys %in% definition_keys)
      if (any(unknown)) {k<-k+1L;q[[k]]<-make_quality("unregistered_instrument","error",row_id=paste(which(unknown),collapse=","),field="instrument_id",message=paste("Unregistered instrument versions:",paste(unique(row_keys[unknown]),collapse=", ")),suggestion="Register the instrument definition first.")}

      for (definition_key in unique(row_keys[!unknown])) {
        definition <- definitions[[match(definition_key, definition_keys)]]
        rows <- which(row_keys == definition_key)
        item_ids <- vapply(definition$items, function(item) item$item_id, character(1))
        item_index <- match(x$item_id[rows], item_ids)
        bad_item <- is.na(item_index)
        if (any(bad_item)) {k<-k+1L;q[[k]]<-make_quality("unregistered_item","error",row_id=paste(rows[bad_item],collapse=","),field="item_id",message=paste("Items are not registered for",definition_key,":",paste(unique(x$item_id[rows[bad_item]]),collapse=", ")),suggestion="Correct item_id or register the intended instrument version.")}

        for (j in which(!bad_item)) {
          row <- rows[j]
          value <- x$value_num[row]
          if (is.na(value)) next
          item <- definition$items[[item_index[j]]]
          allowed <- suppressWarnings(as.numeric(unlist(item$allowed_values)))
          invalid <- length(allowed) && !value %in% allowed
          lower <- observation_item_bound(item, "lower")
          upper <- observation_item_bound(item, "upper")
          if (!is.null(lower)) invalid <- invalid || value < as.numeric(lower)
          if (!is.null(upper)) invalid <- invalid || value > as.numeric(upper)
          if (invalid) {k<-k+1L;q[[k]]<-make_quality("range","error",row_id=as.character(row),field="value_num",message=sprintf("Value for item '%s' is outside its registered range.",x$item_id[row]),suggestion="Correct the value or instrument/version mapping.")}
        }
      }
    }
  }
  append_quality(q)
}
