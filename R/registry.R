#' Create a psyActive Registry
#' @param path Registry directory. A temporary directory is created if omitted.
#' @return A `psy_registry` object.
#' @export
psy_registry <- function(path = NULL) {
  if (is.null(path)) path <- file.path(tempdir(), "psyActive-registry")
  path <- normalizePath(path, winslash="/", mustWork=FALSE)
  dirs <- file.path(path,c("instruments","references","rules")); invisible(lapply(dirs,dir.create,recursive=TRUE,showWarnings=FALSE))
  structure(list(path=path,instruments=dirs[1],references=dirs[2],rules=dirs[3]),class="psy_registry")
}
print.psy_registry <- function(x, ...) {cat("<psy_registry>",x$path,"\n");cat("  instruments:",length(list.files(x$instruments))," references:",length(list.files(x$references))," rules:",length(list.files(x$rules)),"\n");invisible(x)}
read_definition <- function(path) {
  if (!file.exists(path)) psy_abort(sprintf("Definition file does not exist: %s",path),"psy_error_schema")
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("yml","yaml")) yaml::read_yaml(path)
  else if (ext=="json") jsonlite::fromJSON(path,simplifyVector=FALSE)
  else psy_abort("Definitions must be YAML or JSON.","psy_error_schema")
}
validate_instrument_def <- function(x) {
  req<-c("schema_version","instrument_id","name","version","language","items","scores","license")
  miss<-setdiff(req,names(x));if(length(miss)) psy_abort(paste("Instrument definition is missing:",paste(miss,collapse=", ")),"psy_error_instrument")
  ops<-vapply(x$scores,function(z) z$operation %||% "",character(1));allowed<-c("sum","mean","weighted_sum","count_if")
  if(any(!ops %in% allowed)) psy_abort(paste("Unsupported scoring operation(s):",paste(unique(ops[!ops%in%allowed]),collapse=", ")),"psy_error_instrument")
  ids<-vapply(x$items,function(z) z$item_id %||% "",character(1));if(any(!nzchar(ids))||anyDuplicated(ids)) psy_abort("Instrument item_id values must be non-empty and unique.","psy_error_instrument")
  invisible(TRUE)
}
#' Read an Instrument Definition
#' @param path YAML or JSON definition path.
#' @param validate Validate the declarative schema.
#' @return A `psy_instrument` list.
#' @export
read_instrument <- function(path, validate=TRUE) {x<-read_definition(path);if(validate)validate_instrument_def(x);class(x)<-c("psy_instrument","list");attr(x,"source_path")<-normalizePath(path);x}
#' Register an Instrument
#' @param instrument Instrument object or YAML/JSON path.
#' @param registry Registry object.
#' @param overwrite Replace an existing version.
#' @param confirm_license Confirm the caller may store and use this definition.
#' @return Invisibly, the stored path.
#' @export
register_instrument <- function(instrument, registry=psy_registry(), overwrite=FALSE, confirm_license=FALSE) {
  if(is.character(instrument)) instrument<-read_instrument(instrument)
  validate_instrument_def(instrument)
  redistributable<-isTRUE(instrument$license$redistributable)
  if(!redistributable&&!isTRUE(confirm_license)) psy_abort("The instrument is not marked redistributable. Set confirm_license=TRUE only after obtaining permission.","psy_error_instrument")
  dest<-file.path(registry$instruments,paste0(instrument$instrument_id,"__",instrument$version,".rds"));if(file.exists(dest)&&!overwrite)psy_abort("That instrument version is already registered.","psy_error_instrument")
  saveRDS(instrument,dest);invisible(dest)
}
#' List Registered Instruments
#' @param registry Registry object.
#' @param language Optional language filter.
#' @param redistributable Optional license filter.
#' @param active If `TRUE`, omit definitions with inactive status.
#' @return Instrument metadata data frame without item text.
#' @export
list_instruments <- function(registry=psy_registry(),language=NULL,redistributable=NULL,active=TRUE) {
  fs<-list.files(registry$instruments,pattern="\\.rds$",full.names=TRUE);if(!length(fs))return(data.frame(instrument_id=character(),version=character(),name=character(),language=character(),status=character(),redistributable=logical()))
  out<-do.call(rbind,lapply(fs,function(f){z<-readRDS(f);data.frame(instrument_id=z$instrument_id,version=z$version,name=z$name,language=z$language,status=z$status%||%"active",redistributable=isTRUE(z$license$redistributable),stringsAsFactors=FALSE)}))
  if(!is.null(language))out<-out[out$language%in%language,,drop=FALSE];if(!is.null(redistributable))out<-out[out$redistributable%in%redistributable,,drop=FALSE];if(active)out<-out[!out$status%in%c("inactive","retired"),,drop=FALSE];rownames(out)<-NULL;out
}
validate_reference_def <- function(x){req<-c("schema_version","reference_id","instrument_id","instrument_version","language");miss<-setdiff(req,names(x));if(length(miss))psy_abort(paste("Reference definition is missing:",paste(miss,collapse=", ")),"psy_error_reference");invisible(TRUE)}
#' Read a Score Interpretation Reference
#' @param path YAML or JSON path.
#' @param validate Validate required metadata.
#' @return A `psy_reference` list.
#' @export
read_reference<-function(path,validate=TRUE){x<-read_definition(path);if(validate)validate_reference_def(x);class(x)<-c("psy_reference","list");attr(x,"source_path")<-normalizePath(path);x}
#' Register a Score Reference
#' @param reference Reference object or path.
#' @param registry Registry object.
#' @param overwrite Replace an existing reference.
#' @return Invisibly, the stored path.
#' @export
register_reference<-function(reference,registry=psy_registry(),overwrite=FALSE){if(is.character(reference))reference<-read_reference(reference);validate_reference_def(reference);dest<-file.path(registry$references,paste0(reference$reference_id,".rds"));if(file.exists(dest)&&!overwrite)psy_abort("That reference is already registered.","psy_error_reference");saveRDS(reference,dest);invisible(dest)}
get_instrument <- function(instrument,registry){if(inherits(instrument,"psy_instrument"))return(instrument);if(is.character(instrument)&&length(instrument)==1L&&file.exists(instrument))return(read_instrument(instrument));if(is.character(instrument)&&length(instrument)==1L){fs<-list.files(registry$instruments,pattern=paste0("^",instrument,"(__.*)?\\.rds$"),full.names=TRUE);if(length(fs)!=1L)psy_abort(sprintf("Instrument '%s' was not uniquely found in the registry.",instrument),"psy_error_instrument");return(readRDS(fs))};psy_abort("instrument must be a definition, path, or registered ID.","psy_error_instrument")}
get_reference <- function(reference,registry){if(inherits(reference,"psy_reference"))return(reference);if(is.character(reference)&&file.exists(reference))return(read_reference(reference));if(is.character(reference)){f<-file.path(registry$references,paste0(reference,".rds"));if(file.exists(f))return(readRDS(f))};psy_abort("Reference was not found.","psy_error_reference")}
