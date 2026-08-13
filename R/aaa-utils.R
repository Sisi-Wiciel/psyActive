.psy_schema_version <- "1.0"
.psy_missing_reasons <- c("not_asked", "skipped", "refused", "not_applicable",
                          "device_off", "technical_failure", "out_of_window", "unknown")
.psy_alert_levels <- c("none", "watch", "review", "urgent_review")
.psy_event_status <- c("open", "acknowledged", "reviewed", "escalated", "closed")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

psy_abort <- function(message, class = "psy_error", ...) {
  cond <- structure(c(list(message = message, call = NULL), list(...)),
                    class = c(class, "psy_error", "error", "condition"))
  stop(cond)
}

psy_warn <- function(message, class = "psy_warning", ...) {
  cond <- structure(c(list(message = message, call = NULL), list(...)),
                    class = c(class, "psy_warning", "warning", "condition"))
  warning(cond)
}

as_utc <- function(x, timezone = NULL) {
  if (inherits(x, "POSIXct")) return(as.POSIXct(format(x, tz = "UTC", usetz = TRUE), tz = "UTC"))
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = timezone %||% "UTC"))
  if (is.null(timezone) || !nzchar(timezone)) {
    psy_abort("A source timezone is required when observed_at is not POSIXct.", "psy_error_schema")
  }
  out <- as.POSIXct(x, tz = timezone, tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d"))
  if (anyNA(out) && any(!is.na(x))) psy_abort("Some timestamps could not be parsed.", "psy_error_schema")
  as.POSIXct(format(out, tz = "UTC", usetz = TRUE), tz = "UTC")
}

stable_hash <- function(x) digest::digest(x, algo = "sha256", serialize = TRUE)
stable_id <- function(prefix, ...) paste0(prefix, "_", substr(stable_hash(list(...)), 1L, 24L))
now_utc <- function() as.POSIXct(format(Sys.time(), tz = "UTC", usetz = TRUE), tz = "UTC")

new_psy_df <- function(x, class, problems = NULL, audit = NULL) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  class(x) <- c(class, "data.frame")
  attr(x, "problems") <- problems %||% new_quality()
  attr(x, "audit") <- audit
  x
}
new_quality <- function(x = NULL) {
  if (is.null(x)) x <- data.frame(check_id=character(), severity=character(), row_id=character(),
                                  field=character(), message=character(), suggestion=character(),
                                  stringsAsFactors=FALSE)
  new_psy_df(x, "psy_quality", problems = data.frame())
}
make_quality <- function(check_id, severity, row_id = NA_character_, field = NA_character_, message, suggestion = NA_character_) {
  data.frame(check_id=as.character(check_id), severity=as.character(severity), row_id=as.character(row_id),
             field=as.character(field), message=as.character(message), suggestion=as.character(suggestion),
             stringsAsFactors=FALSE)
}
append_quality <- function(...) {
  xs <- Filter(function(z) !is.null(z) && NROW(z)>0L, list(...))
  if (!length(xs)) return(new_quality())
  new_quality(do.call(rbind, lapply(xs, as.data.frame)))
}
new_audit <- function(operation, input, output = NULL, config = NULL, warnings_count = 0L) {
  data.frame(operation=operation, package_version=as.character(utils::packageVersion("psyActive")),
             schema_version=.psy_schema_version, input_hash=stable_hash(input),
             config_hash=stable_hash(config), output_hash=if (is.null(output)) NA_character_ else stable_hash(output),
             timestamp_utc=now_utc(), warnings_count=as.integer(warnings_count), stringsAsFactors=FALSE)
}

print_psy <- function(x, label) {
  cat(sprintf("<%s> %d row%s, %d column%s\n", label, NROW(x), if (NROW(x)==1) "" else "s", NCOL(x), if (NCOL(x)==1) "" else "s"))
  print.data.frame(utils::head(as.data.frame(x), 10L), row.names=FALSE)
  invisible(x)
}
print.psy_observation <- function(x, ...) print_psy(x, "psy_observation")
print.psy_score <- function(x, ...) print_psy(x, "psy_score")
print.psy_quality <- function(x, ...) print_psy(x, "psy_quality")
print.psy_baseline <- function(x, ...) print_psy(x, "psy_baseline")
print.psy_event <- function(x, ...) print_psy(x, "psy_event")
print.psy_review <- function(x, ...) print_psy(x, "psy_review")
print.psy_instrument <- function(x, ...) {cat(sprintf("<psy_instrument> %s version %s (%s)\n", x$instrument_id, x$version, x$status %||% "unspecified")); invisible(x)}
print.psy_reference <- function(x, ...) {cat(sprintf("<psy_reference> %s for %s %s\n", x$reference_id, x$instrument_id, x$instrument_version)); invisible(x)}
print.psy_ruleset <- function(x, ...) {cat(sprintf("<psy_ruleset> %s version %s (%d rules)\n", x$ruleset_id, x$version, length(x$rules))); invisible(x)}

problems <- function(x) UseMethod("problems")
problems.default <- function(x) attr(x, "problems") %||% new_quality()
problems.psy_observation <- function(x) attr(x, "problems") %||% new_quality()
problems.psy_score <- function(x) attr(x, "problems") %||% new_quality()
