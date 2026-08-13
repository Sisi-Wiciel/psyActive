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
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"))
  }
  if (length(x) == 0L) {
    return(as.POSIXct(character(), tz = "UTC"))
  }
  values <- if (inherits(x, "Date")) as.character(x) else as.character(x)
  explicit_offset <- !is.na(values) & grepl(
    "(?:Z|[+-][0-9]{2}:?[0-9]{2})$", values, perl = TRUE
  )

  if (is.null(timezone) || length(timezone) == 0L) {
    if (any(!is.na(values) & !explicit_offset)) {
      psy_abort(
        "A source timezone is required when a timestamp has no explicit offset.",
        "psy_error_schema"
      )
    }
    timezone <- rep("UTC", length(values))
  } else {
    if (length(timezone) == 1L) timezone <- rep(timezone, length(values))
    if (length(timezone) != length(values)) {
      psy_abort(
        "source_timezone must contain one value or one value per timestamp.",
        "psy_error_schema"
      )
    }
    timezone <- as.character(timezone)
    bad_timezone <- is.na(timezone) | !nzchar(timezone) |
      !(timezone %in% OlsonNames())
    if (any(bad_timezone)) {
      psy_abort(
        paste0(
          "source_timezone must contain valid IANA timezone names. Invalid value(s): ",
          paste(unique(timezone[bad_timezone]), collapse = ", ")
        ),
        "psy_error_schema"
      )
    }
  }

  if (length(values) != length(timezone)) {
    psy_abort(
      "Timestamps must contain one value or one value per source timezone.",
      "psy_error_schema"
    )
  }
  offset_values <- values
  offset_values[explicit_offset] <- sub(
    "([+-][0-9]{2}):([0-9]{2})$", "\\1\\2",
    sub("Z$", "+0000", offset_values[explicit_offset], perl = TRUE),
    perl = TRUE
  )

  parse_naive <- function(value, tz) {
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", value, perl = TRUE)) {
      format <- "%Y-%m-%d"
    } else if (grepl(
      "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?$",
      value, perl = TRUE
    )) {
      format <- "%Y-%m-%d %H:%M:%OS"
    } else if (grepl(
      "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?$",
      value, perl = TRUE
    )) {
      format <- "%Y-%m-%dT%H:%M:%OS"
    } else {
      return(NA_real_)
    }
    parsed <- tryCatch(
      as.POSIXct(strptime(value, format = format, tz = tz)),
      error = function(e) as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz)
    )
    as.numeric(parsed)
  }

  parse_offset <- function(value) {
    parts <- regmatches(
      value,
      regexec("^(.*)(Z|[+-][0-9]{2}:?[0-9]{2})$", value, perl = TRUE)
    )[[1L]]
    if (length(parts) != 3L) return(NA_real_)
    local <- parts[2L]
    token <- parts[3L]
    if (!grepl(
      "^\\d{4}-\\d{2}-\\d{2}(?: |T)\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?$",
      local, perl = TRUE
    )) {
      return(NA_real_)
    }
    local_format <- if (grepl("T", local, fixed = TRUE)) {
      "%Y-%m-%dT%H:%M:%OS"
    } else {
      "%Y-%m-%d %H:%M:%OS"
    }
    local_time <- tryCatch(
      as.POSIXct(strptime(local, format = local_format, tz = "UTC")),
      error = function(e) as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    )
    if (is.na(local_time)) return(NA_real_)
    if (identical(token, "Z")) return(as.numeric(local_time))
    token <- sub(":", "", token, fixed = TRUE)
    hours <- suppressWarnings(as.integer(substr(token, 2L, 3L)))
    minutes <- suppressWarnings(as.integer(substr(token, 4L, 5L)))
    if (is.na(hours) || is.na(minutes) || hours > 23L || minutes > 59L) {
      return(NA_real_)
    }
    sign <- if (substr(token, 1L, 1L) == "-") -1 else 1
    as.numeric(local_time) - sign * (hours * 3600 + minutes * 60)
  }

  parsed <- vapply(seq_along(values), function(i) {
    if (is.na(values[i])) return(NA_real_)
    if (explicit_offset[i]) {
      value <- parse_offset(offset_values[i])
    } else {
      value <- parse_naive(values[i], timezone[i])
    }
    value
  }, numeric(1))
  if (any(is.na(parsed) & !is.na(values))) {
    psy_abort("Some timestamps could not be parsed.", "psy_error_schema")
  }
  as.POSIXct(parsed, origin = "1970-01-01", tz = "UTC")
}

stable_hash <- function(x) digest::digest(x, algo = "sha256", serialize = TRUE)
stable_id <- function(prefix, ...) paste0(prefix, "_", substr(stable_hash(list(...)), 1L, 24L))
now_utc <- function() as.POSIXct(format(Sys.time(), tz = "UTC", usetz = TRUE), tz = "UTC")

new_psy_df <- function(x, class, problems = NULL, audit = NULL) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  class(x) <- c(class, "data.frame")
  if (is.null(problems)) problems <- new_quality()
  attr(x, "problems") <- problems
  attr(x, "audit") <- audit
  x
}
new_quality <- function(x = NULL) {
  if (is.null(x)) x <- data.frame(check_id=character(), severity=character(), row_id=character(),
                                  field=character(), message=character(), suggestion=character(),
                                  stringsAsFactors=FALSE)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  required <- c("check_id", "severity", "row_id", "field", "message", "suggestion")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    psy_abort(
      paste0("Quality data is missing required field(s): ", paste(missing, collapse = ", "), "."),
      "psy_error_schema"
    )
  }
  x <- x[required]
  class(x) <- c("psy_quality", "data.frame")
  x
}
make_quality <- function(check_id, severity, row_id = NA_character_, field = NA_character_, message, suggestion = NA_character_) {
  data.frame(check_id=as.character(check_id), severity=as.character(severity), row_id=as.character(row_id),
             field=as.character(field), message=as.character(message), suggestion=as.character(suggestion),
             stringsAsFactors=FALSE)
}
append_quality <- function(...) {
  flatten <- function(x) {
    if (is.null(x)) return(list())
    if (is.data.frame(x)) return(list(x))
    if (is.list(x)) return(unlist(lapply(x, flatten), recursive = FALSE))
    psy_abort(
      "Quality results must be data frames, lists of data frames, or NULL.",
      "psy_error_schema"
    )
  }
  xs <- unlist(lapply(list(...), flatten), recursive = FALSE)
  xs <- Filter(function(z) NROW(z) > 0L, xs)
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
