#' Quality Assessment Configuration
#' @param completion_min Minimum assessment completion fraction.
#' @param timing_min_seconds Optional minimum response duration.
#' @param timing_max_seconds Optional maximum response duration.
#' @return A configuration list.
#' @export
psy_quality_config <- function(completion_min = 0.8,
                               timing_min_seconds = NULL,
                               timing_max_seconds = NULL) {
  if (length(completion_min) != 1L || is.na(completion_min) ||
      completion_min < 0 || completion_min > 1) {
    psy_abort("completion_min must be a scalar between 0 and 1.", "psy_error_schema")
  }
  for (value in list(timing_min_seconds, timing_max_seconds)) {
    if (!is.null(value) &&
        (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0)) {
      psy_abort("Timing limits must be NULL or non-negative finite scalars.", "psy_error_schema")
    }
  }
  if (!is.null(timing_min_seconds) && !is.null(timing_max_seconds) &&
      timing_min_seconds > timing_max_seconds) {
    psy_abort("timing_min_seconds cannot exceed timing_max_seconds.", "psy_error_schema")
  }
  list(
    completion_min = as.numeric(completion_min),
    timing_min_seconds = timing_min_seconds,
    timing_max_seconds = timing_max_seconds
  )
}

quality_group_id <- function(x) {
  fields <- c("person_id", "assessment_id", "instrument_id", "instrument_version")
  if (!all(fields %in% names(x))) return(rep("<unavailable>", NROW(x)))
  vals <- lapply(x[fields], function(z) {
    z <- as.character(z)
    z[is.na(z) | !nzchar(z)] <- "<missing>"
    z
  })
  do.call(paste, c(vals, sep = "|"))
}

quality_groups <- function(x) {
  ids <- quality_group_id(x)
  split(x, ids, drop = TRUE)
}

quality_instrument_definitions <- function(registry) {
  if (is.null(registry) || !is.list(registry) || is.null(registry$instruments)) {
    return(list())
  }
  files <- list.files(registry$instruments, pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) return(list())
  definitions <- lapply(files, readRDS)
  keys <- vapply(definitions, function(z) {
    paste(z$instrument_id, z$version, sep = "@")
  }, character(1))
  stats::setNames(definitions[!duplicated(keys)], keys[!duplicated(keys)])
}

quality_item_bound <- function(item, side) {
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

quality_range <- function(x, registry) {
  if (is.null(registry)) {
    return(make_quality(
      "range", "info", field = "registry",
      message = "Range check was not run because no instrument registry was supplied.",
      suggestion = "Supply registry = psy_registry(...) to validate registered item values."
    ))
  }
  definitions <- quality_instrument_definitions(registry)
  if (!length(definitions)) {
    return(make_quality(
      "range", "warning", field = "registry",
      message = "Range check was not run because the instrument registry is empty.",
      suggestion = "Register each instrument version before assessing value ranges."
    ))
  }
  required <- c("instrument_id", "instrument_version", "item_id", "value_num")
  if (!all(required %in% names(x))) {
    return(make_quality(
      "range", "warning", field = "registry",
      message = "Range check was not run because observation fields are missing.",
      suggestion = "Run schema validation and map the required observation fields."
    ))
  }
  keys <- paste(x$instrument_id, x$instrument_version, sep = "@")
  findings <- list()
  k <- 0L
  for (i in seq_len(NROW(x))) {
    if (is.na(x$value_num[i])) next
    definition <- definitions[[keys[i]]]
    if (is.null(definition)) next
    item_index <- match(x$item_id[i], vapply(definition$items, function(z) z$item_id, character(1)))
    if (is.na(item_index)) next
    item <- definition$items[[item_index]]
    allowed <- suppressWarnings(as.numeric(unlist(item$allowed_values)))
    lower <- quality_item_bound(item, "lower")
    upper <- quality_item_bound(item, "upper")
    invalid <- length(allowed) && !x$value_num[i] %in% allowed
    if (!is.null(lower)) invalid <- invalid || x$value_num[i] < as.numeric(lower)
    if (!is.null(upper)) invalid <- invalid || x$value_num[i] > as.numeric(upper)
    if (isTRUE(invalid)) {
      k <- k + 1L
      findings[[k]] <- make_quality(
        "range", "error", row_id = as.character(i), field = "value_num",
        message = sprintf("Value for item '%s' is outside its registered range.", x$item_id[i]),
        suggestion = "Correct the value or instrument/version mapping."
      )
    }
  }
  if (!length(findings)) return(NULL)
  do.call(rbind, findings)
}

quality_timing <- function(x, config) {
  candidates <- c("duration_seconds", "response_time_seconds", "elapsed_seconds", "duration")
  field <- candidates[candidates %in% names(x)][1L]
  if (is.na(field) || is.null(field)) {
    return(make_quality(
      "timing", "info", field = "timing",
      message = "Timing check was not run because no duration field is present.",
      suggestion = "Provide one of duration_seconds, response_time_seconds, elapsed_seconds, or duration."
    ))
  }
  if (is.null(config$timing_min_seconds) && is.null(config$timing_max_seconds)) {
    return(make_quality(
      "timing", "info", field = field,
      message = "Timing check was not run because no timing thresholds are configured.",
      suggestion = "Set timing_min_seconds and/or timing_max_seconds in psy_quality_config()."
    ))
  }
  values <- suppressWarnings(as.numeric(x[[field]]))
  bad_type <- !is.na(x[[field]]) & (is.na(values) | !is.finite(values))
  findings <- list()
  k <- 0L
  if (any(bad_type)) {
    k <- k + 1L
    findings[[k]] <- make_quality(
      "timing", "error", row_id = paste(which(bad_type), collapse = ","), field = field,
      message = "Timing values must be finite numeric durations in seconds.",
      suggestion = "Convert the duration field to seconds before quality assessment."
    )
  }
  too_short <- !is.na(values) & !is.null(config$timing_min_seconds) &
    values < config$timing_min_seconds
  too_long <- !is.na(values) & !is.null(config$timing_max_seconds) &
    values > config$timing_max_seconds
  if (any(too_short)) {
    k <- k + 1L
    findings[[k]] <- make_quality(
      "timing", "warning", row_id = paste(which(too_short), collapse = ","), field = field,
      message = "Some responses are shorter than the configured minimum duration.",
      suggestion = "Review response capture and possible careless responding."
    )
  }
  if (any(too_long)) {
    k <- k + 1L
    findings[[k]] <- make_quality(
      "timing", "warning", row_id = paste(which(too_long), collapse = ","), field = field,
      message = "Some responses exceed the configured maximum duration.",
      suggestion = "Review interrupted sessions and response timestamps."
    )
  }
  if (!length(findings)) return(NULL)
  do.call(rbind, findings)
}

#' Assess Psychiatric Data Quality
#' @param x Observations.
#' @param registry Optional registry.
#' @param checks Checks to run.
#' @param config Quality configuration.
#' @return A `psy_quality` object.
#' @export
assess_quality <- function(
    x,
    registry = NULL,
    checks = c("schema", "range", "duplicates", "time_order", "completion",
               "straightlining", "timing"),
    config = psy_quality_config()) {
  if (!is.data.frame(x)) psy_abort("x must be a data frame.", "psy_error_schema")
  supported <- c("schema", "range", "duplicates", "time_order", "completion",
                 "straightlining", "timing")
  unknown <- setdiff(checks, supported)
  if (length(unknown)) {
    psy_abort(
      paste0("Unknown quality check(s): ", paste(unknown, collapse = ", "), "."),
      "psy_error_schema"
    )
  }
  if (!is.list(config)) psy_abort("config must be created by psy_quality_config().", "psy_error_schema")

  findings <- list()
  add <- function(value) {
    if (!is.null(value) && NROW(value)) findings[[length(findings) + 1L]] <<- value
  }

  if ("schema" %in% checks) {
    add(validate_observation(x, level = "schema"))
  }
  if ("range" %in% checks) {
    if (is.null(registry)) {
      add(quality_range(x, registry))
    } else {
      registered <- validate_observation(x, registry = registry, level = "registered")
      registered_ids <- c(
        "registry_required", "unregistered_instrument", "unregistered_item", "range"
      )
      add(registered[registered$check_id %in% registered_ids, , drop = FALSE])
      if (!length(quality_instrument_definitions(registry)) ||
          !all(c("instrument_id", "instrument_version", "item_id", "value_num") %in% names(x))) {
        add(quality_range(x, registry))
      }
    }
  }

  duplicate_fields <- c(
    "person_id", "record_id", "assessment_id", "instrument_id",
    "instrument_version", "item_id", "source_system", "observed_at"
  )
  if ("duplicates" %in% checks && all(duplicate_fields %in% names(x))) {
    key <- observation_identity_key(x)
    duplicate <- duplicated(key) | duplicated(key, fromLast = TRUE)
    if (any(duplicate)) add(make_quality(
      "duplicates", "error", row_id = paste(which(duplicate), collapse = ","),
      message = "Duplicate observation identity keys, including the exact UTC observation time, detected.",
      suggestion = "Resolve exact duplicate observations before scoring."
    ))
  } else if ("duplicates" %in% checks) {
    add(make_quality("duplicates", "info", field = "duplicates",
                     message = "Duplicate check was not run because its complete identity fields are missing.",
                     suggestion = paste("Run schema validation and map:", paste(duplicate_fields, collapse = ", "), ".")))
  }

  if ("time_order" %in% checks && all(c("person_id", "observed_at") %in% names(x))) {
    order_index <- order(x$person_id, x$observed_at, na.last = TRUE)
    if (!identical(order_index, seq_len(NROW(x)))) add(make_quality(
      "time_order", "info",
      message = "Rows are not ordered by person and observation time.",
      suggestion = "Sort before longitudinal analysis."
    ))
  } else if ("time_order" %in% checks) {
    add(make_quality("time_order", "info", field = "observed_at",
                     message = "Time-order check was not run because person_id or observed_at is missing.",
                     suggestion = "Run schema validation and map both fields."))
  }

  groups <- quality_groups(x)
  if ("completion" %in% checks) {
    if (!all(c("item_id", "value_num", "value_raw") %in% names(x))) {
      add(make_quality("completion", "info", field = "completion",
                       message = "Completion check was not run because item/value fields are missing.",
                       suggestion = "Run schema validation and map item_id plus value_raw or value_num."))
    } else if (is.null(registry)) {
      add(make_quality(
        "completion", "info", field = "registry",
        message = "Completion check was not run because expected items require an instrument registry.",
        suggestion = "Supply registry = psy_registry(...) to compare observed and expected items."
      ))
    } else {
      definitions <- quality_instrument_definitions(registry)
      if (!length(definitions)) {
        add(make_quality(
          "completion", "warning", field = "registry",
          message = "Completion check was not run because the instrument registry is empty.",
          suggestion = "Register each instrument version before assessing completion."
        ))
      } else {
        for (group_id in names(groups)) {
          z <- groups[[group_id]]
          key <- paste(z$instrument_id[1L], z$instrument_version[1L], sep = "@")
          definition <- definitions[[key]]
          if (is.null(definition) || !length(definition$items)) {
            add(make_quality(
              "completion", "warning", row_id = group_id, field = "registry",
              message = sprintf("Completion check was not run for unregistered instrument version '%s'.", key),
              suggestion = "Register the instrument version before assessing completion."
            ))
            next
          }
          expected_ids <- unique(vapply(definition$items, function(item) item$item_id, character(1)))
          answered_rows <- !is.na(z$value_num) | !is.na(z$value_raw)
          answered_ids <- unique(z$item_id[answered_rows & z$item_id %in% expected_ids])
          answered <- length(answered_ids)
          expected <- length(expected_ids)
          rate <- answered / expected
          if (rate < config$completion_min) add(make_quality(
            "completion", "warning", row_id = group_id,
            message = sprintf("Assessment completion is %.1f%% (%d/%d items).", 100 * rate, answered, expected),
            suggestion = "Review missingness before interpretation."
          ))
        }
      }
    }
  }
  if ("straightlining" %in% checks) {
    if (!"value_num" %in% names(x)) {
      add(make_quality("straightlining", "info", field = "value_num",
                       message = "Straightlining check was not run because value_num is missing.",
                       suggestion = "Map numeric item values before assessing straightlining."))
    } else {
      for (group_id in names(groups)) {
        values <- groups[[group_id]]$value_num
        values <- values[is.finite(values)]
        if (length(values) >= 4L && length(unique(values)) == 1L) add(make_quality(
          "straightlining", "info", row_id = group_id,
          message = "All available numeric item values are identical.",
          suggestion = "Treat as a data-quality signal, not a clinical event."
        ))
      }
    }
  }
  if ("timing" %in% checks) add(quality_timing(x, config))

  append_quality(findings)
}

#' Summarize Quality Findings
#' @param x A quality object.
#' @param by Columns to group by.
#' @return Summary data frame.
#' @export
summarize_quality <- function(x, by = c("check_id", "severity")) {
  if (!NROW(x)) return(data.frame(check_id = character(), severity = character(), n = integer()))
  if (!all(by %in% names(x))) psy_abort("Unknown quality summary column.", "psy_error_schema")
  key <- interaction(x[by], drop = TRUE, lex.order = TRUE)
  out <- do.call(rbind, lapply(split(x, key), function(z) cbind(z[1, by, drop = FALSE], n = NROW(z))))
  rownames(out) <- NULL
  out
}

#' Plot Quality Findings
#' @param x Quality object.
#' @param type Plot type.
#' @return A ggplot object.
#' @export
plot_quality <- function(x, type = c("overview", "missingness", "compliance")) {
  type <- match.arg(type)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    psy_abort("Package 'ggplot2' is required for plotting.", "psy_error_dependency")
  }
  d <- summarize_quality(x)
  check_id <- n <- severity <- NULL
  ggplot2::ggplot(d, ggplot2::aes(x = check_id, y = n, fill = severity)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Check", y = "Findings", title = "psyActive data quality") +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip()
}
