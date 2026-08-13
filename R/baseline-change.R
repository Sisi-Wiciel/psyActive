#' Change Detection Configuration
#' @param threshold Standardized shift threshold.
#' @param persistence Consecutive observations required.
#' @param confidence Reliable-change confidence level.
#' @return A configuration list.
#' @export
psy_change_config <- function(threshold = 2.5, persistence = 2L,
                              confidence = 0.95) {
  threshold <- suppressWarnings(as.numeric(threshold))
  persistence_input <- suppressWarnings(as.numeric(persistence))
  persistence <- suppressWarnings(as.integer(persistence_input))
  confidence <- suppressWarnings(as.numeric(confidence))
  if (length(threshold) != 1L || is.na(threshold) || !is.finite(threshold) || threshold <= 0) {
    psy_abort("threshold must be one positive finite number.", "psy_error_schema")
  }
  if (length(persistence) != 1L || is.na(persistence) ||
      !is.finite(persistence_input) || persistence_input != persistence ||
      persistence < 1L) {
    psy_abort("persistence must be one positive integer.", "psy_error_schema")
  }
  if (length(confidence) != 1L || !is.finite(confidence) ||
      confidence <= 0 || confidence >= 1) {
    psy_abort("confidence must be one number strictly between zero and one.",
              "psy_error_schema")
  }
  list(threshold = threshold, persistence = persistence, confidence = confidence)
}

metric_columns <- function(x) {
  if (inherits(x, "psy_score")) {
    required <- c("instrument_id", "instrument_version", "score_value",
                  "score_name", "observed_at")
    missing <- setdiff(required, names(x))
    if (length(missing)) {
      psy_abort(paste("psy_score is missing:", paste(missing, collapse = ", ")),
                "psy_error_schema")
    }
    return(list(value = "score_value", metric = "score_name", time = "observed_at"))
  }
  required <- c("metric_id", "value", "observed_at")
  if (!all(required %in% names(x))) {
    psy_abort(
      "Input must be psy_score or contain metric_id, value, and observed_at.",
      "psy_error_schema"
    )
  }
  list(value = "value", metric = "metric_id", time = "observed_at")
}

empty_baseline <- function() {
  new_psy_df(data.frame(
    baseline_id = character(), person_id = character(), metric_id = character(),
    window_start = as.POSIXct(character(), tz = "UTC"),
    window_end = as.POSIXct(character(), tz = "UTC"), method = character(),
    center = double(), scale = double(), n_obs = integer(), status = character(),
    stringsAsFactors = FALSE
  ), "psy_baseline")
}

time_vector <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }
  suppressWarnings(as.POSIXct(x, tz = "UTC"))
}

validate_window <- function(window) {
  if (is.null(window)) return(NULL)
  if (length(window) != 2L) {
    psy_abort("window must contain start and end times.", "psy_error_schema")
  }
  w <- time_vector(window)
  if (length(w) != 2L || any(is.na(w)) || any(!is.finite(as.numeric(w))) ||
      w[1L] > w[2L]) {
    psy_abort("window must contain two finite, ordered times.", "psy_error_schema")
  }
  w
}

#' Learn an Immutable Personal Baseline
#' @param x Score data or metric data.
#' @param metrics Optional metric subset.
#' @param window Optional length-two UTC time window.
#' @param method Baseline estimator.
#' @param min_n Minimum number of observations.
#' @param zero_scale Zero-variance policy.
#' @param audit Attach audit information.
#' @return A `psy_baseline` object.
#' @export
learn_baseline <- function(x, metrics = NULL, window = NULL,
                           method = c("median_mad", "mean_sd"),
                           min_n = 4L,
                           zero_scale = c("flag", "fallback_iqr", "error"),
                           audit = TRUE) {
  method <- match.arg(method)
  zero_scale <- match.arg(zero_scale)
  if (!is.data.frame(x)) {
    psy_abort("x must be a data frame.", "psy_error_schema")
  }
  min_n_input <- suppressWarnings(as.numeric(min_n))
  min_n <- suppressWarnings(as.integer(min_n_input))
  if (length(min_n) != 1L || is.na(min_n) || !is.finite(min_n_input) ||
      min_n_input != min_n || min_n < 1L) {
    psy_abort("min_n must be one positive integer.", "psy_error_schema")
  }
  mc <- metric_columns(x)
  if (!"person_id" %in% names(x)) {
    psy_abort("Input must contain person_id.", "psy_error_schema")
  }
  d <- as.data.frame(x, stringsAsFactors = FALSE)
  d$metric_id <- if (inherits(x, "psy_score")) {
    paste(as.character(d$instrument_id), as.character(d$score_name), sep = ":")
  } else as.character(d[[mc$metric]])
  d$value <- suppressWarnings(as.numeric(d[[mc$value]]))
  d$time <- time_vector(d[[mc$time]])
  if (any(is.na(d$person_id) | !nzchar(as.character(d$person_id))) ||
      any(is.na(d$metric_id) | !nzchar(d$metric_id))) {
    psy_abort("person_id and metric identifiers must be non-empty.",
              "psy_error_schema")
  }
  w <- validate_window(window)
  finite_time <- !is.na(d$time) & is.finite(as.numeric(d$time))
  d <- d[finite_time, , drop = FALSE]
  if (!is.null(w)) d <- d[d$time >= w[1L] & d$time <= w[2L], , drop = FALSE]
  if (!is.null(metrics)) {
    metrics <- as.character(metrics)
    d <- d[d$metric_id %in% metrics, , drop = FALSE]
  }

  if (!NROW(d)) {
    out <- empty_baseline()
    if (audit) {
      attr(out, "audit") <- new_audit(
        "learn_baseline", x, out,
        list(method = method, min_n = min_n, window = window,
             metrics = metrics)
      )
    }
    return(out)
  }
  group_key <- interaction(as.character(d$person_id), d$metric_id,
                           drop = TRUE, lex.order = TRUE, sep = "\u001f")
  groups <- split(d, group_key)
  rows <- list()
  for (z in groups) {
    z <- z[order(z$time), , drop = FALSE]
    v <- z$value[is.finite(z$value)]
    n <- length(v)
    status <- "ok"
    center <- NA_real_
    scale <- NA_real_
    if (n < min_n) {
      status <- "insufficient"
    } else if (method == "median_mad") {
      center <- stats::median(v)
      scale <- stats::mad(v, center = center, constant = 1.4826)
    } else {
      center <- mean(v)
      scale <- stats::sd(v)
    }
    if (n >= min_n && (!is.finite(scale) || scale <= 0)) {
      if (zero_scale == "error") {
        psy_abort("A baseline has zero variance.", "psy_error_insufficient_data")
      }
      if (zero_scale == "fallback_iqr") {
        scale <- stats::IQR(v) / 1.349
      }
    if (is.na(scale) || !is.finite(scale) || scale <= 0) {
        scale <- NA_real_
        status <- "zero_variance"
      }
    }
    tmin <- min(z$time, na.rm = TRUE)
    tmax <- max(z$time, na.rm = TRUE)
    rows[[length(rows) + 1L]] <- data.frame(
      baseline_id = stable_id("baseline", z$person_id[1L], z$metric_id[1L],
                              tmin, tmax, method),
      person_id = as.character(z$person_id[1L]),
      metric_id = as.character(z$metric_id[1L]),
      window_start = tmin, window_end = tmax, method = method,
      center = center, scale = scale, n_obs = as.integer(n), status = status,
      stringsAsFactors = FALSE
    )
  }
  out <- new_psy_df(do.call(rbind, rows), "psy_baseline")
  if (audit) {
    attr(out, "audit") <- new_audit(
      "learn_baseline", x, out,
      list(method = method, min_n = min_n, window = window, metrics = metrics)
    )
  }
  out
}

reliable_empty <- function(x, status = "not_available") {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  out$change_value <- rep(NA_real_, NROW(out))
  out$rci <- rep(NA_real_, NROW(out))
  out$reliable_change <- rep(status, NROW(out))
  out
}

reference_parameters <- function(reference) {
  ref <- if (inherits(reference, "psy_reference")) reference else read_reference(reference)
  rel <- suppressWarnings(as.numeric(ref$reliability$estimate %||% NA_real_))[1L]
  sdref <- suppressWarnings(as.numeric(ref$reference_sd %||% NA_real_))[1L]
  list(reference = ref, reliability = rel, reference_sd = sdref)
}

change_groups <- function(d, from) {
  required <- c("person_id", "observed_at")
  missing <- setdiff(required, names(d))
  if (length(missing)) {
    psy_abort(paste("Input is missing:", paste(missing, collapse = ", ")),
              "psy_error_schema")
  }
  if (inherits(d, "psy_score") || all(c("score_name", "score_value") %in% names(d))) {
    metric <- c("instrument_id", "instrument_version", "score_name")
    missing <- setdiff(metric, names(d))
    if (length(missing)) {
      psy_abort(paste("Score input is missing:", paste(missing, collapse = ", ")),
                "psy_error_schema")
    }
    d$.metric_key <- paste(d$instrument_id, d$instrument_version,
                           d$score_name, sep = "\u001f")
    d$.value <- suppressWarnings(as.numeric(d$score_value))
  } else {
    if (!all(c("metric_id", "value") %in% names(d))) {
      psy_abort("Input must contain score_name/score_value or metric_id/value.",
                "psy_error_schema")
    }
    d$.metric_key <- as.character(d$metric_id)
    d$.value <- suppressWarnings(as.numeric(d$value))
  }
  if (from == "episode_start") {
    if (!"episode_id" %in% names(d)) {
      psy_abort("from='episode_start' requires an episode_id column.",
                "psy_error_schema")
    }
    if (any(is.na(d$episode_id) | !nzchar(as.character(d$episode_id)))) {
      psy_abort("episode_id must be non-missing for from='episode_start'.",
                "psy_error_schema")
    }
    key <- interaction(as.character(d$person_id), d$.metric_key,
                       as.character(d$episode_id), drop = TRUE,
                       lex.order = TRUE, sep = "\u001f")
  } else {
    key <- interaction(as.character(d$person_id), d$.metric_key,
                       drop = TRUE, lex.order = TRUE, sep = "\u001f")
  }
  split(d, key)
}

#' Calculate Reliable Change
#' @param x Score data ordered over time.
#' @param reference Reference object or path.
#' @param from Comparison origin.
#' @param confidence Confidence level.
#' @param direction Score direction.
#' @return Data frame containing change, RCI, and status.
#' @export
reliable_change <- function(x, reference,
                            from = c("baseline", "previous", "episode_start"),
                            confidence = 0.95,
                            direction = c("higher_worse", "higher_better")) {
  from <- match.arg(from)
  direction <- match.arg(direction)
  if (!is.data.frame(x)) psy_abort("x must be a data frame.", "psy_error_schema")
  confidence <- suppressWarnings(as.numeric(confidence))
  if (length(confidence) != 1L || !is.finite(confidence) ||
      confidence <= 0 || confidence >= 1) {
    psy_abort("confidence must be one number strictly between zero and one.",
              "psy_error_schema")
  }
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  required <- c("person_id", "observed_at")
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    psy_abort(paste("Input is missing:", paste(missing, collapse = ", ")),
              "psy_error_schema")
  }
  if (from == "episode_start" && !"episode_id" %in% names(out)) {
    psy_abort("from='episode_start' requires an episode_id column.",
              "psy_error_schema")
  }
  d <- out
  d$observed_at <- time_vector(d$observed_at)
  if (any(is.na(d$observed_at) | !is.finite(as.numeric(d$observed_at)))) {
    psy_abort("observed_at must contain finite times for reliable change.",
              "psy_error_schema")
  }
  if (any(is.na(d$person_id) | !nzchar(as.character(d$person_id)))) {
    psy_abort("person_id must be non-missing for reliable change.",
              "psy_error_schema")
  }
  groups <- change_groups(d, from)
  pars <- reference_parameters(reference)
  ref <- pars$reference
  rel <- pars$reliability
  sdref <- pars$reference_sd
  if (is.na(rel) || !is.finite(rel) || rel < 0 || rel >= 1 ||
      is.na(sdref) || !is.finite(sdref) || sdref <= 0) {
    result <- reliable_empty(out, status = "insufficient_reference")
    attr(result, "reference") <- ref
    return(result)
  }
  se <- sdref * sqrt(2 * (1 - rel))
  crit <- stats::qnorm(1 - (1 - confidence) / 2)
  result <- lapply(groups, function(z) {
    z <- z[order(z$observed_at, na.last = TRUE), , drop = FALSE]
    origin <- if (from == "previous") {
      c(NA_real_, z$.value[seq_len(max(0L, NROW(z) - 1L))])
    } else {
      rep(z$.value[1L], NROW(z))
    }
    z$change_value <- z$.value - origin
    z$rci <- z$change_value / se
    z$reliable_change <- ifelse(
      !is.finite(z$rci), "not_available",
      ifelse(abs(z$rci) < crit, "no_reliable_change",
             ifelse((direction == "higher_worse" & z$rci > 0) |
                      (direction == "higher_better" & z$rci < 0),
                    "reliable_deterioration", "reliable_improvement"))
    )
    z$.metric_key <- NULL
    z$.value <- NULL
    z
  })
  if (!length(result)) return(reliable_empty(out))
  do.call(rbind, result)
}

empty_events <- function() {
  new_psy_df(data.frame(
    event_id = character(), person_id = character(),
    event_time = as.POSIXct(character(), tz = "UTC"),
    detected_at = as.POSIXct(character(), tz = "UTC"), event_type = character(),
    metric_id = character(), observed_value = double(), expected_value = double(),
    change_value = double(),
    alert_level = factor(character(), levels = .psy_alert_levels, ordered = TRUE),
    reason_code = character(), rule_id = character(), rule_version = character(),
    event_status = character(), stringsAsFactors = FALSE
  ), "psy_event")
}

#' Detect Persistent Standardized Shifts
#' @param x Score data.
#' @param baseline Personal baseline object.
#' @param threshold Standardized threshold.
#' @param persistence Consecutive observations required.
#' @param direction Direction to detect.
#' @param cooldown Minimum number of subsequent rows before another event.
#' @param audit Attach audit record.
#' @return A `psy_event` object.
#' @export
detect_shift <- function(x, baseline, threshold = 2.5, persistence = 2L,
                         direction = c("both", "increase", "decrease"),
                         cooldown = 0, audit = TRUE) {
  direction <- match.arg(direction)
  if (!is.data.frame(x) || !is.data.frame(baseline)) {
    psy_abort("x and baseline must be data frames.", "psy_error_schema")
  }
  threshold <- suppressWarnings(as.numeric(threshold))
  persistence_input <- suppressWarnings(as.numeric(persistence))
  cooldown_input <- suppressWarnings(as.numeric(cooldown))
  persistence <- suppressWarnings(as.integer(persistence_input))
  cooldown <- suppressWarnings(as.integer(cooldown_input))
  if (length(threshold) != 1L || is.na(threshold) || !is.finite(threshold) || threshold <= 0) {
    psy_abort("threshold must be one positive finite number.", "psy_error_schema")
  }
  if (length(persistence) != 1L || is.na(persistence) ||
      !is.finite(persistence_input) || persistence_input != persistence ||
      persistence < 1L) {
    psy_abort("persistence must be one positive integer.", "psy_error_schema")
  }
  if (length(cooldown) != 1L || is.na(cooldown) ||
      !is.finite(cooldown_input) || cooldown_input != cooldown || cooldown < 0L) {
    psy_abort("cooldown must be one non-negative integer.", "psy_error_schema")
  }
  mc <- metric_columns(x)
  required_x <- c("person_id", mc$time)
  if (!all(required_x %in% names(x))) {
    psy_abort("Input is missing required longitudinal fields.", "psy_error_schema")
  }
  required_b <- c("person_id", "metric_id", "center", "scale")
  missing_b <- setdiff(required_b, names(baseline))
  if (length(missing_b)) {
    psy_abort(paste("baseline is missing:", paste(missing_b, collapse = ", ")),
              "psy_error_schema")
  }
  if (anyDuplicated(baseline[c("person_id", "metric_id")])) {
    psy_abort("baseline must contain one row per person and metric.",
              "psy_error_schema")
  }
  bad_baseline_key <- vapply(baseline[c("person_id", "metric_id")],
                             function(value) {
    value <- as.character(value)
    any(is.na(value) | !nzchar(value))
  }, logical(1))
  if (any(bad_baseline_key)) {
    psy_abort("baseline person_id and metric_id values must be non-empty.",
              "psy_error_schema")
  }
  d <- as.data.frame(x, stringsAsFactors = FALSE)
  d$.metric_id <- if (inherits(x, "psy_score")) {
    paste(as.character(d$instrument_id), as.character(d$score_name), sep = ":")
  } else as.character(d$metric_id)
  d$.value <- suppressWarnings(as.numeric(d[[mc$value]]))
  d$.time <- time_vector(d[[mc$time]])
  if (any(is.na(d$person_id) | !nzchar(as.character(d$person_id))) ||
      any(is.na(d$.metric_id) | !nzchar(d$.metric_id))) {
    psy_abort("person_id and metric identifiers must be non-empty.",
              "psy_error_schema")
  }
  finite_time <- !is.na(d$.time) & is.finite(as.numeric(d$.time))
  d <- d[finite_time, , drop = FALSE]
  if (!NROW(d)) {
    out <- empty_events()
    if (audit) attr(out, "audit") <- new_audit(
      "detect_shift", x, out,
      list(threshold = threshold, persistence = persistence,
           direction = direction, cooldown = cooldown)
    )
    return(out)
  }
  base_key <- paste(as.character(baseline$person_id),
                    as.character(baseline$metric_id), sep = "\u001f")
  data_key <- paste(as.character(d$person_id), d$.metric_id, sep = "\u001f")
  hit_base <- match(data_key, base_key)
  d$.center <- suppressWarnings(as.numeric(baseline$center[hit_base]))
  d$.scale <- suppressWarnings(as.numeric(baseline$scale[hit_base]))
  if ("status" %in% names(baseline)) {
    baseline_status <- as.character(baseline$status[hit_base])
    baseline_ok <- !is.na(hit_base) & !is.na(baseline_status) &
      baseline_status == "ok"
    d$.scale[!baseline_ok] <- NA_real_
  }
  d$.scale[!is.finite(d$.scale) | d$.scale <= 0] <- NA_real_
  d$.z <- (d$.value - d$.center) / d$.scale
  d <- d[order(as.character(d$person_id), d$.metric_id, d$.time,
               seq_len(NROW(d))), , drop = FALSE]
  groups <- split(d, interaction(as.character(d$person_id), d$.metric_id,
                                 drop = TRUE, lex.order = TRUE, sep = "\u001f"))
  rows <- list()
  for (z in groups) {
    hit <- is.finite(z$.z) & switch(
      direction,
      both = abs(z$.z) >= threshold,
      increase = z$.z >= threshold,
      decrease = z$.z <= -threshold
    )
    if (!length(hit) || !any(hit)) next
    run <- rle(hit)
    ends <- cumsum(run$lengths)
    starts <- ends - run$lengths + 1L
    candidates <- starts[run$values & run$lengths >= persistence] + persistence - 1L
    if (!length(candidates)) next
    accepted <- integer()
    for (candidate in candidates) {
      if (!length(accepted) || candidate > accepted[length(accepted)] + cooldown) {
        accepted <- c(accepted, candidate)
      }
    }
    for (ii in accepted) {
      rows[[length(rows) + 1L]] <- data.frame(
        event_id = stable_id("event", z$person_id[ii], z$.metric_id[ii],
                             z$.time[ii], "sustained_shift"),
        person_id = as.character(z$person_id[ii]), event_time = z$.time[ii],
        detected_at = now_utc(), event_type = "sustained_shift",
        metric_id = as.character(z$.metric_id[ii]),
        observed_value = z$.value[ii], expected_value = z$.center[ii],
        change_value = z$.value[ii] - z$.center[ii],
        alert_level = factor("review", levels = .psy_alert_levels, ordered = TRUE),
        reason_code = "STANDARDIZED_PERSISTENT_SHIFT",
        rule_id = NA_character_, rule_version = NA_character_,
        event_status = "open", stringsAsFactors = FALSE
      )
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else empty_events()
  if (!inherits(out, "psy_event")) out <- new_psy_df(out, "psy_event")
  if (audit) {
    attr(out, "audit") <- new_audit(
      "detect_shift", x, out,
      list(threshold = threshold, persistence = persistence,
           direction = direction, cooldown = cooldown)
    )
  }
  out
}

#' Detect Configured Longitudinal Changes
#' @param x Score data.
#' @param baseline Baseline object.
#' @param methods Methods to run. Currently `shift` is supported.
#' @param config Change configuration.
#' @return A `psy_event` object.
#' @export
detect_change <- function(x, baseline = NULL,
                          methods = "shift",
                          config = psy_change_config()) {
  supported <- c("reliable_change", "shift")
  methods <- unique(as.character(methods))
  unknown <- setdiff(methods, supported)
  if (length(unknown)) {
    psy_abort(paste("Unsupported change method(s):", paste(unknown, collapse = ", ")),
              "psy_error_schema")
  }
  if ("reliable_change" %in% methods) {
    psy_abort("reliable_change is not implemented by detect_change(); call reliable_change() with an explicit reference.",
              "psy_error_schema")
  }
  if (!length(methods)) return(empty_events())
  if (is.null(baseline)) {
    psy_abort("baseline is required for shift detection.",
              "psy_error_insufficient_data")
  }
  if (!is.list(config) || is.null(config$threshold) ||
      is.null(config$persistence)) {
    psy_abort("config must contain threshold and persistence.", "psy_error_schema")
  }
  config_check <- psy_change_config(
    threshold = config$threshold, persistence = config$persistence,
    confidence = config$confidence %||% 0.95
  )
  detect_shift(x, baseline, threshold = config_check$threshold,
               persistence = config_check$persistence)
}
