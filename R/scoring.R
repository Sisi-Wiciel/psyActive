score_one <- function(vals, spec) {
  vals <- suppressWarnings(as.numeric(vals))
  ok <- !is.na(vals)
  answered <- sum(ok)
  total <- length(vals)
  min_answered <- suppressWarnings(as.integer(spec$min_answered %||% total))

  if (length(min_answered) != 1L || is.na(min_answered) ||
      min_answered < 0L || min_answered > total) {
    psy_abort("min_answered must be between zero and the item count.",
              "psy_error_instrument")
  }
  if (answered < min_answered) {
    return(list(value = NA_real_, status = "insufficient",
                answered = answered, missing = total - answered,
                prorated = FALSE))
  }

  operation <- as.character(spec$operation %||% "")
  if (length(operation) != 1L || is.na(operation) || !nzchar(operation)) {
    psy_abort("Every score definition must specify an operation.",
              "psy_error_instrument")
  }
  if (operation == "sum") {
    value <- sum(vals[ok])
  } else if (operation == "mean") {
    value <- if (answered) mean(vals[ok]) else NA_real_
  } else if (operation == "weighted_sum") {
    weights <- suppressWarnings(as.numeric(unlist(spec$weights)))
    if (length(weights) != total || any(!is.finite(weights))) {
      psy_abort("weighted_sum requires one finite weight per item.",
                "psy_error_scoring")
    }
    value <- sum(vals[ok] * weights[ok])
  } else if (operation == "count_if") {
    comparison <- as.character(spec$operator %||% "gte")[1L]
    cutoff <- suppressWarnings(as.numeric(spec$value))
    if (length(cutoff) != 1L || is.na(cutoff) || !is.finite(cutoff)) {
      psy_abort("count_if requires a finite numeric value.",
                "psy_error_scoring")
    }
    matches <- switch(
      comparison,
      eq = vals[ok] == cutoff,
      gte = vals[ok] >= cutoff,
      lte = vals[ok] <= cutoff,
      psy_abort("Unsupported count_if operator.", "psy_error_scoring")
    )
    value <- sum(matches)
  } else {
    psy_abort(sprintf("Unsupported operation: %s", operation),
              "psy_error_scoring")
  }

  prorated <- FALSE
  if (answered < total && isTRUE(spec$prorate)) {
    if (operation != "sum") {
      psy_abort("Prorating is supported only for sum scores.",
                "psy_error_instrument")
    }
    if (answered == 0L) {
      return(list(value = NA_real_, status = "insufficient",
                  answered = answered, missing = total - answered,
                  prorated = FALSE))
    }
    value <- value * total / answered
    prorated <- TRUE
  }
  list(value = as.numeric(value),
       status = if (prorated) "prorated" else "complete",
       answered = answered, missing = total - answered,
       prorated = prorated)
}

first_observed_at <- function(x) {
  if (!length(x)) return(as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"))
  if (inherits(x, "POSIXt")) {
    good <- !is.na(x) & is.finite(as.numeric(x))
    if (!any(good)) return(as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"))
    return(min(x[good]))
  }
  x <- suppressWarnings(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  good <- !is.na(x) & is.finite(as.numeric(x))
  if (!any(good)) as.POSIXct(NA, origin = "1970-01-01", tz = "UTC") else min(x[good])
}

#' Score a Registered Instrument
#' @param x Standard observations.
#' @param instrument Instrument object, path, or registered ID.
#' @param registry Registry object.
#' @param scores Optional score-name subset.
#' @param on_invalid Invalid-value policy.
#' @param audit Attach an audit record.
#' @return A `psy_score` data frame.
#' @export
score_instrument <- function(x, instrument, registry = psy_registry(),
                             scores = NULL,
                             on_invalid = c("error", "exclude", "keep_na"),
                             audit = TRUE) {
  on_invalid <- match.arg(on_invalid)
  if (!is.data.frame(x)) {
    psy_abort("x must be a data frame of standard observations.",
              "psy_error_scoring")
  }
  required <- c("person_id", "assessment_id", "observed_at",
                "instrument_id", "instrument_version", "item_id", "value_num")
  missing_fields <- setdiff(required, names(x))
  if (length(missing_fields)) {
    psy_abort(paste("Scoring input is missing:",
                    paste(missing_fields, collapse = ", ")),
              "psy_error_schema")
  }
  inst <- get_instrument(instrument, registry)
  key <- !is.na(x$instrument_id) & !is.na(x$instrument_version) &
    x$instrument_id == inst$instrument_id & x$instrument_version == inst$version
  dat <- x[!is.na(key) & key, , drop = FALSE]
  if (!NROW(dat)) {
    psy_abort("No observations match the requested instrument and version.",
              "psy_error_scoring")
  }

  item_ids <- vapply(inst$items, function(z) as.character(z$item_id %||% "")[1L],
                     character(1))
  item_defs <- stats::setNames(inst$items, item_ids)
  invalid <- logical(NROW(dat))
  for (i in seq_len(NROW(dat))) {
    definition <- item_defs[[as.character(dat$item_id[i])]]
    if (is.null(definition)) {
      invalid[i] <- TRUE
      next
    }
    value <- suppressWarnings(as.numeric(dat$value_num[i]))
    if (is.nan(value) || (!is.na(value) && !is.finite(value))) {
      invalid[i] <- TRUE
      next
    }
    allowed <- suppressWarnings(as.numeric(unlist(definition$allowed_values)))
    lower <- definition$minimum %||% definition$min %||%
      definition$range$minimum %||% definition$range$min
    upper <- definition$maximum %||% definition$max %||%
      definition$range$maximum %||% definition$range$max
    bad <- !is.na(value) && length(allowed) && !value %in% allowed
    if (!is.null(lower)) bad <- bad || (!is.na(value) && value < as.numeric(lower))
    if (!is.null(upper)) bad <- bad || (!is.na(value) && value > as.numeric(upper))
    if (isTRUE(bad)) {
      invalid[i] <- TRUE
    }
  }

  quality <- new_quality()
  if (any(invalid)) {
    quality <- new_quality(make_quality(
      "range", "error", paste(which(invalid), collapse = ","), "value_num",
      "Values or item IDs fall outside the instrument definition.",
      "Correct values or choose an explicit invalid-value policy."
    ))
    if (on_invalid == "error") {
      psy_abort("Invalid instrument values were found.",
                "psy_error_scoring", problems = quality)
    }
    if (on_invalid == "exclude") {
      dat <- dat[!invalid, , drop = FALSE]
    } else {
      dat$value_num[invalid] <- NA_real_
    }
  }
  if (!NROW(dat)) {
    psy_abort("No valid observations remain after applying on_invalid.",
              "psy_error_scoring", problems = quality)
  }
  group_fields <- c("person_id", "assessment_id", "item_id")
  bad_group <- vapply(dat[group_fields], function(value) {
    value <- as.character(value)
    any(is.na(value) | !nzchar(value))
  }, logical(1))
  if (any(bad_group)) {
    psy_abort(paste("Scoring keys must be non-missing:",
                    paste(group_fields[bad_group], collapse = ", ")),
              "psy_error_schema")
  }

  definitions <- inst$scores %||% list()
  if (!is.null(scores)) {
    definitions <- Filter(
      function(z) (z$score_name %||% "")[1L] %in% scores,
      definitions
    )
  }
  if (!length(definitions)) {
    psy_abort("No requested score definitions were found.",
              "psy_error_scoring")
  }

  group_key <- interaction(
    as.character(dat$person_id), as.character(dat$assessment_id),
    as.character(dat$instrument_id), as.character(dat$instrument_version),
    drop = TRUE, lex.order = TRUE, sep = "\u001f"
  )
  groups <- split(dat, group_key)
  rows <- list()
  explanations <- list()
  k <- 0L
  e <- 0L
  for (group in groups) {
    if (anyDuplicated(as.character(group$item_id))) {
      psy_abort("Duplicate item IDs were found within an assessment.",
                "psy_error_scoring")
    }
    for (spec in definitions) {
      ids <- as.character(unlist(spec$items))
      if (!length(ids) || anyNA(ids) || any(!nzchar(ids))) {
        psy_abort("Score definitions must reference non-empty item IDs.",
                  "psy_error_instrument")
      }
      unknown <- setdiff(ids, item_ids)
      if (length(unknown)) {
        psy_abort(
          paste("Score definition references unknown items:",
                paste(unknown, collapse = ", ")),
          "psy_error_instrument"
        )
      }
      # Build the complete item vector explicitly; data-frame subsetting with
      # NA positions can otherwise leak unrelated column values into a score.
      positions <- match(ids, as.character(group$item_id))
      found <- !is.na(positions)
      raw_values <- rep(NA_character_, length(ids))
      numeric_values <- rep(NA_real_, length(ids))
      observation_ids <- rep(NA_character_, length(ids))
      if ("value_raw" %in% names(group)) raw_values[found] <- as.character(group$value_raw[positions[found]])
      numeric_values[found] <- suppressWarnings(as.numeric(group$value_num[positions[found]]))
      if ("observation_id" %in% names(group)) observation_ids[found] <- as.character(group$observation_id[positions[found]])
      values <- numeric_values
      for (j in seq_along(ids)) {
        definition <- item_defs[[ids[j]]]
        if (!is.null(definition$reverse)) {
          minimum <- suppressWarnings(as.numeric(definition$reverse$min))[1L]
          maximum <- suppressWarnings(as.numeric(definition$reverse$max))[1L]
          if (is.finite(minimum) && is.finite(maximum)) {
            values[j] <- if (is.na(values[j])) NA_real_ else minimum + maximum - values[j]
          }
        }
      }
      result <- score_one(values, spec)
      k <- k + 1L
      computed <- now_utc()
      score_name <- as.character(spec$score_name %||% "")[1L]
      score_id <- stable_id(
        "score", group$person_id[1L], group$assessment_id[1L],
        inst$instrument_id, inst$version, score_name
      )
      rows[[k]] <- data.frame(
        score_id = score_id, person_id = as.character(group$person_id[1L]),
        assessment_id = as.character(group$assessment_id[1L]),
        observed_at = first_observed_at(group$observed_at),
        instrument_id = inst$instrument_id,
        instrument_version = inst$version,
        score_name = score_name, score_value = result$value,
        score_min = suppressWarnings(as.numeric(spec$theoretical_min %||% NA_real_))[1L],
        score_max = suppressWarnings(as.numeric(spec$theoretical_max %||% NA_real_))[1L],
        answered_n = as.integer(result$answered),
        missing_n = as.integer(result$missing), prorated = result$prorated,
        score_status = result$status, scoring_version = inst$version,
        input_hash = stable_hash(sort(observation_ids[!is.na(observation_ids)])),
        computed_at = computed, stringsAsFactors = FALSE
      )
      for (j in seq_along(ids)) {
        e <- e + 1L
        weights <- if (!is.null(spec$weights)) {
          suppressWarnings(as.numeric(unlist(spec$weights)))[j]
        } else 1
        explanations[[e]] <- data.frame(
          score_id = score_id, item_id = ids[j],
          value_raw = raw_values[j], value_num = numeric_values[j],
          transformed_value = values[j],
          reversed = !is.null(item_defs[[ids[j]]]$reverse),
          weight = weights, missing = is.na(values[j]),
          operation = as.character(spec$operation %||% "")[1L],
          scoring_version = inst$version,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- new_psy_df(do.call(rbind, rows), "psy_score", quality)
  attr(out, "explanation") <- if (length(explanations)) {
    do.call(rbind, explanations)
  } else data.frame()
  if (audit) {
    attr(out, "audit") <- new_audit(
      "score_instrument", x, out,
      list(instrument = inst$instrument_id, version = inst$version,
           on_invalid = on_invalid), NROW(quality)
    )
  }
  out
}

#' Explain Score Inputs and Transformations
#' @param x A score object.
#' @param score_id Optional score identifier.
#' @return Item-level scoring provenance.
#' @export
explain_score <- function(x, score_id = NULL) {
  out <- attr(x, "explanation")
  if (is.null(out)) return(data.frame())
  if (!is.null(score_id)) out <- out[out$score_id %in% score_id, , drop = FALSE]
  out
}

#' Interpret Scores Using an Explicit Reference
#' @param x Score object.
#' @param reference Reference object, path, or registered ID.
#' @param registry Registry object.
#' @param mismatch Mismatch policy.
#' @return Updated `psy_score` with interpretation columns.
#' @export
interpret_score <- function(x, reference, registry = psy_registry(),
                            mismatch = c("error", "warn", "allow")) {
  mismatch <- match.arg(mismatch)
  ref <- get_reference(reference, registry)
  bad <- any(
    x$instrument_id != ref$instrument_id |
      x$instrument_version != ref$instrument_version,
    na.rm = TRUE
  )
  if (bad) {
    message <- "Reference instrument/version does not match the score object."
    if (mismatch == "error") psy_abort(message, "psy_error_reference")
    if (mismatch == "warn") psy_warn(message, "psy_warning_reference_mismatch")
  }
  x$reference_id <- ref$reference_id
  x$severity_band <- NA_character_
  for (band in ref$severity_bands %||% list()) {
    hit <- !is.na(x$score_value) &
      x$score_value >= as.numeric(band$lower) &
      x$score_value <= as.numeric(band$upper)
    x$severity_band[hit] <- band$label
  }
  x$mcid <- as.numeric(ref$mcid$value %||% NA_real_)
  x$interpretation_status <- ifelse(
    is.na(x$score_value), "not_available",
    ifelse(is.na(x$severity_band), "unclassified", "classified")
  )
  class(x) <- c("psy_score", "data.frame")
  attr(x, "reference") <- ref
  x
}
