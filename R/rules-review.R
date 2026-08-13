ruleset_abort <- function(message) {
  psy_abort(message, "psy_error_schema")
}

.psy_review_dispositions <- c(
  "confirmed", "not_concerning", "insufficient_data", "duplicate", "escalated"
)

review_time_scalar <- function(x, label, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  value <- tryCatch(
    suppressWarnings(as_utc(x, "UTC")),
    error = function(e) NULL
  )
  if (is.null(value) || length(value) != 1L || is.na(value) ||
      !is.finite(as.numeric(value))) {
    qualifier <- if (allow_null) "NULL or one" else "one"
    psy_abort(
      paste(label, "must be", qualifier, "valid, finite time."),
      "psy_error_schema"
    )
  }
  value
}

rule_scalar_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    ruleset_abort(paste0(label, " must be one non-empty string."))
  }
  x
}

validate_condition_node <- function(node, depth = 0L) {
  if (depth > 32L) ruleset_abort("Rule conditions may not exceed 32 levels.")
  if (!is.list(node) || is.null(names(node)) || anyNA(names(node)) ||
      any(!nzchar(names(node))) || anyDuplicated(names(node))) {
    ruleset_abort("Each rule condition must be a uniquely named list.")
  }
  combinators <- intersect(names(node), c("all", "any", "not"))
  leaf_fields <- intersect(names(node), c("field", "operator", "value"))
  if (length(combinators)) {
    if (length(combinators) != 1L || length(leaf_fields) ||
        length(node) != 1L) {
      ruleset_abort("A condition must contain exactly one combinator or one comparison.")
    }
    kind <- combinators[1L]
    children <- node[[kind]]
    if (kind == "not") {
      if (!is.list(children) || is.null(names(children))) {
        ruleset_abort("not must contain exactly one condition.")
      }
      validate_condition_node(children, depth + 1L)
      return(invisible(TRUE))
    }
    if (!is.list(children) || !length(children) || !is.null(names(children))) {
      ruleset_abort(paste0(kind, " must contain a non-empty list of conditions."))
    }
    for (child in children) validate_condition_node(child, depth + 1L)
    return(invisible(TRUE))
  }

  allowed_names <- c("field", "operator", "value")
  unknown <- setdiff(names(node), allowed_names)
  if (length(unknown)) {
    ruleset_abort(paste("Unsupported condition field(s):",
                        paste(unknown, collapse = ", ")))
  }
  if (!all(c("field", "operator") %in% names(node))) {
    ruleset_abort("A comparison condition requires field and operator.")
  }
  field <- rule_scalar_string(node$field, "condition field")
  if (!grepl("^[A-Za-z][A-Za-z0-9_.]*$", field)) {
    ruleset_abort("condition field contains unsafe characters.")
  }
  operator <- rule_scalar_string(node$operator, "condition operator")
  allowed <- c("eq", "in", "gte", "lte", "exists")
  if (!operator %in% allowed) {
    ruleset_abort(sprintf("Unsupported rule operator: %s", operator))
  }
  if (operator == "exists") {
    if ("value" %in% names(node)) {
      ruleset_abort("exists does not accept a value.")
    }
    return(invisible(TRUE))
  }
  if (!"value" %in% names(node) || is.null(node$value) ||
      !is.atomic(node$value) || is.object(node$value)) {
    ruleset_abort(paste0(operator, " requires a literal atomic value."))
  }
  value <- node$value
  if (operator == "in") {
    finite_numeric <- if (is.numeric(value)) is.finite(value) else rep(TRUE, length(value))
    if (!length(value) || anyNA(value) || any(!finite_numeric)) {
      ruleset_abort("in requires one or more finite, non-missing literal values.")
    }
  } else if (length(value) != 1L || is.na(value)) {
    ruleset_abort(paste0(operator, " requires exactly one non-missing value."))
  }
  if (operator %in% c("gte", "lte")) {
    numeric_value <- suppressWarnings(as.numeric(value))
    if (length(numeric_value) != 1L || !is.finite(numeric_value)) {
      ruleset_abort(paste0(operator, " requires one finite numeric value."))
    }
  }
  invisible(TRUE)
}

validate_rule_action <- function(action) {
  if (!is.list(action) || is.null(names(action)) || anyNA(names(action)) ||
      any(!nzchar(names(action))) || anyDuplicated(names(action))) {
    ruleset_abort("Each rule action must be a uniquely named list.")
  }
  allowed <- c("alert_level", "reason_code")
  unknown <- setdiff(names(action), allowed)
  if (length(unknown)) {
    ruleset_abort(paste("Unsupported rule action(s):", paste(unknown, collapse = ", ")))
  }
  if (!length(action)) ruleset_abort("Each rule must define at least one action.")
  if (!is.null(action$alert_level)) {
    alert_level <- rule_scalar_string(action$alert_level, "alert_level")
    if (!alert_level %in% .psy_alert_levels) {
      ruleset_abort(paste("alert_level must be one of",
                          paste(.psy_alert_levels, collapse = ", ")))
    }
  }
  if (!is.null(action$reason_code)) {
    reason <- rule_scalar_string(action$reason_code, "reason_code")
    if (!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", reason)) {
      ruleset_abort("reason_code contains unsafe characters.")
    }
  }
  invisible(TRUE)
}

validate_ruleset_def <- function(x) {
  if (!is.list(x) || is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    ruleset_abort("Ruleset must be a uniquely named list.")
  }
  required <- c("schema_version", "ruleset_id", "version", "rules")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    ruleset_abort(paste("Ruleset is missing:", paste(missing, collapse = ", ")))
  }
  rule_scalar_string(x$schema_version, "schema_version")
  rule_scalar_string(x$ruleset_id, "ruleset_id")
  rule_scalar_string(x$version, "version")
  if (!is.list(x$rules) || (!is.null(names(x$rules)) && length(x$rules))) {
    ruleset_abort("rules must be an unnamed list.")
  }
  if (length(x$rules) > 1000L) ruleset_abort("A ruleset may contain at most 1000 rules.")
  rule_ids <- character(length(x$rules))
  for (i in seq_along(x$rules)) {
    rule <- x$rules[[i]]
    if (!is.list(rule) || is.null(names(rule)) || anyNA(names(rule)) ||
        any(!nzchar(names(rule))) || anyDuplicated(names(rule))) {
      ruleset_abort(sprintf("Rule %d must be a uniquely named list.", i))
    }
    required_rule <- c("rule_id", "when", "then")
    missing_rule <- setdiff(required_rule, names(rule))
    if (length(missing_rule)) {
      ruleset_abort(paste0("Rule ", i, " is missing: ",
                           paste(missing_rule, collapse = ", ")))
    }
    allowed_rule <- c("rule_id", "description", "when", "then")
    unknown <- setdiff(names(rule), allowed_rule)
    if (length(unknown)) {
      ruleset_abort(paste("Unsupported rule field(s):", paste(unknown, collapse = ", ")))
    }
    rule_ids[i] <- rule_scalar_string(rule$rule_id, "rule_id")
    if (!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", rule_ids[i])) {
      ruleset_abort("rule_id contains unsafe characters.")
    }
    if (!is.null(rule$description)) rule_scalar_string(rule$description, "description")
    validate_condition_node(rule$when)
    validate_rule_action(rule$then)
  }
  if (anyDuplicated(rule_ids)) ruleset_abort("rule_id values must be unique.")
  invisible(TRUE)
}

#' Read a Declarative Ruleset
#' @param path YAML or JSON path.
#' @param validate Validate operator and field structure.
#' @return A `psy_ruleset` list.
#' @export
read_ruleset <- function(path, validate = TRUE) {
  x <- read_definition(path)
  if (validate) validate_ruleset_def(x)
  class(x) <- c("psy_ruleset", "list")
  attr(x, "source_path") <- normalizePath(path)
  x
}

#' Validate a Ruleset
#' @param x Ruleset object.
#' @param registry Optional registry.
#' @param strict Stop on errors.
#' @return A `psy_quality` object.
#' @export
validate_ruleset <- function(x, registry = NULL, strict = TRUE) {
  if (length(strict) != 1L || is.na(strict) || !is.logical(strict)) {
    psy_abort("strict must be TRUE or FALSE.", "psy_error_schema")
  }
  error <- tryCatch({
    validate_ruleset_def(x)
    NULL
  }, error = function(e) e)
  if (is.null(error)) return(new_quality())
  quality <- new_quality(make_quality(
    "ruleset_schema", "error", message = conditionMessage(error),
    suggestion = "Use only documented declarative fields, operators, and actions."
  ))
  if (strict) stop(error)
  quality
}

condition_eval <- function(node, row) {
  if (!is.null(node$all)) {
    return(all(vapply(node$all, condition_eval, logical(1), row = row)))
  }
  if (!is.null(node$any)) {
    return(any(vapply(node$any, condition_eval, logical(1), row = row)))
  }
  if (!is.null(node$not)) return(!condition_eval(node$not, row))
  field <- node$field
  if (!field %in% names(row)) return(FALSE)
  lhs <- row[[field]][1L]
  if (node$operator == "exists") {
    return(length(lhs) == 1L && !is.na(lhs) &&
             (!is.character(lhs) || nzchar(lhs)))
  }
  if (length(lhs) != 1L || is.na(lhs)) return(FALSE)
  rhs <- node$value
  switch(
    node$operator,
    eq = as.character(lhs) == as.character(rhs),
    `in` = as.character(lhs) %in% as.character(rhs),
    gte = {
      value <- suppressWarnings(as.numeric(lhs))
      is.finite(value) && value >= as.numeric(rhs)
    },
    lte = {
      value <- suppressWarnings(as.numeric(lhs))
      is.finite(value) && value <= as.numeric(rhs)
    },
    FALSE
  )
}

#' Apply Transparent Workflow Rules
#' @param events Event data.
#' @param scores Optional score data.
#' @param quality Optional quality data.
#' @param ruleset Ruleset object or path.
#' @param as_of Evaluation time.
#' @param audit Attach audit record.
#' @return Prioritized `psy_event` object.
#' @export
apply_rules <- function(events, scores = NULL, quality = NULL, ruleset,
                        as_of = Sys.time(), audit = TRUE) {
  if (!is.data.frame(events)) {
    psy_abort("events must be a data frame.", "psy_error_schema")
  }
  if (is.character(ruleset)) ruleset <- read_ruleset(ruleset)
  validate_ruleset_def(ruleset)
  as_of <- as_utc(as_of, "UTC")
  if (length(as_of) != 1L || is.na(as_of)) {
    psy_abort("as_of must be one valid time.", "psy_error_schema")
  }
  out <- as.data.frame(events, stringsAsFactors = FALSE)
  if (!NROW(out)) {
    out <- new_psy_df(out, "psy_event")
    if (audit) {
      attr(out, "audit") <- new_audit(
        "apply_rules", events, out,
        list(ruleset_id = ruleset$ruleset_id,
             version = ruleset$version, as_of = as_of)
      )
    }
    return(out)
  }
  if (!"event_id" %in% names(out)) {
    psy_abort("events must contain event_id.", "psy_error_schema")
  }
  if (!"alert_level" %in% names(out)) {
    out$alert_level <- factor(rep("none", NROW(out)),
                              levels = .psy_alert_levels, ordered = TRUE)
  }
  if (!"reason_code" %in% names(out)) out$reason_code <- NA_character_
  if (!"rule_id" %in% names(out)) out$rule_id <- NA_character_
  if (!"rule_version" %in% names(out)) out$rule_version <- NA_character_
  out$alert_level <- as.character(out$alert_level)
  out$reason_code <- as.character(out$reason_code)
  out$rule_id <- as.character(out$rule_id)
  out$rule_version <- as.character(out$rule_version)
  for (i in seq_len(NROW(out))) {
    for (rule in ruleset$rules) {
      if (!condition_eval(rule$when, out[i, , drop = FALSE])) next
      if (!is.null(rule$then$alert_level)) {
        out$alert_level[i] <- as.character(rule$then$alert_level)
      }
      if (!is.null(rule$then$reason_code)) {
        out$reason_code[i] <- as.character(rule$then$reason_code)
      }
      out$rule_id[i] <- as.character(rule$rule_id)
      out$rule_version[i] <- as.character(ruleset$version)
    }
  }
  out$alert_level <- factor(as.character(out$alert_level),
                            levels = .psy_alert_levels, ordered = TRUE)
  out <- new_psy_df(out, "psy_event")
  if (audit) {
    attr(out, "audit") <- new_audit(
      "apply_rules", events, out,
      list(ruleset_id = ruleset$ruleset_id,
           version = ruleset$version, as_of = as_of)
    )
  }
  out
}

#' Record a Human Review
#' @param event One event row or a non-missing, non-empty event identifier.
#' @param reviewer_id Non-missing, non-empty pseudonymous local reviewer
#'   identifier.
#' @param disposition Review disposition.
#' @param action_code Optional institution-defined action code.
#' @param rationale_code Optional rationale code.
#' @param next_review_at `NULL` or one valid, finite next review time. Supplied
#'   values are normalized to UTC.
#' @param reviewed_at One valid, finite review time, normalized to UTC.
#' @return A one-row `psy_review` object.
#' @export
record_review <- function(event, reviewer_id, disposition, action_code = NULL,
                          rationale_code = NULL, next_review_at = NULL,
                          reviewed_at = Sys.time()) {
  allowed <- .psy_review_dispositions
  if (!is.character(disposition) || length(disposition) != 1L ||
      is.na(disposition) || !disposition %in% allowed) {
    psy_abort(paste("disposition must be one of", paste(allowed, collapse = ", ")),
              "psy_error_schema")
  }
  reviewer_id <- as.character(reviewer_id)
  if (length(reviewer_id) != 1L || is.na(reviewer_id) ||
      !nzchar(trimws(reviewer_id))) {
    psy_abort("reviewer_id must be one non-empty string.", "psy_error_schema")
  }
  event_id <- if (is.data.frame(event)) {
    if (!"event_id" %in% names(event) || NROW(event) != 1L) character() else {
      as.character(event$event_id)
    }
  } else {
    as.character(event)
  }
  if (length(event_id) != 1L || is.na(event_id) ||
      !nzchar(trimws(event_id))) {
    psy_abort("A valid event_id is required.", "psy_error_schema")
  }
  reviewed_at <- review_time_scalar(reviewed_at, "reviewed_at")
  next_review_at <- review_time_scalar(
    next_review_at, "next_review_at", allow_null = TRUE
  )
  out <- data.frame(
    review_id = stable_id("review", event_id, reviewer_id, reviewed_at, disposition),
    event_id = event_id, reviewer_id = reviewer_id,
    reviewed_at = reviewed_at, disposition = disposition,
    action_code = action_code %||% NA_character_,
    rationale_code = rationale_code %||% NA_character_,
    next_review_at = next_review_at, stringsAsFactors = FALSE
  )
  new_psy_df(out, "psy_review",
             audit = new_audit("record_review", event, out,
                               list(reviewer_id = reviewer_id,
                                    disposition = disposition)))
}

#' Derive Current Event Status
#' @param events Event data with unique, non-missing, non-empty `event_id`
#'   values. If present, `event_status` must use a supported status value.
#' @param reviews Review history with valid `event_id`, `disposition`, and
#'   `reviewed_at` fields.
#' @return Event data with derived status and latest-review fields.
#' @details Reviews for event identifiers not present in `events` are ignored.
#'   This permits deriving status for a subset of events from a complete review
#'   history, but all review rows are validated before matching.
#' @export
event_status <- function(events, reviews) {
  if (!is.data.frame(events) || !is.data.frame(reviews)) {
    psy_abort("events and reviews must be data frames.", "psy_error_schema")
  }
  out <- as.data.frame(events, stringsAsFactors = FALSE)
  if (!NROW(out) && !"event_id" %in% names(out)) out$event_id <- character()
  if (!"event_id" %in% names(out)) {
    psy_abort("events must contain event_id.", "psy_error_schema")
  }
  event_ids <- as.character(out$event_id)
  invalid_event_ids <- is.na(event_ids) | !nzchar(trimws(event_ids))
  if (any(invalid_event_ids)) {
    psy_abort(
      "events$event_id must contain non-missing, non-empty values.",
      "psy_error_schema"
    )
  }
  if (anyDuplicated(event_ids)) {
    psy_abort("events$event_id must be unique.", "psy_error_schema")
  }
  out$event_id <- event_ids
  if ("event_status" %in% names(out)) {
    current <- as.character(out$event_status)
    invalid_status <- is.na(current) | !current %in% .psy_event_status
    if (any(invalid_status)) {
      psy_abort(
        paste0(
          "events$event_status must contain only: ",
          paste(.psy_event_status, collapse = ", "), "."
        ),
        "psy_error_schema"
      )
    }
    out$event_status <- current
  } else {
    out$event_status <- rep("open", NROW(out))
  }
  out$latest_disposition <- rep(NA_character_, NROW(out))
  out$latest_reviewed_at <- as.POSIXct(rep(NA_real_, NROW(out)),
                                      origin = "1970-01-01", tz = "UTC")
  reviews <- as.data.frame(reviews, stringsAsFactors = FALSE)
  required_reviews <- c("event_id", "disposition", "reviewed_at")
  missing <- setdiff(required_reviews, names(reviews))
  if (length(missing)) {
    if (!NROW(out) && !NROW(reviews) && !length(names(reviews))) {
      reviews$event_id <- character()
      reviews$disposition <- character()
      reviews$reviewed_at <- as.POSIXct(character(), tz = "UTC")
    } else {
      psy_abort(paste("reviews is missing:", paste(missing, collapse = ", ")),
                "psy_error_schema")
    }
  }
  review_event_ids <- as.character(reviews$event_id)
  invalid_review_event_ids <- is.na(review_event_ids) |
    !nzchar(trimws(review_event_ids))
  if (any(invalid_review_event_ids)) {
    psy_abort(
      "reviews$event_id must contain non-missing, non-empty values.",
      "psy_error_schema"
    )
  }
  reviews$event_id <- review_event_ids

  dispositions <- as.character(reviews$disposition)
  invalid_dispositions <- is.na(dispositions) |
    !dispositions %in% .psy_review_dispositions
  if (any(invalid_dispositions)) {
    psy_abort(
      paste0(
        "reviews$disposition must contain only: ",
        paste(.psy_review_dispositions, collapse = ", "), "."
      ),
      "psy_error_schema"
    )
  }
  reviews$disposition <- dispositions

  reviewed_at <- tryCatch(
    time_vector(reviews$reviewed_at),
    error = function(e) NULL
  )
  invalid_reviewed_at <- is.null(reviewed_at) ||
    length(reviewed_at) != NROW(reviews) || anyNA(reviewed_at) ||
    any(!is.finite(as.numeric(reviewed_at)))
  if (invalid_reviewed_at) {
    psy_abort(
      "reviews$reviewed_at must contain valid, non-missing UTC times.",
      "psy_error_schema"
    )
  }
  reviews$reviewed_at <- reviewed_at

  if (!NROW(out) || !NROW(reviews)) return(new_psy_df(out, "psy_event"))
  for (i in seq_len(NROW(out))) {
    keep <- reviews$event_id == out$event_id[i]
    z <- reviews[keep, , drop = FALSE]
    if (!NROW(z)) next
    z <- z[order(z$reviewed_at, na.last = TRUE), , drop = FALSE]
    last <- z[NROW(z), , drop = FALSE]
    out$latest_disposition[i] <- as.character(last$disposition[1L])
    out$latest_reviewed_at[i] <- last$reviewed_at[1L]
    out$event_status[i] <- if (last$disposition[1L] == "escalated") {
      "escalated"
    } else "reviewed"
  }
  new_psy_df(out, "psy_event")
}
