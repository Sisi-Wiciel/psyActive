event_rows <- function(status = NULL) {
  out <- data.frame(
    event_id = c("event-1", "event-2", "event-3"),
    stringsAsFactors = FALSE
  )
  if (!is.null(status)) out$event_status <- status
  out
}

review_rows <- function() {
  data.frame(
    event_id = c("event-1", "event-1", "event-2", "event-not-selected"),
    disposition = c("escalated", "confirmed", "escalated", "duplicate"),
    reviewed_at = c(
      "2026-02-01 09:00:00", "2026-02-02 09:00:00",
      "2026-02-03 09:00:00", "2026-02-04 09:00:00"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("event_status validates event identifiers and existing statuses", {
  expect_error(
    event_status(data.frame(value = 1), review_rows()),
    "events must contain event_id",
    class = "psy_error_schema"
  )
  for (bad_id in list(NA_character_, "", "   ")) {
    events <- data.frame(event_id = bad_id, stringsAsFactors = FALSE)
    expect_error(
      event_status(events, review_rows()),
      "non-missing, non-empty",
      class = "psy_error_schema"
    )
  }
  expect_error(
    event_status(data.frame(event_id = c("same", "same")), review_rows()),
    "must be unique",
    class = "psy_error_schema"
  )
  for (bad_status in list(NA_character_, "", "pending")) {
    events <- data.frame(
      event_id = "event-1", event_status = bad_status,
      stringsAsFactors = FALSE
    )
    expect_error(
      event_status(events, review_rows()),
      "events\\$event_status must contain only",
      class = "psy_error_schema"
    )
  }

  valid <- event_status(
    event_rows(c("open", "acknowledged", "closed")),
    review_rows()[0, ]
  )
  expect_equal(valid$event_status, c("open", "acknowledged", "closed"))
})

test_that("event_status validates the review schema and values", {
  reviews <- review_rows()
  for (field in c("event_id", "disposition", "reviewed_at")) {
    expect_error(
      event_status(event_rows(), reviews[setdiff(names(reviews), field)]),
      "reviews is missing",
      class = "psy_error_schema"
    )
  }
  expect_error(
    event_status(event_rows(), data.frame()),
    "reviews is missing",
    class = "psy_error_schema"
  )

  for (bad_id in list(NA_character_, "", "   ")) {
    invalid <- reviews[1, ]
    invalid$event_id <- bad_id
    expect_error(
      event_status(event_rows(), invalid),
      "reviews\\$event_id must contain non-missing, non-empty",
      class = "psy_error_schema"
    )
  }
  for (bad_disposition in list(NA_character_, "", "pending")) {
    invalid <- reviews[1, ]
    invalid$disposition <- bad_disposition
    expect_error(
      event_status(event_rows(), invalid),
      "reviews\\$disposition must contain only",
      class = "psy_error_schema"
    )
  }

  invalid_time <- reviews[1, ]
  invalid_time$reviewed_at <- NA_character_
  expect_error(
    event_status(event_rows(), invalid_time),
    "valid, non-missing UTC times",
    class = "psy_error_schema"
  )
  invalid_time$reviewed_at <- "not-a-time"
  expect_error(
    event_status(event_rows(), invalid_time),
    "valid, non-missing UTC times",
    class = "psy_error_schema"
  )
  invalid_time$reviewed_at <- Inf
  expect_error(
    event_status(event_rows(), invalid_time),
    "valid, non-missing UTC times",
    class = "psy_error_schema"
  )
})

test_that("event_status uses the latest matching valid review", {
  result <- event_status(
    event_rows(c("open", "acknowledged", "closed")),
    review_rows()
  )

  expect_equal(result$event_status, c("reviewed", "escalated", "closed"))
  expect_equal(
    result$latest_disposition,
    c("confirmed", "escalated", NA_character_)
  )
  expect_s3_class(result$latest_reviewed_at, "POSIXct")
  expect_equal(attr(result$latest_reviewed_at, "tzone"), "UTC")
  expect_equal(
    format(result$latest_reviewed_at[1:2], tz = "UTC"),
    c("2026-02-02 09:00:00", "2026-02-03 09:00:00")
  )
  expect_true(is.na(result$latest_reviewed_at[3]))
})

test_that("event_status accepts every record_review disposition", {
  dispositions <- c(
    "confirmed", "not_concerning", "insufficient_data", "duplicate", "escalated"
  )
  events <- data.frame(event_id = paste0("event-", seq_along(dispositions)))
  reviews <- data.frame(
    event_id = events$event_id,
    disposition = dispositions,
    reviewed_at = as.POSIXct("2026-02-01", tz = "UTC") + seq_along(dispositions),
    stringsAsFactors = FALSE
  )

  result <- event_status(events, reviews)
  expect_equal(result$latest_disposition, dispositions)
  expect_equal(
    result$event_status,
    c(rep("reviewed", length(dispositions) - 1L), "escalated")
  )
})
