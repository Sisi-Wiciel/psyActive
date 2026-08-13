test_that("record_review rejects missing and blank identifiers", {
  valid_time <- as.POSIXct("2026-02-01 09:00:00", tz = "UTC")

  for (bad_event_id in list(NA_character_, "", "   ")) {
    expect_error(
      record_review(
        bad_event_id, "reviewer-1", "confirmed", reviewed_at = valid_time
      ),
      "valid event_id",
      class = "psy_error_schema"
    )
    expect_error(
      record_review(
        data.frame(event_id = bad_event_id), "reviewer-1", "confirmed",
        reviewed_at = valid_time
      ),
      "valid event_id",
      class = "psy_error_schema"
    )
  }
  expect_error(
    record_review(
      data.frame(value = 1), "reviewer-1", "confirmed",
      reviewed_at = valid_time
    ),
    "valid event_id",
    class = "psy_error_schema"
  )
  expect_error(
    record_review(
      data.frame(event_id = c("event-1", "event-2")), "reviewer-1",
      "confirmed", reviewed_at = valid_time
    ),
    "valid event_id",
    class = "psy_error_schema"
  )

  for (bad_reviewer_id in list(NA_character_, "", "   ")) {
    expect_error(
      record_review(
        "event-1", bad_reviewer_id, "confirmed", reviewed_at = valid_time
      ),
      "reviewer_id must be one non-empty string",
      class = "psy_error_schema"
    )
  }
})

test_that("record_review strictly validates review times", {
  for (bad_time in list(
    NA_character_, "not-a-time", Inf,
    as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  )) {
    expect_error(
      record_review(
        "event-1", "reviewer-1", "confirmed", reviewed_at = bad_time
      ),
      "reviewed_at must be one valid, finite time",
      class = "psy_error_schema"
    )
  }
  expect_error(
    record_review(
      "event-1", "reviewer-1", "confirmed",
      reviewed_at = c("2026-02-01", "2026-02-02")
    ),
    "reviewed_at must be one valid, finite time",
    class = "psy_error_schema"
  )

  for (bad_time in list(
    NA_character_, "not-a-time", Inf,
    as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  )) {
    expect_error(
      record_review(
        "event-1", "reviewer-1", "confirmed",
        next_review_at = bad_time,
        reviewed_at = as.POSIXct("2026-02-01", tz = "UTC")
      ),
      "next_review_at must be NULL or one valid, finite time",
      class = "psy_error_schema"
    )
  }
  expect_error(
    record_review(
      "event-1", "reviewer-1", "confirmed",
      next_review_at = c("2026-02-02", "2026-02-03"),
      reviewed_at = as.POSIXct("2026-02-01", tz = "UTC")
    ),
    "next_review_at must be NULL or one valid, finite time",
    class = "psy_error_schema"
  )
})

test_that("record_review normalizes supplied times to UTC", {
  reviewed_at <- as.POSIXct("2026-02-01 09:00:00", tz = "Asia/Shanghai")
  next_review_at <- as.POSIXct("2026-02-02 10:30:00", tz = "America/New_York")
  review <- record_review(
    "event-1", "reviewer-1", "confirmed",
    next_review_at = next_review_at, reviewed_at = reviewed_at
  )

  expect_s3_class(review, "psy_review")
  expect_s3_class(review$reviewed_at, "POSIXct")
  expect_s3_class(review$next_review_at, "POSIXct")
  expect_equal(attr(review$reviewed_at, "tzone"), "UTC")
  expect_equal(attr(review$next_review_at, "tzone"), "UTC")
  expect_equal(as.numeric(review$reviewed_at), as.numeric(reviewed_at))
  expect_equal(as.numeric(review$next_review_at), as.numeric(next_review_at))

  no_next_review <- record_review(
    "event-1", "reviewer-1", "confirmed", reviewed_at = reviewed_at
  )
  expect_true(is.na(no_next_review$next_review_at))
  expect_equal(attr(no_next_review$next_review_at, "tzone"), "UTC")
})
