report_fixture <- function() {
  events <- data.frame(
    event_id = c("event-person-1", "event-person-2"),
    person_id = c("person-1", "person-2"),
    event_type = "sustained_shift",
    stringsAsFactors = FALSE
  )
  reviews <- data.frame(
    review_id = c("review-person-1", "review-person-2"),
    event_id = events$event_id,
    reviewer_id = c("reviewer-1", "reviewer-2"),
    stringsAsFactors = FALSE
  )
  attr(events, "audit") <- psyActive:::new_audit("events", events)
  list(
    scores = data.frame(
      person_id = c("person-1", "person-2"),
      score_value = c(1, 99), stringsAsFactors = FALSE
    ),
    quality = data.frame(),
    baseline = data.frame(),
    events = events,
    reviews = reviews
  )
}

report_tempdir <- function() {
  path <- tempfile("psyactive-report-")
  dir.create(path)
  path
}

test_that("patient reports include only reviews linked to selected events", {
  paths <- render_psy_report(
    report_fixture(), person_id = "person-1", language = "en",
    output_dir = report_tempdir(), include_audit = FALSE
  )
  html <- paste(readLines(paths[["report"]], warn = FALSE), collapse = "\n")

  expect_match(html, "event-person-1", fixed = TRUE)
  expect_match(html, "review-person-1", fixed = TRUE)
  expect_false(grepl("event-person-2", html, fixed = TRUE))
  expect_false(grepl("review-person-2", html, fixed = TRUE))
  expect_false(grepl("reviewer-2", html, fixed = TRUE))
})

test_that("patient reports suppress reviews whose person cannot be established", {
  data <- report_fixture()
  data$events <- data$events[c("event_id", "event_type")]

  paths <- render_psy_report(
    data, person_id = "person-1", language = "en",
    output_dir = report_tempdir(), include_audit = FALSE
  )
  html <- paste(readLines(paths[["report"]], warn = FALSE), collapse = "\n")

  expect_false(grepl("review-person-1", html, fixed = TRUE))
  expect_false(grepl("review-person-2", html, fixed = TRUE))
})

test_that("patient reports suppress reviews linked by an ambiguous event id", {
  data <- report_fixture()
  data$events$event_id[] <- "event-shared"
  data$reviews$event_id[] <- "event-shared"

  paths <- render_psy_report(
    data, person_id = "person-1", language = "en",
    output_dir = report_tempdir(), include_audit = FALSE
  )
  html <- paste(readLines(paths[["report"]], warn = FALSE), collapse = "\n")

  expect_false(grepl("review-person-1", html, fixed = TRUE))
  expect_false(grepl("review-person-2", html, fixed = TRUE))
})

test_that("report artifacts are unique within one timestamp", {
  output_dir <- report_tempdir()
  stamp <- "20260813T123456Z"
  first <- psyActive:::reserve_report_base(
    output_dir, "patient", stamp
  )
  on.exit(unlink(first$lock, recursive = TRUE, force = TRUE), add = TRUE)
  file.create(file.path(output_dir, paste0(first$base, ".html")))
  unlink(first$lock, recursive = TRUE, force = TRUE)
  second <- psyActive:::reserve_report_base(
    output_dir, "patient", stamp
  )
  on.exit(unlink(second$lock, recursive = TRUE, force = TRUE), add = TRUE)

  expect_identical(first$base, "psyactive-patient-20260813T123456Z")
  expect_identical(second$base, "psyactive-patient-20260813T123456Z-001")
  expect_true(dir.exists(second$lock))
})

test_that("manifest hashes the report and optional audit artifact", {
  paths <- render_psy_report(
    report_fixture(), language = "en", output_dir = report_tempdir(),
    include_audit = TRUE
  )
  manifest <- jsonlite::read_json(paths[["manifest"]], simplifyVector = TRUE)

  expect_identical(
    manifest$report_hash,
    digest::digest(file = paths[["report"]], algo = "sha256")
  )
  expect_identical(
    manifest$audit_hash,
    digest::digest(file = paths[["audit"]], algo = "sha256")
  )
})

test_that("patient report audit hashes only filtered report sections", {
  first_data <- report_fixture()
  second_data <- report_fixture()
  second_data$scores$score_value[second_data$scores$person_id == "person-2"] <- 777
  second_data$events$event_type[second_data$events$person_id == "person-2"] <-
    "other-person-secret"
  attr(second_data$events, "audit") <- psyActive:::new_audit(
    "events", second_data$events
  )

  first <- render_psy_report(
    first_data, person_id = "person-1", language = "en",
    output_dir = report_tempdir(), include_audit = TRUE
  )
  second <- render_psy_report(
    second_data, person_id = "person-1", language = "en",
    output_dir = report_tempdir(), include_audit = TRUE
  )
  first_audit <- utils::read.csv(first[["audit"]], stringsAsFactors = FALSE)
  second_audit <- utils::read.csv(second[["audit"]], stringsAsFactors = FALSE)

  expect_identical(first_audit$operation, "render_psy_report")
  expect_identical(second_audit$operation, "render_psy_report")
  expect_identical(first_audit$input_hash, second_audit$input_hash)
})
