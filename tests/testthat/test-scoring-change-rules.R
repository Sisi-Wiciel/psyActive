score_fixture_instrument <- function() {
  read_instrument(system.file(
    "extdata", "instruments", "demo_mood_9.yml", package = "psyActive"
  ))
}

score_fixture_observations <- function(items, values) {
  data.frame(
    observation_id = paste0("observation-", seq_along(items)),
    person_id = "person-1", assessment_id = "assessment-1",
    observed_at = as.POSIXct("2026-01-01", tz = "UTC"),
    instrument_id = "demo_mood_9", instrument_version = "1.0.0",
    item_id = items, value_raw = as.character(values), value_num = values,
    stringsAsFactors = FALSE
  )
}

test_that("missing score items are explicit and invalid policies are honored", {
  instrument <- score_fixture_instrument()
  observations <- score_fixture_observations(paste0("dms_", 1:8), rep(1, 8))
  score <- score_instrument(observations, instrument, audit = FALSE)
  explanation <- explain_score(score)

  expect_true(is.na(score$score_value))
  expect_equal(score$score_status, "insufficient")
  expect_equal(c(score$answered_n, score$missing_n), c(8L, 1L))
  expect_equal(nrow(explanation), 9L)
  expect_true(is.na(explanation$value_num[9L]))

  instrument$scores[[1L]]$min_answered <- 8L
  invalid <- score_fixture_observations(
    paste0("dms_", 1:9), c(rep(1, 8), 99)
  )
  expect_error(
    score_instrument(invalid, instrument, on_invalid = "error"),
    class = "psy_error_scoring"
  )
  excluded <- score_instrument(
    invalid, instrument, on_invalid = "exclude", audit = FALSE
  )
  kept_na <- score_instrument(
    invalid, instrument, on_invalid = "keep_na", audit = FALSE
  )
  expect_equal(excluded$score_value, 8)
  expect_equal(kept_na$score_value, 8)
  expect_equal(excluded$answered_n, 8L)
  expect_equal(kept_na$answered_n, 8L)
})

test_that("baseline learning handles finite values and empty selections", {
  time <- as.POSIXct("2026-01-01", tz = "UTC") + 86400 * 0:5
  metrics <- data.frame(
    person_id = "person-1", metric_id = "mood",
    value = c(1, 2, 3, 4, NA, Inf), observed_at = time
  )
  baseline <- learn_baseline(metrics, min_n = 4L, audit = FALSE)
  empty <- learn_baseline(metrics, metrics = "other", audit = FALSE)

  expect_equal(baseline$n_obs, 4L)
  expect_equal(baseline$status, "ok")
  expect_s3_class(empty, "psy_baseline")
  expect_equal(nrow(empty), 0L)
  expect_s3_class(empty$window_start, "POSIXct")
  expect_error(
    learn_baseline(metrics, window = c(NA, NA)), class = "psy_error_schema"
  )
})

test_that("reliable change groups instruments, versions, and episodes", {
  reference <- read_reference(system.file(
    "extdata", "references", "demo_mood_9_zh_adult_v1.yml",
    package = "psyActive"
  ))
  time <- as.POSIXct("2026-01-01", tz = "UTC") + 86400 * 0:5
  score <- data.frame(
    person_id = "person-1", instrument_id = c("a", "a", "a", "b", "b", "b"),
    instrument_version = c("1", "1", "1", "1", "1", "1"),
    score_name = "total", score_value = c(1, 10, 20, 100, 90, 80),
    observed_at = time, episode_id = c("e1", "e1", "e2", "e1", "e1", "e1")
  )
  changed <- reliable_change(score, reference, from = "episode_start")
  expect_equal(changed$change_value, c(0, 9, 0, 0, -10, -20))
  expect_error(
    reliable_change(score[names(score) != "episode_id"], reference,
                    from = "episode_start"),
    class = "psy_error_schema"
  )
})

test_that("persistent shifts use confirmation time and enforce cooldown", {
  time <- as.POSIXct("2026-01-01", tz = "UTC") + 86400 * 0:9
  metrics <- data.frame(
    person_id = "person-1", metric_id = "mood",
    value = c(0, 3, 3, 3, 0, 3, 3, 0, 3, 3), observed_at = time
  )
  baseline <- data.frame(
    person_id = "person-1", metric_id = "mood", center = 0, scale = 1
  )
  events <- detect_shift(
    metrics, baseline, threshold = 2.5, persistence = 2L,
    cooldown = 5L, audit = FALSE
  )

  expect_equal(events$event_time, time[c(3, 10)])
  expect_error(detect_shift(metrics, baseline, persistence = 1.5),
               class = "psy_error_schema")
  configured <- detect_change(metrics, baseline)
  expect_s3_class(configured, "psy_event")
  expect_equal(configured$event_time, time[c(3, 7, 10)])
  expect_error(
    detect_change(metrics, baseline, methods = "reliable_change"),
    class = "psy_error_schema"
  )
})

test_that("rules reject unsafe structures and empty event status is stable", {
  rules <- read_ruleset(system.file(
    "extdata", "rules", "adult_outpatient_demo.yml", package = "psyActive"
  ))
  invalid <- rules
  invalid$rules[[1L]]$when <- list(
    field = "x;system()", operator = "eq", value = "anything"
  )
  expect_error(validate_ruleset(invalid), class = "psy_error_schema")

  applied <- apply_rules(
    data.frame(event_id = "event-1", event_type = "missingness_change"),
    ruleset = rules, audit = FALSE
  )
  expect_equal(as.character(applied$alert_level), "watch")
  expect_equal(applied$reason_code, "MISSINGNESS_CHANGE")

  empty <- event_status(data.frame(), data.frame())
  expect_s3_class(empty, "psy_event")
  expect_equal(nrow(empty), 0L)
  expect_equal(length(empty$event_status), 0L)
  expect_s3_class(empty$latest_reviewed_at, "POSIXct")
})
