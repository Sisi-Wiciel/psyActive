baseline_metrics <- function(values, person_id = "person-1",
                             metric_id = "mood") {
  data.frame(
    person_id = person_id,
    metric_id = metric_id,
    value = values,
    observed_at = as.POSIXct("2026-01-01", tz = "UTC") +
      86400 * seq_along(values),
    stringsAsFactors = FALSE
  )
}

test_that("baseline learning marks samples below min_n as insufficient", {
  baseline <- learn_baseline(
    baseline_metrics(c(1, 2, 3)), min_n = 4L, audit = FALSE
  )

  expect_equal(baseline$n_obs, 3L)
  expect_equal(baseline$status, "insufficient")
  expect_true(is.na(baseline$center))
  expect_true(is.na(baseline$scale))
})

test_that("zero-scale policies flag constant baselines", {
  baseline <- learn_baseline(
    baseline_metrics(rep(5, 4)), min_n = 4L,
    zero_scale = "flag", audit = FALSE
  )

  expect_equal(baseline$center, 5)
  expect_true(is.na(baseline$scale))
  expect_equal(baseline$status, "zero_variance")
})

test_that("fallback IQR still flags a zero scale", {
  values <- c(0, 0, 0, 0, 1)
  baseline <- learn_baseline(
    baseline_metrics(values), min_n = length(values),
    zero_scale = "fallback_iqr", audit = FALSE
  )

  expect_equal(stats::mad(values, constant = 1.4826), 0)
  expect_equal(stats::IQR(values), 0)
  expect_true(is.na(baseline$scale))
  expect_equal(baseline$status, "zero_variance")
})

test_that("fallback IQR rescues a zero MAD when the IQR is positive", {
  values <- c(0, 0, 0, 1, 2)
  baseline <- learn_baseline(
    baseline_metrics(values), min_n = length(values),
    zero_scale = "fallback_iqr", audit = FALSE
  )

  expect_equal(stats::mad(values, constant = 1.4826), 0)
  expect_gt(stats::IQR(values), 0)
  expect_equal(baseline$status, "ok")
  expect_gt(baseline$scale, 0)
  expect_equal(baseline$scale, stats::IQR(values) / 1.349)
})

test_that("zero_scale error uses the insufficient-data condition class", {
  expect_error(
    learn_baseline(
      baseline_metrics(rep(5, 4)), min_n = 4L,
      zero_scale = "error", audit = FALSE
    ),
    class = "psy_error_insufficient_data"
  )
})

test_that("non-ok baselines cannot trigger shift events", {
  observations <- rbind(
    baseline_metrics(100, person_id = "insufficient"),
    baseline_metrics(100, person_id = "zero-variance")
  )
  baseline <- data.frame(
    person_id = c("insufficient", "zero-variance"),
    metric_id = "mood",
    center = 0,
    scale = 1,
    status = c("insufficient", "zero_variance"),
    stringsAsFactors = FALSE
  )

  events <- detect_shift(
    observations, baseline, threshold = 2.5, persistence = 1L,
    audit = FALSE
  )

  expect_s3_class(events, "psy_event")
  expect_equal(nrow(events), 0L)
})
