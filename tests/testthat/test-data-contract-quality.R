observation_mapping <- function() {
  list(
    person_id = "person", assessment_id = "assessment", observed_at = "time",
    source_timezone = "tz", instrument_id = "demo_mood_9",
    instrument_version = "1.0.0", item_id = "item", value_num = "value",
    record_id = "record"
  )
}

test_that("quality constructors do not recurse and append nested results", {
  quality <- psyActive:::new_quality()
  expect_s3_class(quality, "psy_quality")
  expect_equal(nrow(quality), 0L)

  one <- psyActive:::make_quality("one", "error", message = "first")
  two <- psyActive:::make_quality("two", "warning", message = "second")
  combined <- psyActive:::append_quality(list(one, list(NULL, two)))
  expect_s3_class(combined, "psy_quality")
  expect_equal(combined$check_id, c("one", "two"))
  expect_equal(ncol(combined), 6L)
})

test_that("required mappings and naive timestamp timezones are explicit", {
  raw <- data.frame(
    person = "p1", assessment = "a1", time = "2026-01-01 12:00:00",
    tz = "Asia/Shanghai", item = "dms_1", value = 2, record = "r1"
  )
  mapping <- observation_mapping()
  expect_error(
    as_psy_observation(raw, mapping[names(mapping) != "record_id"], source_system = "test"),
    "missing required field"
  )
  expect_error(
    as_psy_observation(raw, mapping[names(mapping) != "source_timezone"], source_system = "test"),
    "source timezone is required"
  )
})

test_that("source_timezone columns convert each naive timestamp correctly", {
  raw <- data.frame(
    person = c("p1", "p1"), assessment = c("a1", "a1"),
    time = c("2026-01-01 12:00:00", "2026-01-01 12:00:00"),
    tz = c("Asia/Shanghai", "UTC"), item = c("dms_1", "dms_2"),
    value = c(2, 3), record = c("r1", "r2")
  )
  observation <- as_psy_observation(
    raw, observation_mapping(), source_system = "test"
  )
  expect_equal(
    format(observation$observed_at, tz = "UTC"),
    c("2026-01-01 04:00:00", "2026-01-01 12:00:00")
  )
  expect_equal(observation$source_timezone, raw$tz)
  expect_error(
    psyActive:::as_utc(c("2026-01-01 12:00:00", "not-a-time"), c("UTC", "UTC")),
    "could not be parsed"
  )
})

test_that("registered validation checks items and ranges", {
  registry <- psy_registry(file.path(tempdir(), paste0("registry-", Sys.getpid())))
  definition <- system.file(
    "extdata", "instruments", "demo_mood_9.yml", package = "psyActive"
  )
  register_instrument(definition, registry, confirm_license = TRUE, overwrite = TRUE)
  raw <- data.frame(
    person = c("p1", "p1"), assessment = c("a1", "a1"),
    time = c("2026-01-01 12:00:00", "2026-01-01 12:00:00"),
    tz = c("UTC", "UTC"), item = c("dms_1", "not_registered"),
    value = c(9, 1), record = c("r1", "r2")
  )
  observation <- as_psy_observation(
    raw, observation_mapping(), source_system = "test", strict = FALSE
  )
  quality <- validate_observation(observation, registry, level = "registered")
  expect_true(all(c("range", "unregistered_item") %in% quality$check_id))
})

test_that("quality groups use the complete assessment key", {
  raw <- data.frame(
    person = rep(c("p1", "p2"), each = 4), assessment = "same-id",
    time = "2026-01-01 12:00:00", tz = "UTC",
    item = rep(paste0("dms_", 1:4), 2),
    value = c(rep(1, 4), c(0, 1, 2, 3)), record = paste0("r", 1:8)
  )
  observation <- as_psy_observation(
    raw, observation_mapping(), source_system = "test", strict = FALSE
  )
  quality <- assess_quality(observation, checks = "straightlining")
  expect_equal(sum(quality$check_id == "straightlining"), 1L)
  expect_match(quality$row_id[quality$check_id == "straightlining"], "p1\\|same-id")
})

test_that("default checks report checks that cannot run", {
  quality <- assess_quality(psy_demo_data())
  expect_true(all(c("range", "timing") %in% quality$check_id))
  expect_true(all(quality$severity[quality$check_id %in% c("range", "timing")] == "info"))
})

test_that("duplicate observation checks distinguish people and source records", {
  raw <- data.frame(
    person = c("p1", "p2", "p1"), assessment = c("same-id", "same-id", "same-id"),
    time = "2026-01-01 12:00:00", tz = "UTC",
    item = c("dms_1", "dms_1", "dms_1"), value = c(1, 2, 1),
    record = c("r1", "r2", "r1")
  )
  observation <- as_psy_observation(
    raw, observation_mapping(), source_system = "test", strict = FALSE
  )
  quality <- validate_observation(observation[1:2, , drop = FALSE])
  expect_false(any(quality$check_id == "duplicates"))

  duplicate <- observation[c(1L, 3L), , drop = FALSE]
  quality <- validate_observation(duplicate)
  expect_true(any(quality$check_id == "duplicates"))
})
