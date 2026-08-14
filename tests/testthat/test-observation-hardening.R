hardening_mapping <- function(include_timezone = TRUE) {
  mapping <- list(
    person_id = "person",
    assessment_id = "assessment",
    observed_at = "time",
    instrument_id = "demo_mood_9",
    instrument_version = "1.0.0",
    item_id = "item",
    value_num = "value",
    record_id = "record"
  )
  if (include_timezone) mapping$source_timezone <- "source_tz"
  mapping
}

one_hardening_observation <- function(
    person = "p1", assessment = "a1", time = "2026-01-01 12:00:00",
    item = "dms_1", record = "r1", source_timezone = "UTC",
    timezone = NULL, source_system = "system-a") {
  raw <- data.frame(
    person = person,
    assessment = assessment,
    item = item,
    value = 1,
    record = record,
    stringsAsFactors = FALSE
  )
  raw$time <- time
  mapping <- hardening_mapping(include_timezone = !is.null(source_timezone))
  if (!is.null(source_timezone)) raw$source_tz <- source_timezone
  as_psy_observation(
    raw,
    mapping = mapping,
    timezone = timezone,
    source_system = source_system
  )
}

test_that("observation identifiers cover identity and exact UTC time", {
  raw <- data.frame(
    person = c("p1", "p2"),
    assessment = "same-assessment",
    time = "2026-01-01 12:00:00",
    source_tz = "UTC",
    item = "dms_1",
    value = 1,
    record = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  observations <- as_psy_observation(
    raw,
    mapping = hardening_mapping(),
    source_system = "system-a",
    strict = FALSE
  )
  expect_equal(anyDuplicated(observations$observation_id), 0L)

  base <- one_hardening_observation()
  same <- one_hardening_observation()
  variants <- c(
    one_hardening_observation(person = "p2")$observation_id,
    one_hardening_observation(record = "r2")$observation_id,
    one_hardening_observation(assessment = "a2")$observation_id,
    one_hardening_observation(item = "dms_2")$observation_id,
    one_hardening_observation(source_system = "system-b")$observation_id,
    one_hardening_observation(time = "2026-01-01 12:00:01")$observation_id
  )
  expect_identical(base$observation_id, same$observation_id)
  expect_false(base$observation_id %in% variants)
  expect_equal(anyDuplicated(variants), 0L)

  fractional_a <- one_hardening_observation(
    time = as.POSIXct("2026-01-01 12:00:00", tz = "UTC") + 0.1,
    source_timezone = NULL
  )
  fractional_b <- one_hardening_observation(
    time = as.POSIXct("2026-01-01 12:00:00", tz = "UTC") + 0.2,
    source_timezone = NULL
  )
  expect_false(identical(
    fractional_a$observation_id,
    fractional_b$observation_id
  ))

  instrument_raw <- data.frame(
    person = c("p1", "p1"), assessment = "a1",
    time = "2026-01-01 12:00:00", item = "dms_1", value = 1,
    record = "r1", instrument = c("instrument-a", "instrument-b"),
    version = "1.0.0", source_tz = "UTC", stringsAsFactors = FALSE
  )
  instrument_mapping <- hardening_mapping()
  instrument_mapping$instrument_id <- "instrument"
  instrument_mapping$instrument_version <- "version"
  instrument_observations <- as_psy_observation(
    instrument_raw, instrument_mapping, source_system = "system-a",
    strict = FALSE
  )
  expect_equal(anyDuplicated(instrument_observations$observation_id), 0L)
  expect_false("duplicates" %in% problems(instrument_observations)$check_id)
})

test_that("explicit ISO 8601 offsets are converted as absolute instants", {
  values <- c(
    "2026-01-01T12:00:00+08:00",
    "2026-01-01T12:00:00+0800",
    "2026-01-01T04:00:00Z"
  )
  converted <- psyActive:::as_utc(values)
  expected <- as.POSIXct("2026-01-01 04:00:00", tz = "UTC")
  expect_equal(as.numeric(converted), rep(as.numeric(expected), 3L))
  expect_identical(attr(converted, "tzone"), "UTC")

  ignored_source_zones <- psyActive:::as_utc(
    values,
    c("America/New_York", "Europe/London", "Asia/Shanghai")
  )
  expect_equal(as.numeric(ignored_source_zones), rep(as.numeric(expected), 3L))

  observation <- one_hardening_observation(
    time = "2026-01-01T12:00:00+08:00",
    source_timezone = "Asia/Shanghai"
  )
  expect_equal(as.numeric(observation$observed_at), as.numeric(expected))

  offset_without_mapping <- one_hardening_observation(
    time = "2026-01-01T12:00:00+08:00",
    source_timezone = NULL
  )
  expect_identical(offset_without_mapping$source_timezone, "UTC")
  expect_equal(as.numeric(offset_without_mapping$observed_at), as.numeric(expected))
})

test_that("naive timestamps still use row-specific IANA timezones", {
  values <- c("2026-01-01 12:00:00", "2026-01-01 12:00:00")
  converted <- psyActive:::as_utc(values, c("Asia/Shanghai", "UTC"))
  expected <- as.POSIXct(
    c("2026-01-01 04:00:00", "2026-01-01 12:00:00"),
    tz = "UTC"
  )
  expect_equal(as.numeric(converted), as.numeric(expected))
  expect_error(
    psyActive:::as_utc(values),
    class = "psy_error_schema"
  )
})

test_that("invalid timestamp text and offsets are schema errors", {
  invalid <- c(
    "not-a-time",
    "2026-02-30 12:00:00",
    "2026-01-01 12:00:00 trailing",
    "2026-01-01T12:00:00+24:00",
    "2026-01-01T12:00:00+0860"
  )
  for (value in invalid) {
    expect_error(
      psyActive:::as_utc(value, "UTC"),
      class = "psy_error_schema"
    )
  }
})

test_that("POSIX timezone metadata is preserved when consistent", {
  shanghai <- as.POSIXct("2026-01-01 12:00:00", tz = "Asia/Shanghai")
  inferred <- one_hardening_observation(
    time = shanghai,
    source_timezone = NULL
  )
  explicit <- one_hardening_observation(
    time = shanghai,
    source_timezone = NULL,
    timezone = "Asia/Shanghai"
  )
  mapped <- one_hardening_observation(
    time = shanghai,
    source_timezone = "Asia/Shanghai"
  )
  expect_identical(inferred$source_timezone, "Asia/Shanghai")
  expect_identical(explicit$source_timezone, "Asia/Shanghai")
  expect_identical(mapped$source_timezone, "Asia/Shanghai")
  expect_equal(
    as.numeric(inferred$observed_at),
    as.numeric(as.POSIXct("2026-01-01 04:00:00", tz = "UTC"))
  )

  posixlt <- as.POSIXlt(shanghai, tz = "Asia/Shanghai")
  from_posixlt <- one_hardening_observation(
    time = I(posixlt),
    source_timezone = NULL
  )
  expect_identical(from_posixlt$source_timezone, "Asia/Shanghai")
  expect_equal(as.numeric(from_posixlt$observed_at), as.numeric(shanghai))
})

test_that("conflicting POSIX timezone metadata is rejected", {
  shanghai <- as.POSIXct("2026-01-01 12:00:00", tz = "Asia/Shanghai")
  expect_error(
    one_hardening_observation(
      time = shanghai,
      source_timezone = NULL,
      timezone = "UTC"
    ),
    "timezone conflicts",
    class = "psy_error_schema"
  )
  expect_error(
    one_hardening_observation(
      time = shanghai,
      source_timezone = "UTC"
    ),
    "cannot be reinterpreted",
    class = "psy_error_schema"
  )

  posixlt <- as.POSIXlt(shanghai, tz = "Asia/Shanghai")
  expect_error(
    one_hardening_observation(
      time = I(posixlt),
      source_timezone = NULL,
      timezone = "UTC"
    ),
    "timezone conflicts",
    class = "psy_error_schema"
  )
})
