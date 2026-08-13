registry_fixture_instrument <- function() {
  read_instrument(system.file(
    "extdata", "instruments", "demo_mood_9.yml", package = "psyActive"
  ))
}

registry_fixture_reference <- function() {
  read_reference(system.file(
    "extdata", "references", "demo_mood_9_zh_adult_v1.yml",
    package = "psyActive"
  ))
}

test_that("registry IDs and semantic versions are validated before storage", {
  root <- tempfile("psyactive-registry-security-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  registry <- psy_registry(file.path(root, "registry"))

  bad_instrument <- registry_fixture_instrument()
  bad_instrument$instrument_id <- "../../escaped"
  expect_error(
    register_instrument(bad_instrument, registry),
    class = "psy_error_instrument"
  )
  expect_false(file.exists(file.path(root, "escaped__1.0.0.rds")))

  bad_version <- registry_fixture_instrument()
  bad_version$version <- "../../escaped"
  expect_error(
    register_instrument(bad_version, registry),
    class = "psy_error_instrument"
  )

  bad_reference <- registry_fixture_reference()
  bad_reference$reference_id <- "../../escaped"
  expect_error(
    register_reference(bad_reference, registry),
    class = "psy_error_reference"
  )
  expect_false(file.exists(file.path(root, "escaped.rds")))

  bad_reference <- registry_fixture_reference()
  bad_reference$instrument_id <- "../escaped"
  expect_error(
    register_reference(bad_reference, registry),
    class = "psy_error_reference"
  )

  bad_reference <- registry_fixture_reference()
  bad_reference$instrument_version <- "1.0"
  expect_error(
    register_reference(bad_reference, registry),
    class = "psy_error_reference"
  )
})

test_that("registry destinations remain contained in registry directories", {
  root <- tempfile("psyactive-registry-containment-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  registry <- psy_registry(file.path(root, "registry"))
  outside <- file.path(root, "outside")
  dir.create(outside)

  tampered <- registry
  tampered$instruments <- outside
  instrument <- registry_fixture_instrument()
  expect_error(
    register_instrument(instrument, tampered),
    class = "psy_error_instrument"
  )
  expect_false(file.exists(file.path(outside, "demo_mood_9__1.0.0.rds")))

  tampered <- registry
  tampered$references <- outside
  reference <- registry_fixture_reference()
  expect_error(
    register_reference(reference, tampered),
    class = "psy_error_reference"
  )
  expect_false(file.exists(file.path(
    outside, "demo_mood_9_zh_adult_v1.rds"
  )))
})

test_that("instrument lookup treats valid metacharacters literally", {
  root <- tempfile("psyactive-registry-literal-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  registry <- psy_registry(root)

  dotted <- registry_fixture_instrument()
  dotted$instrument_id <- "literal.one"
  dotted$version <- "1.2.3-rc.1+build.5"
  similar <- registry_fixture_instrument()
  similar$instrument_id <- "literalXone"

  dotted_path <- register_instrument(dotted, registry)
  register_instrument(similar, registry)
  stored <- psyActive:::get_instrument("literal.one", registry)

  expect_true(file.exists(dotted_path))
  expect_identical(dirname(dotted_path), registry$instruments)
  expect_identical(stored$instrument_id, "literal.one")
  expect_identical(stored$version, "1.2.3-rc.1+build.5")
  expect_error(
    psyActive:::get_instrument("literal.*", registry),
    class = "psy_error_instrument"
  )

  reference <- registry_fixture_reference()
  reference$reference_id <- "reference.one"
  reference$instrument_id <- dotted$instrument_id
  reference$instrument_version <- dotted$version
  reference_path <- register_reference(reference, registry)
  stored_reference <- psyActive:::get_reference("reference.one", registry)

  expect_true(file.exists(reference_path))
  expect_identical(dirname(reference_path), registry$references)
  expect_identical(stored_reference$reference_id, "reference.one")
  expect_identical(stored_reference$instrument_id, "literal.one")
})

test_that("path traversal strings are never treated as registry lookups", {
  root <- tempfile("psyactive-registry-lookup-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  registry <- psy_registry(root)

  expect_error(
    psyActive:::get_instrument("../../escaped", registry),
    class = "psy_error_instrument"
  )
  expect_error(
    psyActive:::get_reference("../../escaped", registry),
    class = "psy_error_reference"
  )
})
