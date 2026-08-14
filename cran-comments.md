## Test environments

* Ubuntu 26.04 LTS, R 4.5.2 (2025-10-31), x86_64-pc-linux-gnu,
  `R CMD check --as-cran --no-manual`
* Ubuntu 26.04 LTS, isolated source build of R 4.6.1 (2026-06-24),
  x86_64-pc-linux-gnu, `R CMD check --as-cran --no-manual`

## R CMD check results

The R 4.5.2 check reported:

* 0 errors | 0 warnings | 2 notes
* One note is the expected `New submission` note.
* One environment-specific note reports that the local machine was unable to
  verify the current time. The package contains no future-dated files.

The isolated R 4.6.1 check, with Pandoc 3.7.0.2 available, reported:

* 0 errors | 0 warnings | 1 note
* The only note is the expected `New submission` note.
* The testthat suite reported 242 passes, 0 failures, 0 warnings, and 0 skips.

Both checks used the exact same source tarball and completed the testthat suite
with 242 passes, 0 failures, 0 warnings, and 0 skips.

Source tarball checked: `psyActive_0.1.0.tar.gz`
SHA-256: `70e43d0dff9c492b9cbb40945e79f8fc7559f7d10ed476c740e5b00f646b6df6`

## Maintainer

The creator and maintainer address in `DESCRIPTION` is an institutional
correspondence address. The maintainer must be able to receive and respond to
CRAN confirmation messages at that address.

## Safety and bundled content

The package supports auditable research and measurement-based care workflows.
It does not diagnose, prescribe, recommend treatment, replace a comprehensive
suicide risk assessment, or initiate clinical action. All bundled instrument,
reference, and rule definitions are fictional, labelled demo-only, and use
synthetic values. They are included solely to test the software workflow.

## Notes for first submission

This is a new submission. There are no reverse dependencies.
