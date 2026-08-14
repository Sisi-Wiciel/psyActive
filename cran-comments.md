## Test environments

* Ubuntu 26.04 LTS, R 4.5.2 (2025-10-31), x86_64-pc-linux-gnu,
  `R CMD check --as-cran --no-manual`
* Ubuntu 26.04 LTS, isolated source build of R 4.6.1 (2026-06-24),
  x86_64-pc-linux-gnu, `R CMD check --as-cran --no-manual`
* GitHub Actions, R release on `ubuntu-latest`, `macos-latest`, and
  `windows-latest`, `R CMD check --as-cran --no-manual`
* GitHub Actions, R release on `ubuntu-latest` with TinyTeX,
  full `R CMD check --as-cran` including the PDF manual

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

The source tarball was created with R 4.6.1. Both local checks used that exact
same tarball and completed the testthat suite with 242 passes, 0 failures,
0 warnings, and 0 skips.

The GitHub Actions release matrix completed successfully on Ubuntu, macOS, and
Windows. Each platform reported `Status: OK` and 242 testthat passes. The
separate TinyTeX job reported `checking PDF version of manual ... OK` and 242
testthat passes. Its only note was environment-specific: HTML validation was
skipped because HTML Tidy and the R package `V8` were unavailable.

Source tarball checked: `psyActive_0.1.0.tar.gz`
SHA-256: `4f3905a6acc108ec15babb767b5b6acc3b05144ef65bc65569dc1891df6358bf`

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
