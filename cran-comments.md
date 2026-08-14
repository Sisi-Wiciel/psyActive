## Test environments

* Ubuntu 26.04 LTS, R 4.5.2 (2025-10-31), x86_64-pc-linux-gnu,
  `R CMD check --as-cran --no-manual`
* Ubuntu 26.04 LTS, isolated source build of R 4.6.1 (2026-06-24),
  x86_64-pc-linux-gnu, `R CMD check --as-cran --no-manual`

## R CMD check results

The R 4.5.2 check reported:

* 0 errors | 0 warnings | 2 notes
* Incoming feasibility reports that this is a new submission.
* Pandoc was unavailable, so `README.md` and `NEWS.md` rendering was not
  checked.

The isolated R 4.6.1 check, with Pandoc 3.7.0.2 available, reported:

* 0 errors | 0 warnings | 1 note
* The only note is the expected `New submission` note.
* The testthat suite reported 213 passes, 0 failures, 0 warnings, and 0 skips.

Source tarball checked: `psyActive_0.1.0.tar.gz`
SHA-256: `3fd1a1787ea038862ce4107f4af9a8ce0576b14251d372c18f6f917cb785d730`

The R 4.6.1 result was obtained from an independent build of the same source
commit. Its source tree was byte-for-byte identical to the delivery tarball
apart from the build-generated `Packaged` timestamp in `DESCRIPTION`.

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
