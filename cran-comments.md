## Test environments

* Local Ubuntu 26.04, R 4.5.2, `R CMD check --as-cran`
* Additional CRAN platform checks: pending

## R CMD check results

Local `R CMD check --no-manual --no-vignettes` result:

* 0 errors | 0 warnings | 0 notes

Local `R CMD check --as-cran --no-manual` result:

* 0 errors | 0 warnings | 2 notes
* Incoming feasibility reports that this is a new submission.
* Pandoc was unavailable, so `README.md` and `NEWS.md` rendering was not
  checked.

Source tarball checked: `psyActive_0.1.0.tar.gz`
SHA-256: `ca7e83e0299a403a828445fe62c600c330659045ce75504ac7558df13ac4e0c6`

The Markdown rendering note should be rechecked on a CRAN-like service with
Pandoc before submission.

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
