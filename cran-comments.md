## Test environments

* Local Ubuntu 26.04, R 4.5.2, `R CMD check --as-cran`
* Additional CRAN platform checks: pending

## R CMD check results

Local `R CMD check --no-manual --no-vignettes` result:

* 0 errors | 0 warnings | 0 notes

Local `R CMD check --as-cran --no-manual` result:

* 0 errors | 0 warnings | 3 notes
* Incoming feasibility and URL checks could not access CRAN, Bioconductor, or
  GitHub because the check environment had no DNS/network access.
* The check environment could not verify its current time.
* Pandoc was unavailable, so `README.md` and `NEWS.md` rendering was not
  checked.

These environment notes should be rechecked on a networked CRAN-like service
with Pandoc before submission.

## Submission blocker

`DESCRIPTION` currently uses `maintainer@example.org` for the creator and
maintainer. This is intentionally not guessed or replaced automatically. A
real, monitored email address controlled by the maintainer must replace this
placeholder before any CRAN submission. The final maintainer must also confirm
that the address, copyright holder, package URL, and contributor roles are
accurate.

## Safety and bundled content

The package supports auditable research and measurement-based care workflows.
It does not diagnose, prescribe, recommend treatment, replace a comprehensive
suicide risk assessment, or initiate clinical action. All bundled instrument,
reference, and rule definitions are fictional, labelled demo-only, and use
synthetic values. They are included solely to test the software workflow.

## Notes for first submission

This is a new submission. There are no reverse dependencies.
