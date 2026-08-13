\name{psyActive-package}
\alias{psyActive}
\alias{psyActive-package}
\docType{package}
\title{Auditable Workflows for Measurement-Based Mental Health Care}
\description{
Tools for standardizing item-level longitudinal observations, applying
versioned scoring definitions, assessing data quality, learning personal
baselines, detecting transparent changes, recording human review, and
producing auditable reports.
}
\details{
The package is decision-support infrastructure. It does not diagnose,
prescribe, recommend treatment, replace a comprehensive suicide risk
assessment, or initiate clinical action. All definitions distributed with the
package are fictional demonstrations and are not validated clinical tools.
Users remain responsible for licensing, validation, privacy, governance, and
appropriate qualified review of real-world workflows.
}
\seealso{
\code{\link{as_psy_observation}}, \code{\link{score_instrument}},
\code{\link{assess_quality}}, and \code{\link{render_psy_report}}.
}
\examples{
library(psyActive)
obs <- psy_demo_data(n_people = 1, n_assessments = 2, seed = 1)
nrow(obs)
}
