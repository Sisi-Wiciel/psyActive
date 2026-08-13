\name{print.psy_observation}
\alias{print.psy_observation}
\alias{print.psy_score}
\alias{print.psy_quality}
\alias{print.psy_baseline}
\alias{print.psy_event}
\alias{print.psy_review}
\alias{print.psy_instrument}
\alias{print.psy_reference}
\alias{print.psy_ruleset}
\alias{print.psy_registry}
\title{Print psyActive Objects}
\description{Compact print methods for psyActive data frames and declarative
definition objects.}
\usage{
\method{print}{psy_observation}(x, ...)
\method{print}{psy_score}(x, ...)
\method{print}{psy_quality}(x, ...)
\method{print}{psy_baseline}(x, ...)
\method{print}{psy_event}(x, ...)
\method{print}{psy_review}(x, ...)
\method{print}{psy_instrument}(x, ...)
\method{print}{psy_reference}(x, ...)
\method{print}{psy_ruleset}(x, ...)
\method{print}{psy_registry}(x, ...)
}
\arguments{
  \item{x}{A psyActive object of the indicated class.}
  \item{...}{Additional arguments reserved for method compatibility.}
}
\value{The input object, invisibly.}
\examples{
obs <- psy_demo_data(n_people = 1, n_assessments = 1, seed = 6)
print(obs)
print(psy_registry(file.path(tempdir(), "psyactive-print-registry")))
}
