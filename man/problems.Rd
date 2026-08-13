\name{problems}
\alias{problems}
\alias{problems.default}
\alias{problems.psy_observation}
\alias{problems.psy_score}
\title{Retrieve Attached Quality Findings}
\description{
Return the structured quality findings attached to a psyActive object.
}
\usage{
problems(x)

\method{problems}{default}(x)

\method{problems}{psy_observation}(x)

\method{problems}{psy_score}(x)
}
\arguments{
  \item{x}{An object, normally a \code{psy_observation} or
  \code{psy_score}.}
}
\value{A \code{psy_quality} data frame. If no findings are attached, an empty
object with the standard quality columns is returned.}
\examples{
obs <- psy_demo_data(n_people = 1, n_assessments = 1, seed = 2)
problems(obs)
problems(unclass(obs))
}
