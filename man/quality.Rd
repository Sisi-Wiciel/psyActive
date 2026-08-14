\name{psy_quality_config}
\alias{psy_quality_config}
\alias{assess_quality}
\alias{summarize_quality}
\alias{plot_quality}
\title{Assess and Summarize Observation Quality}
\description{
Configure and run structured data-quality checks, summarize their findings,
and optionally display finding counts.
}
\usage{
psy_quality_config(completion_min = 0.8, timing_min_seconds = NULL,
  timing_max_seconds = NULL)

assess_quality(x, registry = NULL,
  checks = c("schema", "range", "duplicates", "time_order", "completion",
    "straightlining", "timing"),
  config = psy_quality_config())

summarize_quality(x, by = c("check_id", "severity"))

plot_quality(x, type = c("overview", "missingness", "compliance"))
}
\arguments{
  \item{completion_min}{Minimum fraction of expected items considered
  complete, between zero and one.}
  \item{timing_min_seconds}{Optional non-negative minimum response duration.}
  \item{timing_max_seconds}{Optional non-negative maximum response duration.}
  \item{x}{For assessment, observation data. For summary and plotting, a
  \code{psy_quality} object.}
  \item{registry}{Optional instrument registry used for registered-schema,
  value-range, and expected-item checks.}
  \item{checks}{Character vector selecting supported quality checks.}
  \item{config}{A configuration returned by \code{psy_quality_config()}.}
  \item{by}{Columns used to group findings.}
  \item{type}{Plot view. The current implementation uses the same aggregate
  finding-count display for the documented views.}
}
\value{
\code{psy_quality_config()} returns a configuration list.
\code{assess_quality()} returns a \code{psy_quality} data frame.
\code{summarize_quality()} returns grouped finding counts.
\code{plot_quality()} returns a \code{ggplot} object and requires the
suggested package \pkg{ggplot2}.
}
\details{
Quality findings are data-review signals. Straightlining, timing, and
completion findings must not be interpreted as diagnoses or autonomous
clinical events.
}
\examples{
obs <- psy_demo_data(n_people = 1, n_assessments = 2, seed = 4)
config <- psy_quality_config(completion_min = 0.8)
quality <- assess_quality(obs,
  checks = c("schema", "duplicates", "time_order"), config = config)
summarize_quality(quality)

\donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- plot_quality(quality)
}
}
}
