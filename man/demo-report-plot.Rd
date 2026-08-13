\name{psy_demo_data}
\alias{psy_demo_data}
\alias{plot_trajectory}
\alias{render_psy_report}
\title{Generate Demonstration Data, Plots, and Reports}
\description{
Generate deterministic fictional observations, plot a score trajectory, and
write a self-contained auditable HTML report plus machine-readable manifest.
}
\usage{
psy_demo_data(n_people = 3L, n_assessments = 8L, seed = 20260813L)

plot_trajectory(x, person_id, metric, baseline = NULL, events = NULL,
  reference = NULL, timezone = NULL)

render_psy_report(data, person_id = NULL,
  type = c("patient", "cohort"),
  audience = c("clinician", "researcher"), language = c("zh-CN", "en"),
  format = c("html", "pdf"), output_dir = tempdir(), template = NULL,
  include_audit = TRUE, quiet = TRUE)
}
\arguments{
  \item{n_people}{Number of fictional people.}
  \item{n_assessments}{Number of assessments per fictional person.}
  \item{seed}{Random seed for deterministic demonstration values.}
  \item{x}{Score data.}
  \item{person_id}{For plotting, the pseudonymous person to display. For
  reporting, an optional person filter.}
  \item{metric}{Score name or full \code{instrument:score} identifier.}
  \item{baseline}{Optional personal-baseline data.}
  \item{events}{Optional event data. Reserved for plot annotations.}
  \item{reference}{Optional reference. Reserved for plot annotations.}
  \item{timezone}{Optional display timezone. Reserved for display formatting.}
  \item{data}{Named list with elements \code{scores}, \code{quality},
  \code{baseline}, \code{events}, and \code{reviews}. Missing sections can be
  supplied as empty data frames.}
  \item{type}{Patient-level or cohort report label.}
  \item{audience}{Clinician or researcher report label.}
  \item{language}{Report language.}
  \item{format}{Output format. Core functionality guarantees HTML; requesting
  PDF stops with instructions to configure an external workflow.}
  \item{output_dir}{Directory in which report artifacts are written.}
  \item{template}{Reserved custom template argument.}
  \item{include_audit}{If \code{TRUE}, also write attached audit records to
  CSV.}
  \item{quiet}{If \code{FALSE}, announce the report path.}
}
\value{
\code{psy_demo_data()} returns a \code{psy_observation} object containing only
fictional records. \code{plot_trajectory()} returns a \code{ggplot} object and
requires the suggested package \pkg{ggplot2}. \code{render_psy_report()}
returns named paths to the report, JSON manifest, and optional audit CSV.
}
\details{
All generated records, instrument content, thresholds, and reference values
used in demonstrations are synthetic. Reports are decision-support artifacts,
not diagnoses or treatment recommendations. The \code{events},
\code{reference}, \code{timezone}, and \code{template} arguments are reserved
for richer rendering in future versions and do not currently change the core
output where noted.
}
\examples{
obs <- psy_demo_data(n_people = 1, n_assessments = 2, seed = 5)
nrow(obs)

inst_file <- system.file("extdata", "instruments", "demo_mood_9.yml",
  package = "psyActive")
scores <- score_instrument(obs, read_instrument(inst_file), audit = FALSE)

\donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot_trajectory(scores, "demo-p01", "total")
}
}

report_data <- list(
  scores = scores, quality = data.frame(), baseline = data.frame(),
  events = data.frame(), reviews = data.frame()
)
paths <- render_psy_report(report_data, language = "en",
  output_dir = tempdir(), include_audit = FALSE)
file.exists(paths)
}
