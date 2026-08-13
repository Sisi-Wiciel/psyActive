\name{psy_change_config}
\alias{psy_change_config}
\alias{learn_baseline}
\alias{reliable_change}
\alias{detect_shift}
\alias{detect_change}
\title{Learn Personal Baselines and Detect Longitudinal Change}
\description{
Create a change-detection configuration, freeze personal baselines, calculate
reliable change using an explicit reference, and detect persistent standardized
shifts.
}
\usage{
psy_change_config(threshold = 2.5, persistence = 2L, confidence = 0.95)

learn_baseline(x, metrics = NULL, window = NULL,
  method = c("median_mad", "mean_sd"), min_n = 4L,
  zero_scale = c("flag", "fallback_iqr", "error"), audit = TRUE)

reliable_change(x, reference,
  from = c("baseline", "previous", "episode_start"), confidence = 0.95,
  direction = c("higher_worse", "higher_better"))

detect_shift(x, baseline, threshold = 2.5, persistence = 2L,
  direction = c("both", "increase", "decrease"), cooldown = 0,
  audit = TRUE)

detect_change(x, baseline = NULL,
  methods = "shift", config = psy_change_config())
}
\arguments{
  \item{threshold}{Absolute standardized shift threshold.}
  \item{persistence}{Number of consecutive threshold-crossing observations
  required.}
  \item{confidence}{Confidence level used for reliable-change thresholds.}
  \item{x}{A \code{psy_score} object, or metric data containing
  \code{person_id}, \code{metric_id}, \code{value}, and
  \code{observed_at} where supported. For \code{reliable_change()}, score
  data must also include \code{instrument_id} and \code{instrument_version}.}
  \item{metrics}{Optional metric identifier subset.}
  \item{window}{Optional length-two time window used to freeze baseline data.}
  \item{method}{Baseline center and scale estimator.}
  \item{min_n}{Minimum finite observations per person and metric.}
  \item{zero_scale}{Policy when the baseline has zero variance.}
  \item{audit}{If \code{TRUE}, attach an audit record.}
  \item{reference}{A \code{psy_reference} object or YAML/JSON reference path
  containing reliability and reference standard deviation.}
  \item{from}{Comparison origin. \code{"previous"} uses the preceding finite
  observation, \code{"baseline"} uses the first finite observation in each
  person-score series, and \code{"episode_start"} uses the first finite
  observation in each \code{episode_id}.}
  \item{direction}{For reliable change, direction in which higher scores move.
  For shift detection, which standardized direction should trigger an event.}
  \item{baseline}{A \code{psy_baseline} object matched by person and metric.}
  \item{cooldown}{Number of subsequent rows in the same person-metric series
  during which another shift event is suppressed.}
  \item{methods}{Methods to request. The default and currently orchestrated
  method is \code{"shift"}. Reliable-change calculations require an explicit
  reference and are available through \code{reliable_change()}.}
  \item{config}{A list returned by \code{psy_change_config()}.}
}
\value{
\code{psy_change_config()} returns a configuration list.
\code{learn_baseline()} returns a \code{psy_baseline} data frame.
\code{reliable_change()} returns score rows with change, reliable change index,
and classification columns. \code{detect_shift()} and \code{detect_change()}
return \code{psy_event} data frames.
}
\details{
Events are transparent workflow signals for qualified human review. They do
not constitute diagnoses, treatment recommendations, or crisis-risk
classifications. Reference validity and population fit are user
responsibilities.
}
\examples{
metric <- data.frame(
  person_id = rep("demo-p01", 6), metric_id = rep("demo:total", 6),
  observed_at = as.POSIXct("2026-01-01", tz = "UTC") + 0:5 * 86400,
  value = c(0, 1, 0, 1, 5, 6)
)
config <- psy_change_config(threshold = 2, persistence = 2)
baseline <- learn_baseline(metric, window = range(metric$observed_at[1:4]),
  method = "mean_sd", min_n = 4, audit = FALSE)
events <- detect_shift(metric, baseline, threshold = config$threshold,
  persistence = config$persistence, audit = FALSE)
events
detect_change(metric, baseline, methods = "shift", config = config)

ref_file <- system.file("extdata", "references",
  "demo_mood_9_zh_adult_v1.yml", package = "psyActive")
score_rows <- data.frame(
  person_id = rep("demo-p01", 3), score_name = rep("total", 3),
  instrument_id = rep("demo_mood_9", 3),
  instrument_version = rep("1.0.0", 3),
  observed_at = as.POSIXct("2026-02-01", tz = "UTC") + 0:2 * 86400,
  score_value = c(4, 5, 12)
)
reliable_change(score_rows, read_reference(ref_file), from = "previous")
}
