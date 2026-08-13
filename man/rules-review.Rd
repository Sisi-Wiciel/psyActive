\name{apply_rules}
\alias{apply_rules}
\alias{record_review}
\alias{event_status}
\title{Apply Workflow Rules and Record Human Review}
\description{
Apply versioned declarative rules to event rows, record a structured human
review, and derive current event status from review history.
}
\usage{
apply_rules(events, scores = NULL, quality = NULL, ruleset,
  as_of = Sys.time(), audit = TRUE)

record_review(event, reviewer_id, disposition, action_code = NULL,
  rationale_code = NULL, next_review_at = NULL,
  reviewed_at = Sys.time())

event_status(events, reviews)
}
\arguments{
  \item{events}{Event data.}
  \item{scores}{Optional score data reserved as workflow context.}
  \item{quality}{Optional quality data reserved as workflow context.}
  \item{ruleset}{A \code{psy_ruleset} object or YAML/JSON path.}
  \item{as_of}{Evaluation timestamp recorded in the audit configuration.}
  \item{audit}{If \code{TRUE}, attach an audit record.}
  \item{event}{One event row or an event identifier.}
  \item{reviewer_id}{Pseudonymous local reviewer identifier.}
  \item{disposition}{One of \code{"confirmed"},
  \code{"not_concerning"}, \code{"insufficient_data"},
  \code{"duplicate"}, or \code{"escalated"}.}
  \item{action_code}{Optional institution-defined action code.}
  \item{rationale_code}{Optional structured rationale code.}
  \item{next_review_at}{Optional next-review timestamp.}
  \item{reviewed_at}{Review timestamp.}
  \item{reviews}{Review-history data, normally rows returned by
  \code{record_review()}.}
}
\value{
\code{apply_rules()} returns a \code{psy_event} object with applicable alert,
reason, and rule-version fields. \code{record_review()} returns a one-row
\code{psy_review} object. \code{event_status()} returns the events with derived
status and latest-review fields.
}
\details{
Rules coordinate review; they must not be used as autonomous diagnosis,
prescribing, or crisis-response logic. Bundled rules are fictional and
demo-only. The \code{scores} and \code{quality} arguments are retained for
workflow compatibility but the current evaluator applies rules to event-row
fields.
}
\examples{
events <- data.frame(
  event_id = "event-demo-1", person_id = "demo-p01",
  event_time = as.POSIXct("2026-02-01", tz = "UTC"),
  detected_at = as.POSIXct("2026-02-01", tz = "UTC"),
  event_type = "sustained_shift", metric_id = "demo_mood_9:total",
  observed_value = 10, expected_value = 5, change_value = 5,
  alert_level = "watch", reason_code = "UNREVIEWED",
  rule_id = NA_character_, rule_version = NA_character_,
  event_status = "open", stringsAsFactors = FALSE
)
rule_file <- system.file("extdata", "rules", "adult_outpatient_demo.yml",
  package = "psyActive")
prioritized <- apply_rules(events, ruleset = read_ruleset(rule_file),
  audit = FALSE)
review <- record_review(prioritized, reviewer_id = "reviewer-01",
  disposition = "confirmed",
  reviewed_at = as.POSIXct("2026-02-02", tz = "UTC"))
event_status(prioritized, review)
}
