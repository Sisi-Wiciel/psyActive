\name{as_psy_observation}
\alias{as_psy_observation}
\alias{validate_observation}
\title{Create and Validate Standard Longitudinal Observations}
\description{
Convert item-level source data to the psyActive observation contract and
validate either its core schema or its registered instrument definitions.
Direct identifiers should be removed before conversion.
}
\usage{
as_psy_observation(x, mapping, timezone = NULL, source_system,
  strict = TRUE, keep_unmapped = FALSE)

validate_observation(x, registry = NULL,
  level = c("schema", "registered"))
}
\arguments{
  \item{x}{A data frame. For validation, this is normally a
  \code{psy_observation} object.}
  \item{mapping}{A named list whose elements are source column names,
  constants, or vectors. Required standard fields are \code{person_id},
  \code{assessment_id}, \code{observed_at}, \code{instrument_id},
  \code{instrument_version}, \code{item_id}, and \code{record_id}. At least
  one of \code{value_raw}, \code{value_num}, or \code{missing_reason} must be
  mapped.}
  \item{timezone}{A scalar IANA timezone used when timestamps do not already
  carry timezone metadata.}
  \item{source_system}{A non-empty identifier for the source system.}
  \item{strict}{If \code{TRUE}, stop when conversion produces schema errors.
  If \code{FALSE}, return the object and expose findings through
  \code{problems()}.}
  \item{keep_unmapped}{If \code{TRUE}, retain unmapped source columns in an
  attribute.}
  \item{registry}{An optional object returned by \code{psy_registry()} that
  contains registered instrument definitions.}
  \item{level}{Validation level. \code{"schema"} checks the standard data
  contract; \code{"registered"} additionally checks registered versions,
  items, and value ranges.}
}
\value{
\code{as_psy_observation()} returns a \code{psy_observation} data frame with
stable observation identifiers, UTC timestamps, quality findings, and an
audit attribute. \code{validate_observation()} returns a
\code{psy_quality} data frame; zero rows means that no finding was produced.
}
\details{
Identifiers should be pseudonymous. Conversion records both source timezone
and UTC time so that temporal assumptions remain auditable.
}
\examples{
raw <- data.frame(
  id = c("p1", "p1"), assessment = c("a1", "a1"),
  when = c("2026-01-01 09:00:00", "2026-01-01 09:00:00"),
  item = c("dms_1", "dms_2"), value = c(1, 2)
)
obs <- as_psy_observation(
  raw,
  mapping = list(
    person_id = "id", assessment_id = "assessment",
    observed_at = "when", item_id = "item", value_num = "value",
    instrument_id = "demo_mood_9", instrument_version = "1.0.0",
    record_id = "assessment"
  ),
  timezone = "UTC", source_system = "example"
)
validate_observation(obs)
}
