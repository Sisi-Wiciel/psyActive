\name{score_instrument}
\alias{score_instrument}
\alias{explain_score}
\alias{interpret_score}
\title{Score and Interpret Versioned Instruments}
\description{
Apply an explicit declarative scoring definition, inspect item-level scoring
provenance, and optionally classify results using an explicit reference.
}
\usage{
score_instrument(x, instrument, registry = psy_registry(), scores = NULL,
  on_invalid = c("error", "exclude", "keep_na"), audit = TRUE)

explain_score(x, score_id = NULL)

interpret_score(x, reference, registry = psy_registry(),
  mismatch = c("error", "warn", "allow"))
}
\arguments{
  \item{x}{For \code{score_instrument()}, standardized item observations. For
  the other functions, a \code{psy_score} object.}
  \item{instrument}{A \code{psy_instrument}, a YAML/JSON definition path, or
  the unique identifier of an instrument stored in \code{registry}.}
  \item{registry}{A registry returned by \code{psy_registry()}.}
  \item{scores}{Optional character vector selecting score names from the
  instrument definition.}
  \item{on_invalid}{Policy for unknown items or values outside the definition:
  stop, exclude invalid rows, or turn invalid numeric values into missing
  values.}
  \item{audit}{If \code{TRUE}, attach an audit record.}
  \item{score_id}{Optional score identifier or vector of identifiers used to
  filter the explanation.}
  \item{reference}{A \code{psy_reference}, a YAML/JSON path, or a registered
  reference identifier.}
  \item{mismatch}{Policy when the reference instrument or version differs
  from the scored data. Allowing a mismatch is explicit but does not establish
  that the reference is appropriate.}
}
\value{
\code{score_instrument()} returns a \code{psy_score} data frame with one row
per assessment and requested score. If the input contains \code{episode_id},
the output also contains the validated assessment-level episode identifier.
\code{explain_score()} returns item-level inputs and transformations.
\code{interpret_score()} returns a \code{psy_score} with reference,
severity-band, minimal-change, and status columns.
}
\details{
The per-score \code{input_hash} covers a canonical, row-order-independent
representation of the matched observation IDs (when supplied), item IDs, raw
and numeric inputs, values remaining after the invalid-value policy,
transformed scoring values, and the instrument ID and version. The attached
audit record uses the same canonical scoring material together with all
matched observations. Consequently, changing a scoring input changes the
corresponding hash even when observation IDs are unchanged.

If \code{x} contains \code{episode_id}, every person, assessment, instrument,
and instrument-version group must either use one non-blank episode ID on every
row or have \code{NA} on every row. A single ID or the all-missing value is
propagated to each score row. Mixed missing and non-missing IDs, blank IDs, and
conflicting IDs are schema errors. Non-missing propagated IDs can be used
directly with \code{reliable_change(from = "episode_start")}; all-missing
episode IDs remain explicit and are rejected by that comparison mode.

Interpretation bands are descriptive outputs of the selected definition. They
are not diagnoses or treatment recommendations. Bundled definitions and
reference values are fictional demonstrations only.
}
\examples{
inst_file <- system.file("extdata", "instruments", "demo_mood_9.yml",
  package = "psyActive")
ref_file <- system.file("extdata", "references",
  "demo_mood_9_zh_adult_v1.yml", package = "psyActive")
obs <- psy_demo_data(n_people = 1, n_assessments = 2, seed = 3)
scores <- score_instrument(obs, read_instrument(inst_file), audit = FALSE)
explain_score(scores, score_id = scores$score_id[1])
interpreted <- interpret_score(scores, read_reference(ref_file))
interpreted[c("score_value", "severity_band", "interpretation_status")]
}
