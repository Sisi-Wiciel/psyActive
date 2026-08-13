# psyActive

`psyActive` provides auditable building blocks for longitudinal,
measurement-based mental health data workflows in R. It standardizes
item-level observations, scores versioned declarative instruments, records
data-quality findings, learns frozen personal baselines, detects transparent
longitudinal events, and records human review decisions.

## Safety boundary

This package is decision-support infrastructure. It does **not** diagnose a
condition, recommend treatment, prescribe, replace a comprehensive suicide
risk assessment, or initiate clinical action. Crisis-related information must
be reviewed by appropriately qualified people under a locally approved
protocol.

Every instrument, reference value, and ruleset bundled with the package is
fictional and marked demo-only. The bundled `demo_mood_9` content is for
software testing and examples only; it is not a validated questionnaire or a
clinical threshold set. Users are responsible for licensing, validation,
governance, privacy, and regulatory review of any real content they add.

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Sisi-Wiciel/psyActive")
```

## Demonstration workflow

The following workflow is fully offline after installation and uses only
fictional data and definitions.

```r
library(psyActive)

instrument_file <- system.file(
  "extdata", "instruments", "demo_mood_9.yml",
  package = "psyActive"
)
reference_file <- system.file(
  "extdata", "references", "demo_mood_9_zh_adult_v1.yml",
  package = "psyActive"
)

observations <- psy_demo_data(n_people = 2, n_assessments = 6)
quality <- assess_quality(observations, checks = c("schema", "duplicates"))
scores <- score_instrument(observations, read_instrument(instrument_file))
interpreted <- interpret_score(scores, read_reference(reference_file))
baseline <- learn_baseline(scores, min_n = 4, method = "mean_sd")

head(interpreted[c("person_id", "observed_at", "score_value",
                   "severity_band")])
baseline
summarize_quality(quality)
```

The synthetic generator is deterministic for a fixed `seed`. Input data should
use pseudonymous identifiers, and direct identifiers should be removed before
conversion with `as_psy_observation()`.

## Declarative definitions

Instrument, interpretation-reference, and workflow-rule definitions are YAML
or JSON. Definitions remain explicit and versioned so that a result can be
traced back to its scoring and decision logic. `register_instrument()` requires
an explicit confirmation before storing content that is not marked
redistributable; that confirmation is not a substitute for obtaining a valid
license.

## Reporting issues

Please report reproducible software problems at
<https://github.com/Sisi-Wiciel/psyActive/issues>. Do not include identifiable
or sensitive health data in issue reports.
