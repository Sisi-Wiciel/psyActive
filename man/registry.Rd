\name{psy_registry}
\alias{psy_registry}
\alias{read_instrument}
\alias{register_instrument}
\alias{list_instruments}
\alias{read_reference}
\alias{register_reference}
\alias{read_ruleset}
\alias{validate_ruleset}
\title{Read and Register Declarative Definitions}
\description{
Create a local registry and read, validate, or store versioned instrument,
interpretation-reference, and workflow-rule definitions in YAML or JSON.
}
\usage{
psy_registry(path = NULL)

read_instrument(path, validate = TRUE)

register_instrument(instrument, registry = psy_registry(), overwrite = FALSE,
  confirm_license = FALSE)

list_instruments(registry = psy_registry(), language = NULL,
  redistributable = NULL, active = TRUE)

read_reference(path, validate = TRUE)

register_reference(reference, registry = psy_registry(), overwrite = FALSE)

read_ruleset(path, validate = TRUE)

validate_ruleset(x, registry = NULL, strict = TRUE)
}
\arguments{
  \item{path}{For \code{psy_registry()}, a registry directory; a temporary
  location is used when omitted. For readers, a YAML or JSON file.}
  \item{validate}{If \code{TRUE}, validate required definition fields while
  reading.}
  \item{instrument}{A \code{psy_instrument} object or its YAML/JSON path.}
  \item{registry}{A registry returned by \code{psy_registry()}. The argument
  to \code{validate_ruleset()} is reserved for cross-definition validation.}
  \item{overwrite}{If \code{TRUE}, replace an existing stored definition with
  the same identifier or version.}
  \item{confirm_license}{Explicit confirmation that the caller has permission
  to store and use a definition not marked redistributable. This flag does not
  grant or verify a license.}
  \item{language}{Optional language-code filter.}
  \item{redistributable}{Optional logical license-metadata filter.}
  \item{active}{If \code{TRUE}, omit definitions with inactive or retired
  status. Demo-only definitions are still listed.}
  \item{reference}{A \code{psy_reference} object or its YAML/JSON path.}
  \item{x}{A ruleset object to validate.}
  \item{strict}{If \code{TRUE}, stop on an invalid ruleset; otherwise return
  structured findings.}
}
\value{
\code{psy_registry()} returns a \code{psy_registry} list of directories.
The readers return \code{psy_instrument}, \code{psy_reference}, or
\code{psy_ruleset} lists. Registration functions return the stored path
invisibly. \code{list_instruments()} returns a metadata data frame without
item text. \code{validate_ruleset()} returns a \code{psy_quality} object.
}
\details{
Definitions bundled with psyActive are fictional and demo-only. A successful
schema check is not evidence of clinical validity, appropriate population fit,
or permission to use a real instrument.

Registry identifiers are portable ASCII strings of at most 100 bytes. They
must begin and end with a letter or digit; periods, underscores, and hyphens
may also appear between those characters. Instrument versions and reference
instrument versions must use Semantic Versioning 2.0.0, for example
\code{1.0.0} or \code{1.2.3-rc.1+build.5}. These restrictions keep stored
definitions inside their registry directories and give identifiers consistent
meaning on supported filesystems.
}
\examples{
inst_file <- system.file("extdata", "instruments", "demo_mood_9.yml",
  package = "psyActive")
ref_file <- system.file("extdata", "references",
  "demo_mood_9_zh_adult_v1.yml", package = "psyActive")
rule_file <- system.file("extdata", "rules", "adult_outpatient_demo.yml",
  package = "psyActive")

registry <- psy_registry(file.path(tempdir(), "psyactive-rd-registry"))
instrument <- read_instrument(inst_file)
register_instrument(instrument, registry, overwrite = TRUE)
list_instruments(registry)

reference <- read_reference(ref_file)
register_reference(reference, registry, overwrite = TRUE)

rules <- read_ruleset(rule_file)
validate_ruleset(rules)
}
