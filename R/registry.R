#' Create a psyActive Registry
#' @param path Registry directory. A temporary directory is created if omitted.
#' @details Registry identifiers are portable ASCII strings that begin and end
#'   with a letter or digit and may contain periods, underscores, and hyphens.
#'   Instrument versions follow Semantic Versioning 2.0.0. Registration checks
#'   that each destination remains inside the applicable registry directory.
#' @return A `psy_registry` object.
#' @export
psy_registry <- function(path = NULL) {
  if (is.null(path)) path <- file.path(tempdir(), "psyActive-registry")
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(trimws(path))) {
    psy_abort("path must be one non-empty directory path.", "psy_error_schema")
  }
  if (!dir.exists(path) &&
      !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    psy_abort("The registry directory could not be created.",
              "psy_error_schema")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  dirs <- file.path(path, c("instruments", "references", "rules"))
  for (directory in dirs) {
    if (!dir.exists(directory) &&
        !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
      psy_abort("A registry subdirectory could not be created.",
                "psy_error_schema")
    }
  }
  dirs <- unname(vapply(
    dirs, normalizePath, character(1), winslash = "/", mustWork = TRUE
  ))
  structure(
    list(
      path = path,
      instruments = unname(dirs[1L]),
      references = unname(dirs[2L]),
      rules = unname(dirs[3L])
    ),
    class = "psy_registry"
  )
}

print.psy_registry <- function(x, ...) {
  cat("<psy_registry>", x$path, "\n")
  cat(
    "  instruments:", length(list.files(x$instruments)),
    " references:", length(list.files(x$references)),
    " rules:", length(list.files(x$rules)), "\n"
  )
  invisible(x)
}

validate_registry_id <- function(x, field, error_class) {
  valid_shape <- is.character(x) && length(x) == 1L && !is.na(x) &&
    nchar(x, type = "bytes") <= 100L &&
    grepl(
      "^[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?$",
      x, perl = TRUE
    )
  windows_devices <- c(
    "CON", "PRN", "AUX", "NUL", paste0("COM", 1:9), paste0("LPT", 1:9)
  )
  device_stem <- if (is.character(x) && length(x) == 1L && !is.na(x)) {
    toupper(sub("[.].*$", "", x))
  } else {
    NA_character_
  }
  if (!isTRUE(valid_shape) || isTRUE(device_stem %in% windows_devices)) {
    psy_abort(
      sprintf(
        paste0(
          "%s must be a portable identifier of at most 100 ASCII bytes; ",
          "use letters or digits at both ends and only letters, digits, ",
          "periods, underscores, or hyphens in between."
        ),
        field
      ),
      error_class
    )
  }
  invisible(TRUE)
}

validate_semantic_version <- function(x, field, error_class) {
  semver_pattern <- paste0(
    "^(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)",
    "(?:-((?:0|[1-9][0-9]*|[0-9]*[A-Za-z-][0-9A-Za-z-]*)",
    "(?:\\.(?:0|[1-9][0-9]*|[0-9]*[A-Za-z-][0-9A-Za-z-]*))*))?",
    "(?:\\+([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*))?$"
  )
  valid <- is.character(x) && length(x) == 1L && !is.na(x) &&
    nchar(x, type = "bytes") <= 100L && grepl(semver_pattern, x, perl = TRUE)
  if (!isTRUE(valid)) {
    psy_abort(
      sprintf("%s must be a valid semantic version such as '1.0.0'.", field),
      error_class
    )
  }
  invisible(TRUE)
}

registry_component_path <- function(registry, component, error_class) {
  if (!inherits(registry, "psy_registry") ||
      !is.character(registry$path) || length(registry$path) != 1L ||
      is.na(registry$path) || !dir.exists(registry$path) ||
      !is.character(registry[[component]]) ||
      length(registry[[component]]) != 1L ||
      is.na(registry[[component]]) || !dir.exists(registry[[component]])) {
    psy_abort("registry must be a valid psy_registry object.", error_class)
  }

  root <- normalizePath(
    registry$path, winslash = "/", mustWork = TRUE
  )
  expected <- file.path(root, component)
  if (!dir.exists(expected)) {
    psy_abort("The registry is missing a required subdirectory.", error_class)
  }
  expected <- normalizePath(expected, winslash = "/", mustWork = TRUE)
  actual <- normalizePath(
    registry[[component]], winslash = "/", mustWork = TRUE
  )
  if (!identical(actual, expected) || !path_is_within(expected, root) ||
      !identical(dirname(expected), root)) {
    psy_abort(
      sprintf("The registry %s directory must be contained in the registry.",
              component),
      error_class
    )
  }
  expected
}

path_is_within <- function(path, root) {
  if (.Platform$OS.type == "windows") {
    path <- tolower(path)
    root <- tolower(root)
  }
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

registry_target_path <- function(registry, component, filename, error_class) {
  root <- registry_component_path(registry, component, error_class)
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) ||
      !nzchar(filename) || basename(filename) != filename ||
      grepl("[/\\\\]", filename)) {
    psy_abort("A registry target must be a direct child of its registry directory.",
              error_class)
  }

  target <- file.path(root, filename)
  link_target <- Sys.readlink(target)
  if (length(link_target) == 1L && !is.na(link_target) &&
      nzchar(link_target)) {
    psy_abort("Registry targets may not be symbolic links.", error_class)
  }
  resolved <- normalizePath(target, winslash = "/", mustWork = FALSE)
  if (!path_is_within(resolved, root) ||
      !identical(dirname(resolved), root)) {
    psy_abort("The registry target is outside its registry directory.",
              error_class)
  }
  target
}

registry_rds_files <- function(registry, component, error_class) {
  root <- registry_component_path(registry, component, error_class)
  filenames <- list.files(root, pattern = "[.]rds$", full.names = FALSE)
  if (!length(filenames)) return(character())
  vapply(
    filenames,
    function(filename) {
      registry_target_path(registry, component, filename, error_class)
    },
    character(1)
  )
}

read_definition <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path)) {
    psy_abort("path must be one non-empty definition file path.",
              "psy_error_schema")
  }
  if (!file.exists(path)) {
    psy_abort(sprintf("Definition file does not exist: %s", path),
              "psy_error_schema")
  }
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("yml", "yaml")) yaml::read_yaml(path)
  else if (ext == "json") jsonlite::fromJSON(path, simplifyVector = FALSE)
  else psy_abort("Definitions must be YAML or JSON.", "psy_error_schema")
}

validate_instrument_def <- function(x) {
  req <- c(
    "schema_version", "instrument_id", "name", "version", "language",
    "items", "scores", "license"
  )
  miss <- if (is.list(x)) setdiff(req, names(x)) else req
  if (length(miss)) {
    psy_abort(
      paste("Instrument definition is missing:", paste(miss, collapse = ", ")),
      "psy_error_instrument"
    )
  }
  validate_registry_id(
    x$instrument_id, "instrument_id", "psy_error_instrument"
  )
  validate_semantic_version(x$version, "version", "psy_error_instrument")
  ops <- vapply(x$scores, function(z) z$operation %||% "", character(1))
  allowed <- c("sum", "mean", "weighted_sum", "count_if")
  if (any(!ops %in% allowed)) {
    psy_abort(
      paste(
        "Unsupported scoring operation(s):",
        paste(unique(ops[!ops %in% allowed]), collapse = ", ")
      ),
      "psy_error_instrument"
    )
  }
  ids <- vapply(x$items, function(z) z$item_id %||% "", character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids)) {
    psy_abort("Instrument item_id values must be non-empty and unique.",
              "psy_error_instrument")
  }
  invisible(TRUE)
}

#' Read an Instrument Definition
#' @param path YAML or JSON definition path.
#' @param validate Validate the declarative schema.
#' @return A `psy_instrument` list.
#' @export
read_instrument <- function(path, validate = TRUE) {
  x <- read_definition(path)
  if (validate) validate_instrument_def(x)
  class(x) <- c("psy_instrument", "list")
  attr(x, "source_path") <- normalizePath(
    path, winslash = "/", mustWork = TRUE
  )
  x
}

#' Register an Instrument
#' @param instrument Instrument object or YAML/JSON path.
#' @param registry Registry object.
#' @param overwrite Replace an existing version.
#' @param confirm_license Confirm the caller may store and use this definition.
#' @return Invisibly, the stored path.
#' @export
register_instrument <- function(instrument, registry = psy_registry(),
                                overwrite = FALSE,
                                confirm_license = FALSE) {
  if (is.character(instrument)) instrument <- read_instrument(instrument)
  validate_instrument_def(instrument)
  redistributable <- isTRUE(instrument$license$redistributable)
  if (!redistributable && !isTRUE(confirm_license)) {
    psy_abort(
      paste0(
        "The instrument is not marked redistributable. Set ",
        "confirm_license=TRUE only after obtaining permission."
      ),
      "psy_error_instrument"
    )
  }
  filename <- paste0(
    instrument$instrument_id, "__", instrument$version, ".rds"
  )
  dest <- registry_target_path(
    registry, "instruments", filename, "psy_error_instrument"
  )
  if (file.exists(dest) && !overwrite) {
    psy_abort("That instrument version is already registered.",
              "psy_error_instrument")
  }
  saveRDS(instrument, dest)
  invisible(dest)
}

#' List Registered Instruments
#' @param registry Registry object.
#' @param language Optional language filter.
#' @param redistributable Optional license filter.
#' @param active If `TRUE`, omit definitions with inactive status.
#' @return Instrument metadata data frame without item text.
#' @export
list_instruments <- function(registry = psy_registry(), language = NULL,
                             redistributable = NULL, active = TRUE) {
  fs <- registry_rds_files(
    registry, "instruments", "psy_error_instrument"
  )
  if (!length(fs)) {
    return(data.frame(
      instrument_id = character(), version = character(), name = character(),
      language = character(), status = character(), redistributable = logical()
    ))
  }
  out <- do.call(rbind, lapply(fs, function(f) {
    z <- readRDS(f)
    data.frame(
      instrument_id = z$instrument_id, version = z$version, name = z$name,
      language = z$language, status = z$status %||% "active",
      redistributable = isTRUE(z$license$redistributable),
      stringsAsFactors = FALSE
    )
  }))
  if (!is.null(language)) {
    out <- out[out$language %in% language, , drop = FALSE]
  }
  if (!is.null(redistributable)) {
    out <- out[out$redistributable %in% redistributable, , drop = FALSE]
  }
  if (active) {
    out <- out[!out$status %in% c("inactive", "retired"), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

validate_reference_def <- function(x) {
  req <- c(
    "schema_version", "reference_id", "instrument_id",
    "instrument_version", "language"
  )
  miss <- if (is.list(x)) setdiff(req, names(x)) else req
  if (length(miss)) {
    psy_abort(
      paste("Reference definition is missing:", paste(miss, collapse = ", ")),
      "psy_error_reference"
    )
  }
  validate_registry_id(
    x$reference_id, "reference_id", "psy_error_reference"
  )
  validate_registry_id(
    x$instrument_id, "instrument_id", "psy_error_reference"
  )
  validate_semantic_version(
    x$instrument_version, "instrument_version", "psy_error_reference"
  )
  invisible(TRUE)
}

#' Read a Score Interpretation Reference
#' @param path YAML or JSON path.
#' @param validate Validate required metadata.
#' @return A `psy_reference` list.
#' @export
read_reference <- function(path, validate = TRUE) {
  x <- read_definition(path)
  if (validate) validate_reference_def(x)
  class(x) <- c("psy_reference", "list")
  attr(x, "source_path") <- normalizePath(
    path, winslash = "/", mustWork = TRUE
  )
  x
}

#' Register a Score Reference
#' @param reference Reference object or path.
#' @param registry Registry object.
#' @param overwrite Replace an existing reference.
#' @return Invisibly, the stored path.
#' @export
register_reference <- function(reference, registry = psy_registry(),
                               overwrite = FALSE) {
  if (is.character(reference)) reference <- read_reference(reference)
  validate_reference_def(reference)
  dest <- registry_target_path(
    registry, "references", paste0(reference$reference_id, ".rds"),
    "psy_error_reference"
  )
  if (file.exists(dest) && !overwrite) {
    psy_abort("That reference is already registered.",
              "psy_error_reference")
  }
  saveRDS(reference, dest)
  invisible(dest)
}

get_instrument <- function(instrument, registry) {
  if (inherits(instrument, "psy_instrument")) return(instrument)
  if (!is.character(instrument) || length(instrument) != 1L ||
      is.na(instrument)) {
    psy_abort("instrument must be a definition, path, or registered ID.",
              "psy_error_instrument")
  }
  if (file.exists(instrument)) return(read_instrument(instrument))

  validate_registry_id(
    instrument, "instrument_id", "psy_error_instrument"
  )
  files <- registry_rds_files(
    registry, "instruments", "psy_error_instrument"
  )
  definitions <- lapply(files, readRDS)
  matches <- vapply(definitions, function(definition) {
    is.list(definition) && is.character(definition$instrument_id) &&
      length(definition$instrument_id) == 1L &&
      !is.na(definition$instrument_id) &&
      identical(unname(definition$instrument_id), unname(instrument))
  }, logical(1))
  if (sum(matches) != 1L) {
    psy_abort(
      sprintf("Instrument '%s' was not uniquely found in the registry.",
              instrument),
      "psy_error_instrument"
    )
  }
  definitions[[which(matches)]]
}

get_reference <- function(reference, registry) {
  if (inherits(reference, "psy_reference")) return(reference)
  if (!is.character(reference) || length(reference) != 1L ||
      is.na(reference)) {
    psy_abort("Reference was not found.", "psy_error_reference")
  }
  if (file.exists(reference)) return(read_reference(reference))

  validate_registry_id(reference, "reference_id", "psy_error_reference")
  target <- registry_target_path(
    registry, "references", paste0(reference, ".rds"),
    "psy_error_reference"
  )
  if (file.exists(target)) return(readRDS(target))
  psy_abort("Reference was not found.", "psy_error_reference")
}
