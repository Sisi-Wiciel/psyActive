provenance_instrument <- function() {
  read_instrument(system.file(
    "extdata", "instruments", "demo_mood_9.yml", package = "psyActive"
  ))
}

provenance_observations <- function(assessments = "assessment-1",
                                    episodes = NULL) {
  items <- paste0("dms_", 1:9)
  out <- do.call(rbind, lapply(seq_along(assessments), function(i) {
    values <- rep(i - 1, length(items))
    data.frame(
      observation_id = paste(assessments[i], items, sep = "-"),
      person_id = "person-1",
      assessment_id = assessments[i],
      observed_at = as.POSIXct("2026-01-01", tz = "UTC") +
        (i - 1) * 86400,
      instrument_id = "demo_mood_9",
      instrument_version = "1.0.0",
      item_id = items,
      value_raw = as.character(values),
      value_num = values,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  if (!is.null(episodes)) {
    out$episode_id <- rep(episodes, each = length(items))
  }
  out
}

test_that("score and audit hashes cover canonical scoring values", {
  instrument <- provenance_instrument()
  observations <- provenance_observations()
  original <- score_instrument(observations, instrument)

  reordered <- score_instrument(
    observations[rev(seq_len(nrow(observations))), , drop = FALSE],
    instrument
  )
  expect_identical(original$input_hash, reordered$input_hash)
  expect_identical(
    attr(original, "audit")$input_hash,
    attr(reordered, "audit")$input_hash
  )

  changed_numeric <- observations
  changed_numeric$value_num[1L] <- 1
  changed_numeric$value_raw[1L] <- "1"
  numeric_score <- score_instrument(changed_numeric, instrument)
  expect_false(identical(original$input_hash, numeric_score$input_hash))
  expect_false(identical(
    attr(original, "audit")$input_hash,
    attr(numeric_score, "audit")$input_hash
  ))

  changed_raw <- observations
  changed_raw$value_raw[1L] <- "zero"
  raw_score <- score_instrument(changed_raw, instrument)
  expect_equal(original$score_value, raw_score$score_value)
  expect_false(identical(original$input_hash, raw_score$input_hash))
  expect_false(identical(
    attr(original, "audit")$input_hash,
    attr(raw_score, "audit")$input_hash
  ))

  reversed_instrument <- instrument
  reversed_instrument$items[[1L]]$reverse <- list(min = 0, max = 3)
  reversed_score <- score_instrument(observations, reversed_instrument)
  expect_equal(explain_score(reversed_score)$transformed_value[1L], 3)
  expect_false(identical(original$input_hash, reversed_score$input_hash))
})

test_that("episode IDs propagate from observations into reliable change", {
  instrument <- provenance_instrument()
  observations <- provenance_observations(
    c("assessment-1", "assessment-2"), c("episode-1", "episode-1")
  )
  scores <- score_instrument(observations, instrument, audit = FALSE)
  expect_identical(scores$episode_id, c("episode-1", "episode-1"))

  reference <- read_reference(system.file(
    "extdata", "references", "demo_mood_9_zh_adult_v1.yml",
    package = "psyActive"
  ))
  changed <- reliable_change(scores, reference, from = "episode_start")
  expect_equal(changed$change_value, c(0, 9))
})

test_that("episode ID assessment contract rejects ambiguity", {
  instrument <- provenance_instrument()
  observations <- provenance_observations(episodes = "episode-1")

  conflicting <- observations
  conflicting$episode_id[1L] <- "episode-2"
  expect_error(
    score_instrument(conflicting, instrument, audit = FALSE),
    class = "psy_error_schema"
  )

  partly_missing <- observations
  partly_missing$episode_id[1L] <- NA_character_
  expect_error(
    score_instrument(partly_missing, instrument, audit = FALSE),
    class = "psy_error_schema"
  )

  blank <- observations
  blank$episode_id[1L] <- "  "
  expect_error(
    score_instrument(blank, instrument, audit = FALSE),
    class = "psy_error_schema"
  )

  all_missing <- observations
  all_missing$episode_id[] <- NA_character_
  missing_score <- score_instrument(all_missing, instrument, audit = FALSE)
  expect_true("episode_id" %in% names(missing_score))
  expect_true(all(is.na(missing_score$episode_id)))

  no_episode <- observations[names(observations) != "episode_id"]
  score_without_episode <- score_instrument(
    no_episode, instrument, audit = FALSE
  )
  expect_false("episode_id" %in% names(score_without_episode))
})
