library(testthat)
library(PriorRhythm)

# ---------------------------------------------------------------------------
# .parse_time_intervals() tests
# ---------------------------------------------------------------------------

test_that(".parse_time_intervals: '0 to 3 hr' -> time1=0, time2=3", {
  df     <- data.frame(time = "0 to 3 hr", stringsAsFactors = FALSE)
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result$time1, 0)
  expect_equal(result$time2, 3)
})

test_that(".parse_time_intervals: '1.00 to 3.5 hr' parses correctly", {
  df     <- data.frame(time = "1.00 to 3.5 hr", stringsAsFactors = FALSE)
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result$time1, 1.00)
  expect_equal(result$time2, 3.5)
})

test_that(".parse_time_intervals: '3.5to6.0 hr' (no spaces) parses correctly", {
  df     <- data.frame(time = "3.5to6.0 hr", stringsAsFactors = FALSE)
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result$time1, 3.5)
  expect_equal(result$time2, 6.0)
})

test_that(".parse_time_intervals: 'hours' suffix accepted", {
  df     <- data.frame(time = "0 to 3 hours", stringsAsFactors = FALSE)
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result$time1, 0)
  expect_equal(result$time2, 3)
})

test_that(".parse_time_intervals: unitless interval accepted (e.g. '0.75 to 3.50')", {
  df     <- data.frame(time = "0.75 to 3.50", stringsAsFactors = FALSE)
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result$time1, 0.75)
  expect_equal(result$time2, 3.50)
  expect_equal(result$time, "0.75 to 3.50")
})

test_that(".parse_time_intervals: unitless intervals in historical dog/NHP files parse correctly", {
  df <- data.frame(
    time = c("0.75 to 3.50", "5.00 to 9.00"),
    stringsAsFactors = FALSE
  )
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result$time1, c(0.75, 5.00))
  expect_equal(result$time2, c(3.50, 9.00))
  expect_equal(result$time, c("0.75 to 3.50", "5.00 to 9.00"))
})

test_that(".parse_time_intervals: reversed unitless interval still errors", {
  df <- data.frame(time = "5.00 to 1.00", stringsAsFactors = FALSE)
  expect_error(
    PriorRhythm:::.parse_time_intervals(df, time_col = "time"),
    regexp = "time2"
  )
})

test_that(".parse_time_intervals: multiple rows parse correctly", {
  df <- data.frame(
    time = c("0 to 3 hr", "3 to 6 hr", "6 to 12 hr"),
    value = 1:3,
    stringsAsFactors = FALSE
  )
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result$time1, c(0, 3, 6))
  expect_equal(result$time2, c(3, 6, 12))
})

test_that(".parse_time_intervals: raw time column is unchanged", {
  df       <- data.frame(time = c("0 to 3 hr", "3.5to6.0 hr"),
                         stringsAsFactors = FALSE)
  result   <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result$time, c("0 to 3 hr", "3.5to6.0 hr"))
})

test_that(".parse_time_intervals: NA values pass through silently", {
  df     <- data.frame(time = c("0 to 3 hr", NA_character_),
                       stringsAsFactors = FALSE)
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result$time1, c(0, NA_real_))
  expect_equal(result$time2, c(3, NA_real_))
})

test_that(".parse_time_intervals: 'morning' errors with actionable message", {
  df <- data.frame(time = "morning", stringsAsFactors = FALSE)
  expect_error(
    PriorRhythm:::.parse_time_intervals(df, time_col = "time"),
    regexp = "morning"
  )
})

test_that(".parse_time_intervals: '3 to 0 hr' (reversed) errors clearly", {
  df <- data.frame(time = "3 to 0 hr", stringsAsFactors = FALSE)
  expect_error(
    PriorRhythm:::.parse_time_intervals(df, time_col = "time"),
    regexp = "time2"
  )
})

test_that(".parse_time_intervals: error names the time column", {
  df <- data.frame(mytime = "bad_value", stringsAsFactors = FALSE)
  expect_error(
    PriorRhythm:::.parse_time_intervals(df, time_col = "mytime"),
    regexp = "mytime"
  )
})

test_that(".parse_time_intervals: time1/time2 inserted after time_col", {
  df <- data.frame(
    id   = 1L,
    time = "0 to 3 hr",
    val  = 42,
    stringsAsFactors = FALSE
  )
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  pos_time  <- which(names(result) == "time")
  pos_time1 <- which(names(result) == "time1")
  pos_time2 <- which(names(result) == "time2")
  expect_equal(pos_time1, pos_time + 1L)
  expect_equal(pos_time2, pos_time + 2L)
})

test_that(".parse_time_intervals: works when time is the final column", {
  df <- data.frame(
    id   = 1L,
    time = "0 to 3 hr",
    stringsAsFactors = FALSE
  )
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result[["time1"]], 0)
  expect_equal(result[["time2"]], 3)
  expect_equal(result[["time"]], "0 to 3 hr")
  pos_time  <- which(names(result) == "time")
  pos_time1 <- which(names(result) == "time1")
  pos_time2 <- which(names(result) == "time2")
  expect_equal(pos_time1, pos_time + 1L)
  expect_equal(pos_time2, pos_time + 2L)
})

test_that(".parse_time_intervals: works when time is the only column", {
  df <- data.frame(
    time = c("0 to 3 hr", "3 to 6 hr"),
    stringsAsFactors = FALSE
  )
  result <- PriorRhythm:::.parse_time_intervals(df, time_col = "time")
  expect_equal(result[["time1"]], c(0, 3))
  expect_equal(result[["time2"]], c(3, 6))
  expect_equal(result[["time"]], c("0 to 3 hr", "3 to 6 hr"))
})

test_that(".parse_time_intervals: errors when time_col absent", {
  df <- data.frame(other = 1, stringsAsFactors = FALSE)
  expect_error(
    PriorRhythm:::.parse_time_intervals(df, time_col = "time"),
    regexp = "time"
  )
})


# ---------------------------------------------------------------------------
# .resolve_column_map_with_diagnostics() tests
# ---------------------------------------------------------------------------

test_that(".resolve_column_map_with_diagnostics: renames and records provenance", {
  df     <- data.frame(summary_period = "0 to 3 hr", a_mean = 40.1,
                       stringsAsFactors = FALSE)
  result <- PriorRhythm:::.resolve_column_map_with_diagnostics(
    df,
    column_map = list(time = c("summary_period", "Time"),
                      activity = "a_mean")
  )
  expect_true("time"     %in% names(result$df))
  expect_true("activity" %in% names(result$df))
  expect_equal(result$provenance[["time"]],     "summary_period")
  expect_equal(result$provenance[["activity"]], "a_mean")
})

test_that(".resolve_column_map_with_diagnostics: canonical already present is not overwritten", {
  df <- data.frame(time = "already", summary_period = "0 to 3 hr",
                   stringsAsFactors = FALSE)
  result <- PriorRhythm:::.resolve_column_map_with_diagnostics(
    df,
    column_map = list(time = c("summary_period", "Time"))
  )
  expect_true("summary_period" %in% names(result$df))
  expect_equal(result$df$time, "already")
  expect_equal(result$provenance[["time"]], "time")
})

test_that(".resolve_column_map_with_diagnostics: NA provenance when no alias matched", {
  df     <- data.frame(a = 1, stringsAsFactors = FALSE)
  result <- PriorRhythm:::.resolve_column_map_with_diagnostics(
    df,
    column_map = list(missing_col = c("col_x", "col_y"))
  )
  expect_true(is.na(result$provenance[["missing_col"]]))
})

test_that(".resolve_column_map_with_diagnostics: override_map wins over column_map", {
  df <- data.frame(src_a = 1, src_b = 2, stringsAsFactors = FALSE)
  result <- PriorRhythm:::.resolve_column_map_with_diagnostics(
    df,
    column_map   = list(myfield = "src_a"),
    override_map = list(myfield = "src_b")
  )
  expect_equal(result$provenance[["myfield"]], "src_b")
  expect_false("src_b" %in% names(result$df))
  expect_true("myfield" %in% names(result$df))
})

test_that(".resolve_column_map_with_diagnostics: NULL column_map returns empty provenance", {
  df     <- data.frame(a = 1, stringsAsFactors = FALSE)
  result <- PriorRhythm:::.resolve_column_map_with_diagnostics(df)
  expect_equal(names(result$df), "a")
  expect_length(result$provenance, 0L)
})


# ---------------------------------------------------------------------------
# .validate_study_input() tests
# ---------------------------------------------------------------------------

test_that(".validate_study_input: ok=TRUE when all required cols present", {
  df         <- data.frame(animal = 1, time = "0 to 3 hr",
                           stringsAsFactors = FALSE)
  provenance <- c(animal = "animal", time = "time")
  result     <- PriorRhythm:::.validate_study_input(
    df            = df,
    provenance    = provenance,
    required_cols = c("animal", "time"),
    abort         = FALSE
  )
  expect_true(result$ok)
  expect_length(result$missing, 0L)
})

test_that(".validate_study_input: ok=FALSE and missing reported when col absent", {
  df         <- data.frame(animal = 1, stringsAsFactors = FALSE)
  provenance <- c(animal = "animal")
  result     <- PriorRhythm:::.validate_study_input(
    df            = df,
    provenance    = provenance,
    required_cols = c("animal", "time"),
    abort         = FALSE
  )
  expect_false(result$ok)
  expect_true("time" %in% result$missing)
})

test_that(".validate_study_input: aborts with cli error when abort=TRUE", {
  df         <- data.frame(animal = 1, stringsAsFactors = FALSE)
  provenance <- c(animal = "animal")
  expect_error(
    PriorRhythm:::.validate_study_input(
      df            = df,
      provenance    = provenance,
      required_cols = c("animal", "time"),
      abort         = TRUE
    ),
    regexp = "time"
  )
})

test_that(".validate_study_input: endpoint_cols absence is a warning, not error", {
  df         <- data.frame(animal = 1, time = "0 to 3 hr",
                           stringsAsFactors = FALSE)
  provenance <- c(animal = "animal", time = "time")
  result     <- PriorRhythm:::.validate_study_input(
    df            = df,
    provenance    = provenance,
    required_cols = c("animal", "time"),
    endpoint_cols = c("qtc"),
    abort         = FALSE
  )
  expect_true(result$ok)
  expect_true(any(grepl("qtc", result$warnings)))
})


# ---------------------------------------------------------------------------
# Character code preservation
# ---------------------------------------------------------------------------

test_that("import_historical_data preserves '001' as character", {
  study_list <- list(
    study_1 = data.frame(
      animal         = c("001", "002"),
      treatment_code = c(1L, 2L),
      period_code    = c(1L, 1L),
      time           = c("0 to 3 hr", "3 to 6 hr"),
      stringsAsFactors = FALSE
    )
  )
  result <- import_historical_data(data_file_list = study_list)
  expect_true(is.character(result$animal))
  expect_equal(result$animal, c("001", "002"))
})


# ---------------------------------------------------------------------------
# Period code tests
# ---------------------------------------------------------------------------

test_that(".derive_period_code: existing period_code unchanged", {
  df <- data.frame(
    period      = c("Day 1", "Day 2"),
    period_code = c(99L, 88L),
    stringsAsFactors = FALSE
  )
  result <- PriorRhythm:::.derive_period_code(df)
  expect_equal(result$period_code, c(99L, 88L))
})

test_that(".derive_period_code: derives only when period_code absent", {
  df <- data.frame(
    period = c("Day 2", "Day 1", "Day 2"),
    stringsAsFactors = FALSE
  )
  result <- PriorRhythm:::.derive_period_code(df)
  expect_equal(result$period_code, c(1L, 2L, 1L))
})


# ---------------------------------------------------------------------------
# Path consistency: historical import vs. target upload preparation
# ---------------------------------------------------------------------------

test_that("import_historical_data + .parse_time_intervals yield same time1/time2 as target path", {
  fixture_df <- data.frame(
    animal         = c("001", "002"),
    time           = c("0 to 3 hr", "3.5to6.0 hr"),
    period         = c("Day 1", "Day 1"),
    treatment_code = c(1L, 1L),
    stringsAsFactors = FALSE
  )

  # Historical path
  hist_result <- import_historical_data(
    data_file_list = list(study_a = fixture_df)
  )
  hist_result <- PriorRhythm:::.parse_time_intervals(hist_result, time_col = "time")

  # Target path (simulate what mod_file_upload_server does)
  target_result <- PriorRhythm:::.parse_time_intervals(fixture_df, time_col = "time")
  target_result <- PriorRhythm:::.derive_period_code(target_result)

  expect_equal(hist_result$time1, target_result$time1)
  expect_equal(hist_result$time2, target_result$time2)
  expect_equal(hist_result$period_code, target_result$period_code)
})
