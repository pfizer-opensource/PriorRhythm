library(testthat)
library(PriorRhythm)

# Helper data ----------------------------------------------------------------

make_df_A <- function() {
  data.frame(
    summary_period = c("0 to 3 hr", "3 to 6 hr"),
    a_mean         = c(40.1, 42.3),
    animal         = 1:2,
    stringsAsFactors = FALSE
  )
}

make_df_B <- function() {
  data.frame(
    Time           = c("0 to 3 hr", "3 to 6 hr"),
    Activity       = c(39.0, 41.5),
    animal         = 1:2,
    stringsAsFactors = FALSE
  )
}


# .resolve_column_map() tests ------------------------------------------------

test_that(".resolve_column_map: single-alias named character vector renames correctly", {
  df     <- make_df_A()
  result <- PriorRhythm:::.resolve_column_map(
    df, c(time = "summary_period", activity = "a_mean")
  )
  expect_true("time"     %in% names(result))
  expect_true("activity" %in% names(result))
  expect_false("summary_period" %in% names(result))
  expect_false("a_mean"         %in% names(result))
})

test_that(".resolve_column_map: multi-alias list uses first matching alias", {
  df     <- make_df_A()   # has summary_period, a_mean
  result <- PriorRhythm:::.resolve_column_map(
    df,
    list(
      time     = c("summary_period", "Time"),
      activity = c("a_mean", "Activity")
    )
  )
  expect_true("time"     %in% names(result))
  expect_true("activity" %in% names(result))
  expect_false("summary_period" %in% names(result))
})

test_that(".resolve_column_map: multi-alias list uses second alias when first absent", {
  df     <- make_df_B()   # has Time, Activity (not summary_period / a_mean)
  result <- PriorRhythm:::.resolve_column_map(
    df,
    list(
      time     = c("summary_period", "Time"),
      activity = c("a_mean", "Activity")
    )
  )
  expect_true("time"     %in% names(result))
  expect_true("activity" %in% names(result))
  expect_false("Time"     %in% names(result))
  expect_false("Activity" %in% names(result))
})

test_that(".resolve_column_map: no alias found leaves column unchanged silently", {
  df     <- make_df_A()
  result <- PriorRhythm:::.resolve_column_map(
    df,
    list(nonexistent_col = c("col_x", "col_y"))
  )
  expect_equal(names(result), names(df))
})

test_that(".resolve_column_map: canonical name already present causes no rename", {
  df        <- make_df_A()
  df$time   <- "already_here"
  result    <- PriorRhythm:::.resolve_column_map(
    df,
    list(time = c("summary_period", "Time"))
  )
  # summary_period should remain; time was already there
  expect_true("time"           %in% names(result))
  expect_true("summary_period" %in% names(result))
})

test_that(".resolve_column_map: NULL column_map returns df unchanged", {
  df     <- make_df_A()
  result <- PriorRhythm:::.resolve_column_map(df, NULL)
  expect_equal(names(result), names(df))
})


# import_historical_data() tests ---------------------------------------------

test_that("import_historical_data errors when both data_file_list and data_path_dir are NULL", {
  expect_error(
    import_historical_data(),
    regexp = "data_file_list|data_path_dir"
  )
})

test_that("import_historical_data returns a data frame from data_file_list", {
  study_list <- list(
    study_1 = make_df_A(),
    study_2 = make_df_B()
  )
  result <- import_historical_data(
    data_file_list = study_list,
    column_map     = list(
      time     = c("summary_period", "Time"),
      activity = c("a_mean", "Activity")
    )
  )
  expect_true(is.data.frame(result))
})

test_that("import_historical_data applies multi-alias column map to combined output", {
  study_list <- list(
    study_1 = make_df_A(),
    study_2 = make_df_B()
  )
  result <- import_historical_data(
    data_file_list = study_list,
    column_map     = list(
      time     = c("summary_period", "Time"),
      activity = c("a_mean", "Activity")
    )
  )
  expect_true("time"     %in% names(result))
  expect_true("activity" %in% names(result))
  expect_false("summary_period" %in% names(result))
  expect_false("Time"            %in% names(result))
  expect_false("Activity"        %in% names(result))
})

test_that("import_historical_data applies named character vector column map", {
  study_list <- list(study_1 = make_df_A())
  result <- import_historical_data(
    data_file_list = study_list,
    column_map     = c(time = "summary_period", activity = "a_mean")
  )
  expect_true("time"     %in% names(result))
  expect_true("activity" %in% names(result))
})

test_that("import_historical_data uses config$column_map when column_map is NULL", {
  study_list <- list(study_1 = make_df_A())
  cfg    <- study_config(column_map = c(time = "summary_period"))
  result <- import_historical_data(data_file_list = study_list, config = cfg)
  expect_true("time" %in% names(result))
  expect_false("summary_period" %in% names(result))
})

test_that("import_historical_data explicit column_map overrides config$column_map", {
  study_list <- list(study_1 = make_df_A())
  cfg    <- study_config(column_map = c(time = "summary_period"))
  # Pass a different (wrong) column_map to show explicit arg wins
  result <- import_historical_data(
    data_file_list = study_list,
    column_map     = c(activity = "a_mean"),
    config         = cfg
  )
  # activity renamed because explicit column_map was used
  expect_true("activity"       %in% names(result))
  # summary_period NOT renamed because explicit column_map does not map it
  expect_true("summary_period" %in% names(result))
})

test_that("import_historical_data reads column_map from a YAML file", {
  study_list <- list(
    study_1 = make_df_A(),
    study_2 = make_df_B()
  )
  tmp_yaml <- file.path(tempdir(), "test_col_map_import.yaml")
  on.exit(unlink(tmp_yaml))
  writeLines(c(
    "column_map:",
    "  time: [summary_period, Time]",
    "  activity: a_mean"
  ), tmp_yaml)

  result <- import_historical_data(
    data_file_list = study_list,
    column_map     = tmp_yaml
  )
  expect_true("time"     %in% names(result))
  expect_true("activity" %in% names(result))
  expect_false("summary_period" %in% names(result))
  expect_false("Time"            %in% names(result))
})

test_that("import_historical_data handles single-alias YAML correctly", {
  study_list <- list(study_1 = make_df_A())
  tmp_yaml <- file.path(tempdir(), "test_single_alias.yaml")
  on.exit(unlink(tmp_yaml))
  writeLines(c(
    "column_map:",
    "  time: summary_period"
  ), tmp_yaml)

  result <- import_historical_data(
    data_file_list = study_list,
    column_map     = tmp_yaml
  )
  expect_true("time" %in% names(result))
  expect_false("summary_period" %in% names(result))
})

test_that("import_historical_data output has expected columns from input data frames", {
  study_list <- list(
    study_1 = make_df_A(),
    study_2 = make_df_B()
  )
  result <- import_historical_data(
    data_file_list = study_list,
    column_map     = list(
      time     = c("summary_period", "Time"),
      activity = c("a_mean", "Activity")
    )
  )
  expect_true(is.data.frame(result))
  expect_true("animal"   %in% names(result))
  expect_true("time"     %in% names(result))
  expect_true("activity" %in% names(result))
  expect_equal(nrow(result), nrow(make_df_A()) + nrow(make_df_B()))
})

test_that(".derive_period_code adds period_code preserving encounter order", {
  df <- data.frame(
    animal = 1:4,
    period = c("3 to 6 hr", "0 to 3 hr", "3 to 6 hr", "0 to 3 hr"),
    stringsAsFactors = FALSE
  )
  result <- PriorRhythm:::.derive_period_code(df)
  expect_true("period_code" %in% names(result))
  # Encounter order: "3 to 6 hr" first → code 1; "0 to 3 hr" second → code 2
  expect_equal(result$period_code[result$period == "3 to 6 hr"], c(1L, 1L))
  expect_equal(result$period_code[result$period == "0 to 3 hr"], c(2L, 2L))
})

test_that(".derive_period_code does not overwrite existing period_code", {
  df <- data.frame(
    animal      = 1:3,
    period      = c("0 to 3 hr", "3 to 6 hr", "0 to 3 hr"),
    period_code = c(99L, 99L, 99L),
    stringsAsFactors = FALSE
  )
  result <- PriorRhythm:::.derive_period_code(df)
  expect_equal(result$period_code, c(99L, 99L, 99L))
})

test_that("import_historical_data preserves leading-zero character identifiers", {
  study_list <- list(
    study_1 = data.frame(
      animal         = c("001", "002", "001"),
      treatment_code = c(1L, 1L, 2L),
      period_code    = c(1L, 1L, 1L),
      time           = c(1, 2, 3),
      stringsAsFactors = FALSE
    )
  )
  result <- import_historical_data(data_file_list = study_list)
  expect_true(is.character(result$animal))
  expect_equal(result$animal, c("001", "002", "001"))
})

test_that("import_historical_data does not overwrite existing period_code when period also present", {
  study_list <- list(
    study_1 = data.frame(
      animal         = 1:3,
      period         = c("0 to 3 hr", "3 to 6 hr", "0 to 3 hr"),
      period_code    = c(10L, 20L, 10L),
      treatment_code = c(1L, 1L, 1L),
      time           = c(1, 2, 3),
      stringsAsFactors = FALSE
    )
  )
  result <- import_historical_data(data_file_list = study_list)
  expect_equal(result$period_code, c(10L, 20L, 10L))
})

test_that("import_historical_data binds studies with mixed numeric and character period labels", {
  study_list <- list(
    old_study = data.frame(
      animal         = c(1L, 2L),
      period         = c(1, 2),
      treatment_code = c(1L, 1L),
      time           = c(1.0, 2.0),
      stringsAsFactors = FALSE
    ),
    new_study = data.frame(
      animal         = c(3L, 4L),
      period         = c("Day 1", "Day 8"),
      period_code    = c(1L, 2L),
      treatment_code = c(1L, 1L),
      time           = c(1.0, 8.0),
      stringsAsFactors = FALSE
    )
  )

  result <- import_historical_data(data_file_list = study_list)

  expect_true(is.character(result$period))
  expect_true(all(c("1", "2", "Day 1", "Day 8") %in% result$period))
  expect_true(is.numeric(result$period_code) || is.integer(result$period_code))
  expect_equal(result$period_code[result$period == "Day 1"], 1L)
  expect_equal(result$period_code[result$period == "Day 8"], 2L)
  expect_true(all(!is.na(result$period_code[result$period %in% c("1", "2")])))
})


# .parse_time_intervals() tests ----------------------------------------------

test_that(".parse_time_intervals defaults accept 'hr' suffix", {
  df  <- data.frame(time = c("0 to 3 hr", "3 to 6 hr"), stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time")
  expect_equal(res$time1, c(0, 3))
  expect_equal(res$time2, c(3, 6))
})

test_that(".parse_time_intervals defaults accept 'hours' suffix", {
  df  <- data.frame(time = "0 to 3 hours", stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time")
  expect_equal(res$time1, 0)
  expect_equal(res$time2, 3)
})

test_that(".parse_time_intervals defaults accept whitespace-free variant", {
  df  <- data.frame(time = "3.5to6.0 hr", stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time")
  expect_equal(res$time1, 3.5)
  expect_equal(res$time2, 6.0)
})

test_that(".parse_time_intervals defaults accept unitless values", {
  df  <- data.frame(time = "0.75 to 3.50", stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time")
  expect_equal(res$time1, 0.75)
  expect_equal(res$time2, 3.50)
})

test_that(".parse_time_intervals passes NA through silently", {
  df  <- data.frame(time = c("0 to 3 hr", NA_character_), stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time")
  expect_true(is.na(res$time1[2]))
  expect_true(is.na(res$time2[2]))
})

test_that(".parse_time_intervals errors on malformed value", {
  df <- data.frame(time = "bad_value", stringsAsFactors = FALSE)
  expect_error(
    PriorRhythm:::.parse_time_intervals(df, "time"),
    regexp = "bad_value"
  )
})

test_that(".parse_time_intervals errors when time2 <= time1", {
  df <- data.frame(time = "6 to 3 hr", stringsAsFactors = FALSE)
  expect_error(
    PriorRhythm:::.parse_time_intervals(df, "time"),
    regexp = "time2"
  )
})

test_that(".parse_time_intervals custom delimiter works", {
  df  <- data.frame(time = c("0 - 3", "3 - 6"), stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time", delimiter = "-")
  expect_equal(res$time1, c(0, 3))
  expect_equal(res$time2, c(3, 6))
})

test_that(".parse_time_intervals regex-special delimiter is safely escaped", {
  df  <- data.frame(time = c("0 | 3", "3 | 6"), stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time", delimiter = "|")
  expect_equal(res$time1, c(0, 3))
  expect_equal(res$time2, c(3, 6))
})

test_that(".parse_time_intervals custom units are accepted", {
  df  <- data.frame(time = "0 to 3 min", stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time", units = c("min"))
  expect_equal(res$time1, 0)
  expect_equal(res$time2, 3)
})

test_that(".parse_time_intervals allow_unitless=FALSE rejects unitless value", {
  df <- data.frame(time = "0 to 3", stringsAsFactors = FALSE)
  expect_error(
    PriorRhythm:::.parse_time_intervals(df, "time", allow_unitless = FALSE),
    regexp = "0 to 3"
  )
})

test_that(".parse_time_intervals allow_unitless=FALSE accepts value with unit", {
  df  <- data.frame(time = "0 to 3 hr", stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time", allow_unitless = FALSE)
  expect_equal(res$time1, 0)
  expect_equal(res$time2, 3)
})

test_that(".parse_time_intervals explicit args override config$time_parser", {
  cfg <- study_config(time_parser = list(delimiter = "thru"))
  df  <- data.frame(time = "0 to 3 hr", stringsAsFactors = FALSE)
  # explicit delimiter="to" wins over config delimiter="thru"
  res <- PriorRhythm:::.parse_time_intervals(df, "time", delimiter = "to", config = cfg)
  expect_equal(res$time1, 0)
  expect_equal(res$time2, 3)
})

test_that(".parse_time_intervals config$time_parser is used when no explicit args", {
  cfg <- study_config(time_parser = list(delimiter = "-"))
  df  <- data.frame(time = "0 - 3", stringsAsFactors = FALSE)
  res <- PriorRhythm:::.parse_time_intervals(df, "time", config = cfg)
  expect_equal(res$time1, 0)
  expect_equal(res$time2, 3)
})

test_that(".parse_time_intervals places time1/time2 immediately after time_col", {
  df  <- data.frame(
    a    = 1:2,
    time = c("0 to 3 hr", "3 to 6 hr"),
    b    = 5:6,
    stringsAsFactors = FALSE
  )
  res  <- PriorRhythm:::.parse_time_intervals(df, "time")
  cols <- names(res)
  ti1  <- which(cols == "time")
  ti2  <- which(cols == "time1")
  ti3  <- which(cols == "time2")
  expect_equal(ti2, ti1 + 1L)
  expect_equal(ti3, ti1 + 2L)
})
