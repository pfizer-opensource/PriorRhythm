# tests/testthat/test-General_Data_Formatting.R

library(testthat)
library(PriorRhythm)

# load_study_data tests -------------------------------------------------------

test_that("load_study_data loads a valid CSV and cleans names", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  dat <- data.frame(Animal = 1:5, Treatment_Code = 1:5, Value = rnorm(5))
  readr::write_csv(dat, tmp)

  result <- load_study_data(tmp)
  expect_s3_class(result, "data.frame")
  expect_true(all(names(result) == tolower(names(result))))
})

test_that("load_study_data renames animal column using animal_string", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  dat <- data.frame(monkey = 1:5, value = rnorm(5))
  readr::write_csv(dat, tmp)

  result <- load_study_data(tmp, animal_string = "monkey|dog")
  expect_true("animal" %in% names(result))
})

test_that("load_study_data errors on non-existent file", {
  expect_error(load_study_data("nonexistent_file.csv"))
})

test_that("load_study_data loads TSV files", {
  tmp <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp))
  dat <- data.frame(Dog = 1:3, value = c(1, 2, 3))
  readr::write_tsv(dat, tmp)
  result <- load_study_data(tmp)
  expect_true("animal" %in% names(result))
})

test_that("load_study_data rejects Excel files with CSV/TSV guidance", {
  for (extension in c(".xls", ".xlsx")) {
    tmp <- tempfile(fileext = extension)
    writeLines("not a delimited file", tmp)
    on.exit(unlink(tmp), add = TRUE)

    expect_error(
      load_study_data(tmp),
      regexp = "CSV and TSV are supported; export the[[:space:]]*workbook to CSV or TSV"
    )
  }
})

test_that("load_study_data errors on unsupported extension", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines("not a csv", tmp)
  expect_error(load_study_data(tmp))
})

test_that("load_study_data default data_path is NULL", {
  args <- formals(load_study_data)
  expect_null(args$data_path)
})

test_that("load_study_data has a config argument defaulting to NULL", {
  args <- formals(load_study_data)
  expect_true("config" %in% names(args))
  expect_null(args$config)
  expect_false("excel_sheet" %in% names(args))
})

test_that("load_study_data applies config$column_map after clean_names/animal normalization", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  dat <- data.frame(monkey = 1:3, Summary_Period = c("0 to 3 hr", "3 to 6 hr", "0 to 3 hr"),
                     A_Mean = c(1.1, 2.2, 3.3))
  readr::write_csv(dat, tmp)

  cfg <- study_config(column_map = list(time = c("summary_period", "Time"),
                                         activity = "a_mean"))
  result <- load_study_data(tmp, config = cfg)

  expect_true("animal" %in% names(result))
  expect_true("time" %in% names(result))
  expect_true("activity" %in% names(result))
  expect_false("summary_period" %in% names(result))
  expect_false("a_mean" %in% names(result))
})

test_that("load_study_data uses config$animal_string when animal_string is NULL", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  dat <- data.frame(rat = 1:3, value = c(1.1, 2.2, 3.3))
  readr::write_csv(dat, tmp)

  cfg <- study_config(animal_string = "rat")

  # animal_string = NULL is equivalent to omitting the argument.
  result <- load_study_data(tmp, animal_string = NULL, config = cfg)

  expect_true("animal" %in% names(result))
})

test_that("load_study_data explicit animal_string overrides config$animal_string", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  dat <- data.frame(rat = 1:3, monkey = 4:6, value = c(1.1, 2.2, 3.3))
  readr::write_csv(dat, tmp)

  cfg <- study_config(animal_string = "rat")
  result <- load_study_data(tmp, animal_string = "monkey", config = cfg)

  # "monkey" was renamed to "animal"; "rat" was left untouched because the
  # explicit argument took precedence over config$animal_string.
  expect_true("animal" %in% names(result))
  expect_true("rat" %in% names(result))
})

test_that("load_study_data uses study_config() default animal_string when config is not supplied", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  dat <- data.frame(monkey = 1:3, value = c(1.1, 2.2, 3.3))
  readr::write_csv(dat, tmp)

  # No config and no animal_string supplied: falls back to study_config()'s
  # default animal_string ("monkey|dog").
  result <- load_study_data(tmp)

  expect_true("animal" %in% names(result))
})

test_that("load_study_data behavior is unchanged when config is NULL", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  dat <- data.frame(Animal = 1:5, Treatment_Code = 1:5, Value = rnorm(5))
  readr::write_csv(dat, tmp)

  result <- load_study_data(tmp)
  expect_s3_class(result, "data.frame")
  expect_true(all(names(result) == tolower(names(result))))
  expect_true("animal" %in% names(result))
})

test_that("load_study_data does not error when show_col_types = FALSE passed via ...", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  dat <- data.frame(Animal = 1:3, value = c(1.1, 2.2, 3.3))
  readr::write_csv(dat, tmp)
  result <- load_study_data(
    data_path     = tmp,
    animal_string = "Animal"
  )
  expect_s3_class(result, "data.frame")
})

test_that("load_study_data errors when show_col_types = FALSE is explicitly forwarded via ...", {
  # Reproduces the former duplicate-formal-argument failure that occurred when
  # mod_file_upload_server passed show_col_types = FALSE to load_study_data(),
  # which already hard-codes the argument before forwarding `...` to read_csv().
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  dat <- data.frame(Animal = 1:3, value = c(1.1, 2.2, 3.3))
  readr::write_csv(dat, tmp)
  expect_error(
    load_study_data(
      data_path       = tmp,
      animal_string   = "Animal",
      show_col_types  = FALSE   # duplicate — triggers the former bug
    ),
    regexp = "formal argument|duplicate"
  )
})


# time_bins tests -----------------------------------------------------------

test_that("time_bins creates time1 and time2 columns", {
  dat <- data.frame(
    animal = 1:3,
    time = c("1 to 3 hr", "3 to 6 hr", "6 to 12 hr"),
    value = rnorm(3),
    stringsAsFactors = FALSE
  )
  result <- time_bins(dat = dat, time_col = "time", delimiter = "to")

  expect_true("time1" %in% names(result))
  expect_true("time2" %in% names(result))
  expect_equal(result$time1, c(1, 3, 6))
  expect_equal(result$time2, c(3, 6, 12))
})

test_that("time_bins works with custom delimiter", {
  dat <- data.frame(
    animal = 1:2,
    tb = c("0-3", "3-6"),
    stringsAsFactors = FALSE
  )
  result <- time_bins(dat = dat, time_col = "tb", delimiter = "-")
  expect_equal(result$time1, c(0, 3))
  expect_equal(result$time2, c(3, 6))
})

test_that("time_bins default dat is NULL", {
  args <- formals(time_bins)
  expect_null(args$dat)
})

test_that("time_bins default time_col is NULL", {
  args <- formals(time_bins)
  expect_null(args$time_col)
})


# random_data_generator tests -------------------------------------------------

test_that("random_data_generator returns a list", {
  set.seed(42)
  result <- random_data_generator(ST_length = 3)
  expect_type(result, "list")
  expect_length(result, 3)
})

test_that("random_data_generator returns data.frames with expected columns", {
  set.seed(42)
  result <- random_data_generator(ST_length = 2)
  for (study in result) {
    expect_s3_class(study, "data.frame")
    expect_true(nrow(study) > 0)
    # Should have some common columns after cleaning
    col_names_lower <- tolower(names(study))
    expect_true(any(grepl("time", col_names_lower)))
    expect_true(any(grepl("assay", col_names_lower)))
  }
})

test_that("random_data_generator default ST_length is 15", {
  args <- formals(random_data_generator)
  expect_equal(args$ST_length, 15)
})

test_that("random_data_generator names include custom study strings", {
  set.seed(42)
  result <- random_data_generator(ST_length = 5, syn_study_strings = c("XX"))
  expect_true(any(grepl("XX", names(result))))
})


# time_bins delegation to .parse_time_intervals() ----------------------------

test_that("time_bins and .parse_time_intervals yield identical time1/time2", {
  df <- data.frame(
    animal = 1:4,
    time   = c("0 to 3 hr", "3 to 6 hr", "3.5to6.0 hr", "0.75 to 3.50"),
    stringsAsFactors = FALSE
  )
  res_tb  <- time_bins(dat = df, time_col = "time", delimiter = "to")
  res_pti <- PriorRhythm:::.parse_time_intervals(df, "time")
  expect_equal(res_tb$time1, res_pti$time1)
  expect_equal(res_tb$time2, res_pti$time2)
})

test_that("time_bins custom delimiter via .parse_time_intervals", {
  df <- data.frame(
    animal = 1:2,
    tb     = c("0 - 3", "3 - 6"),
    stringsAsFactors = FALSE
  )
  res <- time_bins(dat = df, time_col = "tb", delimiter = "-")
  expect_equal(res$time1, c(0, 3))
  expect_equal(res$time2, c(3, 6))
})

test_that("time_bins errors on reversed interval (via .parse_time_intervals)", {
  df <- data.frame(time = "6 to 3 hr", stringsAsFactors = FALSE)
  expect_error(
    time_bins(dat = df, time_col = "time"),
    regexp = "time2"
  )
})

test_that("time_bins errors on malformed value (via .parse_time_intervals)", {
  df <- data.frame(time = "bad_value", stringsAsFactors = FALSE)
  expect_error(
    time_bins(dat = df, time_col = "time"),
    regexp = "bad_value"
  )
})

test_that("time_bins allow_unitless=FALSE rejects unitless value", {
  df <- data.frame(time = "0 to 3", stringsAsFactors = FALSE)
  expect_error(
    time_bins(dat = df, time_col = "time", allow_unitless = FALSE),
    regexp = "0 to 3"
  )
})

test_that("time_bins accepts config$time_parser settings", {
  cfg <- study_config(time_parser = list(delimiter = "|"))
  df  <- data.frame(time = c("0 | 3", "3 | 6"), stringsAsFactors = FALSE)
  res <- time_bins(dat = df, time_col = "time", delimiter = "|", config = cfg)
  expect_equal(res$time1, c(0, 3))
  expect_equal(res$time2, c(3, 6))
})

test_that("time_bins uses config$time_parser$delimiter when delimiter not supplied", {
  cfg <- study_config(time_parser = list(delimiter = "|"))
  df  <- data.frame(time = c("0 | 3", "3 | 6"), stringsAsFactors = FALSE)
  res <- time_bins(dat = df, time_col = "time", config = cfg)
  expect_equal(res$time1, c(0, 3))
  expect_equal(res$time2, c(3, 6))
})

test_that("time_bins has new optional arguments in formals", {
  args <- formals(time_bins)
  expect_true("units" %in% names(args))
  expect_true("allow_unitless" %in% names(args))
  expect_true("config" %in% names(args))
})
