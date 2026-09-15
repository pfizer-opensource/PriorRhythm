# tests/testthat/test-Study_Data_Formatting.R

library(testthat)
library(PriorRhythm)

test_that("import_historical_data errors unless exactly one input source is provided", {
  study_list <- list(study_a = data.frame(animal = 1, time = "0 to 1 hr"))

  expect_error(import_historical_data())
  expect_error(import_historical_data(
    data_file_list = study_list,
    data_path_dir = tempdir()
  ))
})

test_that("import_historical_data directory mode loads CSV and TSV files and adds study", {
  tmp_dir <- file.path(tempdir(), "hist_import_mixed")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  csv_dat <- data.frame(Animal = 1:2, Time = c("0 to 1 hr", "1 to 2 hr"), Value = c(10, 11))
  tsv_dat <- data.frame(Dog = 3:4, Time = c("0 to 1 hr", "1 to 2 hr"), Value = c(12, 13))
  readr::write_csv(csv_dat, file.path(tmp_dir, "study_csv.csv"))
  readr::write_tsv(tsv_dat, file.path(tmp_dir, "study_tsv.tsv"))
  result <- import_historical_data(data_path_dir = tmp_dir)
  expect_s3_class(result, "data.frame")
  expect_true("study" %in% names(result))
  expect_true(all(c("study_csv", "study_tsv") %in% unique(result$study)))
})

test_that("import_historical_data applies column_map consistently across formats", {
  tmp_dir <- file.path(tempdir(), "hist_import_colmap")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  csv_dat <- data.frame(
    Animal = 1:2,
    `Summary Period` = c("0 to 1 hr", "1 to 2 hr"),
    `A Mean` = c(10, 11)
  )
  readr::write_csv(csv_dat, file.path(tmp_dir, "study_csv.csv"))
  tsv_dat <- data.frame(
    Dog = 3:4,
    `Summary Period` = c("0 to 1 hr", "1 to 2 hr"),
    `A Mean` = c(12, 13)
  )
  readr::write_tsv(tsv_dat, file.path(tmp_dir, "study_tsv.tsv"))

  result <- import_historical_data(
    data_path_dir = tmp_dir,
    column_map = list(
      time = c("summary_period", "time"),
      activity = c("a_mean", "activity")
    )
  )

  expect_true("time" %in% names(result))
  expect_true("activity" %in% names(result))
})

test_that("import_historical_data preserves existing study column", {
  study_list <- list(
    file_name_stem = data.frame(
      study = c("my_study", "my_study"),
      animal = c(1, 2),
      time = c("0 to 1 hr", "1 to 2 hr")
    )
  )
  result <- import_historical_data(data_file_list = study_list)
  expect_true(all(result$study == "my_study"))
})

test_that("import_historical_data errors on unsupported extension", {
  tmp_dir <- file.path(tempdir(), "hist_import_unsupported")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("unsupported", file.path(tmp_dir, "study.txt"))
  expect_error(
    import_historical_data(
      data_path_dir = tmp_dir,
      file_pattern = "\\.txt$"
    )
  )
})

test_that("import_historical_data derives period_code from period when missing", {
  study_list <- list(
    study_a = data.frame(
      animal = c(1, 2, 3),
      period = c("Day 2", "Day 1", "Day 2"),
      time = c("0 to 1 hr", "0 to 1 hr", "1 to 2 hr")
    )
  )

  result <- import_historical_data(data_file_list = study_list) 
  expect_true("period_code" %in% names(result))
  expect_equal(result$period_code, c(1L, 2L, 1L))
})
