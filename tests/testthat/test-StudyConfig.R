library(testthat)
library(PriorRhythm)

# study_config() constructor tests -------------------------------------------

test_that("study_config returns object of class PriorRhythmConfig", {
  cfg <- study_config()
  expect_s3_class(cfg, "PriorRhythmConfig")
})

test_that("study_config default values are set correctly", {
  cfg <- study_config()
  expect_equal(cfg$file_pattern, "\\.(csv|tsv)$")
  expect_equal(cfg$animal_string, "monkey|dog")
  expect_null(cfg$column_map)
  expect_equal(cfg$endpoints, list())
  expect_null(cfg$active_endpoint)
})

test_that("study_config errors when column_map is not a character vector or named list", {
  expect_error(
    study_config(column_map = 42L),
    regexp = "column_map"
  )
})

test_that("study_config accepts a named list for column_map (multi-alias)", {
  cfg <- study_config(
    column_map = list(
      time     = c("summary_period", "Time"),
      activity = c("a_mean", "Activity"),
      temperature = "t_mean"
    )
  )
  expect_s3_class(cfg, "PriorRhythmConfig")
  expect_equal(cfg$column_map$time, c("summary_period", "Time"))
})

test_that("study_config errors when column_map is unnamed", {
  expect_error(
    study_config(column_map = c("summary_period", "a_mean")),
    regexp = "column_map"
  )
})

test_that("study_config errors when endpoints is not a named list", {
  expect_error(
    study_config(endpoints = c("qtc", "hr")),
    regexp = "endpoints"
  )
})

test_that("study_config errors when endpoints list is unnamed", {
  expect_error(
    study_config(endpoints = list(list(alpha0_pop_mean = 250))),
    regexp = "endpoints"
  )
})

test_that("study_config errors when an endpoint entry contains an unknown key", {
  expect_error(
    study_config(endpoints = list(qtc = list(alpha0_pop_mean = 250, bad_key = 1))),
    regexp = "bad_key"
  )
})

test_that("study_config errors when value_min >= value_max", {
  expect_error(
    study_config(endpoints = list(qtc = list(value_min = 400, value_max = 150))),
    regexp = "value_min"
  )
})

test_that("study_config errors when value_min equals value_max", {
  expect_error(
    study_config(endpoints = list(qtc = list(value_min = 200, value_max = 200))),
    regexp = "value_min"
  )
})

test_that("study_config errors when active_endpoint is not in endpoints", {
  expect_error(
    study_config(
      endpoints       = list(qtc = list(alpha0_pop_mean = 250)),
      active_endpoint = "hr"
    ),
    regexp = "active_endpoint"
  )
})

test_that("study_config errors when active_endpoint given but endpoints is empty", {
  expect_error(
    study_config(active_endpoint = "qtc"),
    regexp = "active_endpoint"
  )
})

test_that("study_config accepts valid active_endpoint in endpoints", {
  cfg <- study_config(
    endpoints       = list(qtc = list(alpha0_pop_mean = 250)),
    active_endpoint = "qtc"
  )
  expect_equal(cfg$active_endpoint, "qtc")
})

test_that("study_config stores endpoint parameters correctly", {
  cfg <- study_config(
    endpoints = list(
      qtc = list(alpha0_pop_mean = 250, value_min = 150, value_max = 400, threshold = 10)
    ),
    active_endpoint = "qtc"
  )
  expect_equal(cfg$endpoints$qtc$alpha0_pop_mean, 250)
  expect_equal(cfg$endpoints$qtc$value_min, 150)
  expect_equal(cfg$endpoints$qtc$value_max, 400)
  expect_equal(cfg$endpoints$qtc$threshold, 10)
})

test_that("study_config stores column_map correctly", {
  cfg <- study_config(column_map = c(time = "summary_period", activity = "a_mean"))
  expect_equal(cfg$column_map, c(time = "summary_period", activity = "a_mean"))
})

test_that("study_config stores list-valued column_map correctly", {
  cfg <- study_config(
    column_map = list(
      time     = c("summary_period", "Time", "time_bin"),
      activity = c("a_mean", "Activity"),
      temperature = "t_mean"
    )
  )
  expect_equal(cfg$column_map$time, c("summary_period", "Time", "time_bin"))
  expect_equal(cfg$column_map$activity, c("a_mean", "Activity"))
  expect_equal(cfg$column_map$temperature, "t_mean")
})

test_that("study_config errors when list column_map has non-character element", {
  expect_error(
    study_config(column_map = list(time = 42L, activity = "a_mean")),
    regexp = "column_map"
  )
})

test_that("study_config errors when list column_map has empty-character element", {
  expect_error(
    study_config(column_map = list(time = character(0))),
    regexp = "column_map"
  )
})

test_that("study_config errors when list column_map has unnamed elements", {
  expect_error(
    study_config(column_map = list("summary_period", activity = "a_mean")),
    regexp = "column_map"
  )
})

test_that("print.PriorRhythmConfig renders multi-alias column_map without error", {
  cfg <- study_config(
    column_map = list(
      time     = c("summary_period", "Time"),
      activity = "a_mean"
    )
  )
  expect_no_error(print(cfg))
})

test_that("print.PriorRhythmConfig runs without error", {
  cfg <- study_config(
    endpoints       = list(qtc = list(alpha0_pop_mean = 250, value_min = 150, value_max = 400)),
    active_endpoint = "qtc"
  )
  expect_no_error(print(cfg))
})

test_that("print.PriorRhythmConfig invisibly returns the config", {
  cfg <- study_config()
  result <- withVisible(print(cfg))
  expect_false(result$visible)
  expect_identical(result$value, cfg)
})


# write_study_config_template() tests ----------------------------------------

test_that("write_study_config_template creates a file at the given path", {
  tmp <- file.path(tempdir(), "test_config_write.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  expect_true(file.exists(tmp))
})

test_that("write_study_config_template produces valid YAML", {
  tmp <- file.path(tempdir(), "test_config_valid.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  parsed <- yaml::read_yaml(tmp)
  expect_type(parsed, "list")
})

test_that("write_study_config_template errors if file already exists without overwrite", {
  tmp <- file.path(tempdir(), "test_config_overwrite.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  expect_error(
    write_study_config_template(tmp),
    regexp = "already exists"
  )
})

test_that("write_study_config_template succeeds with overwrite = TRUE on existing file", {
  tmp <- file.path(tempdir(), "test_config_overwrite2.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  expect_no_error(write_study_config_template(tmp, overwrite = TRUE))
})

test_that("write_study_config_template YAML contains expected top-level keys", {
  tmp <- file.path(tempdir(), "test_config_keys.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  parsed <- yaml::read_yaml(tmp)
  expect_true("file_pattern"    %in% names(parsed))
  expect_true("animal_string"   %in% names(parsed))
  expect_true("column_map"      %in% names(parsed))
  expect_true("endpoints"       %in% names(parsed))
  expect_true("active_endpoint" %in% names(parsed))
})


# read_study_config() tests --------------------------------------------------

test_that("read_study_config returns a PriorRhythmConfig object", {
  tmp <- file.path(tempdir(), "test_read_config.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  cfg <- read_study_config(tmp)
  expect_s3_class(cfg, "PriorRhythmConfig")
})

test_that("read_study_config round-trips template values correctly", {
  tmp <- file.path(tempdir(), "test_roundtrip_config.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  cfg <- read_study_config(tmp)
  expect_equal(cfg$file_pattern, "\\.(csv|tsv)$")
  expect_equal(cfg$animal_string, "monkey|dog")
  expect_equal(cfg$active_endpoint, "qtc")
  expect_true("qtc" %in% names(cfg$endpoints))
  expect_equal(cfg$endpoints$qtc$alpha0_pop_mean, 250)
})

test_that("read_study_config round-trips column_map correctly", {
  tmp <- file.path(tempdir(), "test_colmap_config.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  cfg <- read_study_config(tmp)
  expect_true(is.character(cfg$column_map))
  expect_true(!is.null(names(cfg$column_map)))
  expect_equal(cfg$column_map[["time"]], "summary_period")
})

test_that("read_study_config round-trips multi-alias column_map correctly", {
  tmp <- file.path(tempdir(), "test_multi_alias_colmap.yaml")
  on.exit(unlink(tmp))
  writeLines(c(
    "column_map:",
    "  time: [summary_period, Time, time_bin]",
    "  activity: a_mean"
  ), tmp)
  cfg <- read_study_config(tmp)
  expect_true(is.list(cfg$column_map))
  expect_equal(cfg$column_map$time, c("summary_period", "Time", "time_bin"))
  expect_equal(cfg$column_map$activity, "a_mean")
})

test_that("read_study_config errors on non-existent path", {
  expect_error(
    read_study_config("/nonexistent/path/to/config.yaml"),
    regexp = "not found|File"
  )
})

test_that("read_study_config errors on invalid YAML content", {
  tmp <- file.path(tempdir(), "test_bad_yaml.yaml")
  on.exit(unlink(tmp))
  writeLines(c("key: valid", "bad: [unclosed"), tmp)
  expect_error(
    read_study_config(tmp),
    regexp = NULL
  )
})


# config_get_endpoint() tests ------------------------------------------------

test_that("config_get_endpoint returns correct sub-list when active_endpoint is set", {
  cfg <- study_config(
    endpoints       = list(qtc = list(alpha0_pop_mean = 250, value_min = 150)),
    active_endpoint = "qtc"
  )
  ep <- config_get_endpoint(cfg)
  expect_equal(ep$alpha0_pop_mean, 250)
  expect_equal(ep$value_min, 150)
})

test_that("config_get_endpoint returns list() when no active endpoint and no explicit endpoint", {
  cfg <- study_config(
    endpoints = list(qtc = list(alpha0_pop_mean = 250))
  )
  ep <- config_get_endpoint(cfg)
  expect_equal(ep, list())
})

test_that("config_get_endpoint returns list() when config is NULL", {
  ep <- config_get_endpoint(NULL)
  expect_equal(ep, list())
})

test_that("config_get_endpoint endpoint argument overrides active_endpoint", {
  cfg <- study_config(
    endpoints       = list(
      qtc = list(alpha0_pop_mean = 250),
      hr  = list(alpha0_pop_mean = 70)
    ),
    active_endpoint = "qtc"
  )
  ep <- config_get_endpoint(cfg, endpoint = "hr")
  expect_equal(ep$alpha0_pop_mean, 70)
})


# Integration with import_historical_data() tests ------------------------

test_that("config column_map renames columns in output", {
  skip_on_cran()

  tmp_dir <- file.path(tempdir(), "study_cfg_int_test")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  dat <- data.frame(
    Animal         = 1:4,
    Treatment_code = c(1, 2, 3, 4),
    Period_code    = c(1, 1, 2, 2),
    summary_period = c("0 to 3 hr", "0 to 3 hr", "3 to 6 hr", "3 to 6 hr"),
    value          = c(40, 45, 50, 55)
  )
  readr::write_csv(dat, file.path(tmp_dir, "Study_20AA050_dat.csv"))

  cfg <- study_config(
    column_map = c(time = "summary_period")
  )

  result <- import_historical_data(data_path_dir = tmp_dir, config = cfg)

  expect_true("time" %in% names(result))
  expect_false("summary_period" %in% names(result))
})

test_that("config animal_string overrides default when not explicitly provided", {
  skip_on_cran()

  tmp_dir <- file.path(tempdir(), "study_cfg_animal_test")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  dat <- data.frame(
    Animal         = 1:4,
    Treatment_code = c(1, 2, 3, 4),
    Period_code    = c(1, 1, 2, 2),
    value          = c(40, 45, 50, 55)
  )
  readr::write_csv(dat, file.path(tmp_dir, "Study_20AA050_dat.csv"))

  cfg <- study_config(animal_string = "rat|mouse")

  result <- import_historical_data(data_path_dir = tmp_dir, config = cfg)
  expect_s3_class(result, "data.frame")
})

test_that("explicit animal_string argument wins over config value", {
  skip_on_cran()

  tmp_dir <- file.path(tempdir(), "study_cfg_override_test")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  dat <- data.frame(
    Animal         = 1:4,
    Treatment_code = c(1, 2, 3, 4),
    Period_code    = c(1, 1, 2, 2),
    value          = c(40, 45, 50, 55)
  )
  readr::write_csv(dat, file.path(tmp_dir, "Study_20AA050_dat.csv"))

  cfg <- study_config(animal_string = "rat|mouse")

  result <- import_historical_data(
    data_path_dir = tmp_dir,
    animal_string = "monkey|dog",
    config        = cfg
  )
  expect_s3_class(result, "data.frame")
})


# time_parser in study_config() tests ----------------------------------------

test_that("study_config stores default time_parser when none provided", {
  cfg <- study_config()
  expect_equal(cfg$time_parser$delimiter, "to")
  expect_equal(cfg$time_parser$units, c("hr", "hours"))
  expect_true(cfg$time_parser$allow_unitless)
})

test_that("study_config merges user time_parser onto defaults", {
  cfg <- study_config(time_parser = list(delimiter = "-"))
  expect_equal(cfg$time_parser$delimiter, "-")
  expect_equal(cfg$time_parser$units, c("hr", "hours"))
  expect_true(cfg$time_parser$allow_unitless)
})

test_that("study_config accepts full custom time_parser", {
  cfg <- study_config(time_parser = list(
    delimiter      = "|",
    units          = c("min"),
    allow_unitless = FALSE
  ))
  expect_equal(cfg$time_parser$delimiter, "|")
  expect_equal(cfg$time_parser$units, "min")
  expect_false(cfg$time_parser$allow_unitless)
})

test_that("study_config errors on unknown time_parser key", {
  expect_error(
    study_config(time_parser = list(bad_key = TRUE)),
    regexp = "bad_key"
  )
})

test_that("study_config errors when time_parser delimiter is empty", {
  expect_error(
    study_config(time_parser = list(delimiter = "")),
    regexp = "delimiter"
  )
})

test_that("study_config errors when time_parser units contains empty string", {
  expect_error(
    study_config(time_parser = list(units = c("hr", ""))),
    regexp = "units"
  )
})

test_that("study_config errors when allow_unitless is not logical", {
  expect_error(
    study_config(time_parser = list(allow_unitless = "yes")),
    regexp = "allow_unitless"
  )
})

test_that("study_config errors when allow_unitless is NA", {
  expect_error(
    study_config(time_parser = list(allow_unitless = NA)),
    regexp = "allow_unitless"
  )
})

test_that("study_config errors when allow_unitless FALSE and units is empty", {
  expect_error(
    study_config(time_parser = list(units = character(0), allow_unitless = FALSE)),
    regexp = "units"
  )
})

test_that("study_config allows character(0) units when allow_unitless TRUE", {
  cfg <- study_config(time_parser = list(units = character(0), allow_unitless = TRUE))
  expect_equal(cfg$time_parser$units, character(0))
})

test_that("print.PriorRhythmConfig displays time_parser without error", {
  cfg <- study_config(time_parser = list(delimiter = "to"))
  expect_no_error(print(cfg))
})

# YAML round-trip for time_parser --------------------------------------------

test_that("write_study_config_template YAML contains time_parser block", {
  tmp <- file.path(tempdir(), "test_tp_keys.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  parsed <- yaml::read_yaml(tmp)
  expect_true("time_parser" %in% names(parsed))
  expect_equal(parsed$time_parser$delimiter, "to")
  expect_true(parsed$time_parser$allow_unitless)
})

test_that("read_study_config round-trips time_parser correctly", {
  tmp <- file.path(tempdir(), "test_tp_roundtrip.yaml")
  on.exit(unlink(tmp))
  write_study_config_template(tmp)
  cfg <- read_study_config(tmp)
  expect_equal(cfg$time_parser$delimiter, "to")
  expect_equal(cfg$time_parser$units, c("hr", "hours"))
  expect_true(cfg$time_parser$allow_unitless)
})

test_that("read_study_config round-trips custom time_parser", {
  tmp <- file.path(tempdir(), "test_tp_custom.yaml")
  on.exit(unlink(tmp))
  writeLines(c(
    "time_parser:",
    "  delimiter: \"-\"",
    "  units: [min]",
    "  allow_unitless: false"
  ), tmp)
  cfg <- read_study_config(tmp)
  expect_equal(cfg$time_parser$delimiter, "-")
  expect_equal(cfg$time_parser$units, "min")
  expect_false(cfg$time_parser$allow_unitless)
})
