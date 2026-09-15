library(PriorRhythm)
library(readr)

# Create two simple data frames that use different column names for the
# same concepts, simulating files with inconsistent naming across studies.
study1 <- data.frame(
  animal         = 1:4,
  treatment_code = c(1L, 1L, 2L, 2L),
  period_code    = c(1L, 2L, 1L, 2L),
  summary_period = c("0 to 3 hr", "3 to 6 hr", "0 to 3 hr", "3 to 6 hr"),
  a_mean         = c(40.1, 42.3, 38.5, 44.0)
)

study2 <- data.frame(
  animal         = 1:4,
  treatment_code = c(1L, 1L, 2L, 2L),
  period_code    = c(1L, 2L, 1L, 2L),
  Time           = c("0 to 3 hr", "3 to 6 hr", "0 to 3 hr", "3 to 6 hr"),
  Activity       = c(39.0, 41.5, 37.8, 43.2)
)

study_list <- list(study_1 = study1, study_2 = study2)

# --- Option A: named list column map (multi-alias, first match wins) ---
dat_a <- import_historical_data(
  data_file_list = study_list,
  column_map     = list(
    time     = c("summary_period", "Time"),
    activity = c("a_mean", "Activity")
  )
)

# --- Option B: named character vector (backward-compatible, single alias) ---
dat_b <- import_historical_data(
  data_file_list = study_list,
  column_map     = c(time = "summary_period", activity = "a_mean")
)

# --- Option C: YAML file ---
tmp_yaml <- file.path(tempdir(), "col_map.yaml")
writeLines(c(
  "column_map:",
  "  time: [summary_period, Time]",
  "  activity: a_mean"
), tmp_yaml)
dat_c <- import_historical_data(
  data_file_list = study_list,
  column_map     = tmp_yaml
)
unlink(tmp_yaml)

# --- Option D: config object ---
cfg <- study_config(
  column_map = list(
    time     = c("summary_period", "Time"),
    activity = c("a_mean", "Activity")
  )
)
dat_d <- import_historical_data(data_file_list = study_list, config = cfg)

# --- Option E: directory import (CSV/TSV) ---
tmp_dir <- file.path(tempdir(), "example_hist_import")
dir.create(tmp_dir, showWarnings = FALSE)
readr::write_csv(study1, file.path(tmp_dir, "study_a.csv"))
readr::write_tsv(study2, file.path(tmp_dir, "study_b.tsv"))

dat_e <- import_historical_data(
  data_path_dir = tmp_dir,
  file_pattern  = "\\.(csv|tsv)$",
  config        = cfg
)

unlink(tmp_dir, recursive = TRUE)
