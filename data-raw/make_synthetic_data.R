# data-raw/make_synthetic_data.R
# Generates synthetic example datasets for PriorRhythm
# Run this script from the package root to regenerate inst/extdata/ RDS files.
#
# Usage:
#   Rscript data-raw/make_synthetic_data.R
#
# Outputs:
#   inst/extdata/SyntheticHistorical/Syn_long.RDS
#   inst/extdata/SyntheticTest/Syn_test.RDS

# library(PriorRhythm)
# library(dplyr)
# library(tidyr)
# library(purrr)

set.seed(42)

# Synthetic Historical Data -----------------------------------------------
# Build long-format historical data matching the expected column structure:
# study_code, animal_code, time, time1, time2,
# treatment_code, period_code, parameter, value

n_studies <- 5
n_animals <- 4
treatments <- 1:4
periods <- 1:2
time_bins <- c("0 to 3 hr", "3 to 6 hr", "6 to 12 hr", "12 to 18 hr", "18 to 24 hr")
parameters <- c("assay1", "assay2")

Syn_long <- purrr::map_dfr(seq_len(n_studies), function(s) {
  purrr::map_dfr(seq_len(n_animals), function(a) {
    purrr::map_dfr(treatments, function(trt) {
      purrr::map_dfr(periods, function(per) {
        purrr::map_dfr(time_bins, function(tb) {
          purrr::map_dfr(parameters, function(par) {
            data.frame(
              study_code = s,
              animal_code = paste0("s", s, "_a", a),
              time = tb,
              treatment_code = trt,
              period_code = per,
              parameter = par,
              value = rnorm(1, mean = 40 + trt * 2, sd = 8),
              stringsAsFactors = FALSE
            )
          })
        })
      })
    })
  })
}) %>%
  PriorRhythm::time_bins(time_col = "time", delimiter = "to") %>%
  dplyr::mutate(
    study_code = as.integer(.data$study_code),
    animal_code = as.character(.data$animal_code),
    treatment_code = as.integer(.data$treatment_code),
    period_code = as.integer(.data$period_code)
  )

cat("Syn_long dimensions:", nrow(Syn_long), "x", ncol(Syn_long), "\n")
cat("Columns:", names(Syn_long), "\n")
cat("Studies:", unique(Syn_long$study_code), "\n")
cat("Parameters:", unique(Syn_long$parameter), "\n")

# Save to inst/extdata/SyntheticHistorical/
hist_dir <- file.path("inst", "extdata", "SyntheticHistorical")
dir.create(hist_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(Syn_long, file = file.path(hist_dir, "Syn_long.RDS"))
cat("Saved:", file.path(hist_dir, "Syn_long.RDS"), "\n")


# Synthetic Test Study Data -----------------------------------------------
# Build wide-format test data for Bayesian_Test_Example()
# Expected columns: animal_code, treatment_code, period_code, time, time1, time2, value

n_test_animals <- 8
test_treatments <- 1:4
test_periods <- 1:3
test_time_bins <- c("0 to 3 hr", "3 to 6 hr", "6 to 12 hr", "12 to 18 hr", "18 to 24 hr")

Syn_test <- purrr::map_dfr(seq_len(n_test_animals), function(a) {
  purrr::map_dfr(test_treatments, function(trt) {
    purrr::map_dfr(test_periods, function(per) {
      purrr::map_dfr(test_time_bins, function(tb) {
        data.frame(
          animal_code = paste0("animal_", a),
          treatment_code = trt,
          period_code = per,
          time = tb,
          value = rnorm(1, mean = 40 + trt * 1.5, sd = 7),
          stringsAsFactors = FALSE
        )
      })
    })
  })
}) %>%
  PriorRhythm::time_bins(time_col = "time", delimiter = "to") %>%
  dplyr::mutate(
    animal_code = as.character(.data$animal_code),
    treatment_code = as.integer(.data$treatment_code),
    period_code = as.integer(.data$period_code)
  )

cat("\nSyn_test dimensions:", nrow(Syn_test), "x", ncol(Syn_test), "\n")
cat("Columns:", names(Syn_test), "\n")
cat("Animals:", length(unique(Syn_test$animal_code)), "\n")
cat("Treatments:", unique(Syn_test$treatment_code), "\n")
cat("Time bins:", unique(Syn_test$time), "\n")

# Save to inst/extdata/SyntheticTest/
test_dir <- file.path("inst", "extdata", "SyntheticTest")
dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(Syn_test, file = file.path(test_dir, "Syn_test.RDS"))
cat("Saved:", file.path(test_dir, "Syn_test.RDS"), "\n")

cat("\nDone! Synthetic data files created.\n")
