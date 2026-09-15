# tests/testthat/helper-synthetic_data.R
# Shared helper functions for generating synthetic test data
# used across multiple test files.

#' Create a minimal long-format historical data frame
make_long_dat <- function(n_studies = 3, n_animals = 4,
                          treatments = 1:4, periods = 1:2,
                          time_bins = c("0 to 3 hr", "3 to 6 hr"),
                          parameters = c("assay1", "assay2")) {
  set.seed(42)
  rows <- list()
  for (s in seq_len(n_studies)) {
    for (a in seq_len(n_animals)) {
      for (trt in treatments) {
        for (per in periods) {
          for (tb in time_bins) {
            for (par in parameters) {
              rows[[length(rows) + 1]] <- data.frame(
                study_code = s,
                animal_code = paste0("s", s, "_a", a),
                time = tb,
                treatment_code = trt,
                period_code = per,
                parameter = par,
                value = rnorm(1, mean = 40, sd = 8),
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
    }
  }
  dat <- do.call(rbind, rows)

  # Split time bins into time1 / time2
  dat$time1 <- as.numeric(sub(" to.*", "", dat$time))
  dat$time2 <- as.numeric(sub(".*to ([0-9.]+).*", "\\1", dat$time))
  dat
}

#' Create a minimal wide-format test study data frame
make_test_dat <- function(n_animals = 8, treatments = 1:4,
                          periods = 1:2,
                          time_bins = c("0 to 3 hr", "3 to 6 hr")) {
  set.seed(42)
  rows <- list()
  for (a in seq_len(n_animals)) {
    for (trt in treatments) {
      for (per in periods) {
        for (tb in time_bins) {
          rows[[length(rows) + 1]] <- data.frame(
            animal_code = paste0("animal_", a),
            treatment_code = trt,
            period_code = per,
            time = tb,
            value = rnorm(1, mean = 40, sd = 7),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  dat <- do.call(rbind, rows)
  dat$time1 <- as.numeric(sub(" to.*", "", dat$time))
  dat$time2 <- as.numeric(sub(".*to ([0-9.]+).*", "\\1", dat$time))
  dat
}
