library(PriorRhythm)

# Build and package a prior bundle — internal workflow ----
# This script documents the Stage 1 prior-building workflow for advanced users
# who need to construct a custom compatible prior bundle. It is an internal
# prior-refresh workflow, not required for normal app use.
#
# For normal use, call run_app() with no arguments: the reviewed bundle
# shipped with the package is used automatically.
#
# Long-running steps are wrapped in if (FALSE) so this script is safely
# runnable top-to-bottom without executing side effects.

# Step 1: Load historical study files ----
# Replace study_dir with the path to your approved historical study files.
if (FALSE) {
  study_dir <- "/path/to/historical_study_files"

  study_files <- list.files(
    study_dir,
    pattern = "\\.(csv|tsv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  list_long_dat <- lapply(study_files, function(f) {
    PriorRhythm::load_study_data(f,
      animal_string = "monkey|dog",
      show_col_types = FALSE) |>
      PriorRhythm::time_bins(
        time_col = "time",
        delimiter = "to")
  })
  names(list_long_dat) <- tools::file_path_sans_ext(basename(study_files))
}

# Step 2: Build priors from historical data ----
if (FALSE) {
  list_historical_prior <- PriorRhythm::chain_priors_from_data(
    historical_data = list_long_dat,
    y_col = "q_tc",
    animal_col = "animal_code",
    treatment_col = "treatment_code",
    period_col = "period_code",
    time1_col = "time1",
    time2_col = "time2",
    alpha0_pop_mean = 250)
}

# Step 3: Slim the bundle — retain only pa draws and metadata ----
# Do NOT save raw historical long data into a distributable bundle.
if (FALSE) {
  slim_bundle <- lapply(list_historical_prior, function(species_res) {
    lapply(species_res, function(ep_res) {
      list(pa = ep_res$pa)
    })
  })
}

# Step 4: Save the slim prior bundle ----
# To update the package-distributed bundle, save to:
#   inst/extdata/priors/list_historical_prior.RDS
# To use a custom bundle locally, save anywhere and pass the path to run_app().
if (FALSE) {
  saveRDS(slim_bundle,
    file = "list_historical_prior.RDS",
    compress = "xz")
}

# Step 5: Launch the app with the custom bundle ----
if (FALSE) {
  PriorRhythm::run_app(prior_file = "list_historical_prior.RDS")
}
