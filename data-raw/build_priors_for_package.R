# loading historical data -------------------------------------------------
library(PriorRhythm)

rm(list = ls()); gc()
# .rs.restartR()
devtools::document(); devtools::load_all()

# historical data ---------------------------------------------------------
build_priors_cfg <- config::get(
  config = "build_priors",
  file = "~/PriorRhythm.yml"
)


# species-specific study configurations -----------------------------------
#
# alpha0_pop_mean values are from Dean's species × endpoint table in
# Development/realdata_benchmarking.R.
#
# Existing release-builder value filters and thresholds are retained unless
# a separately approved replacement is available.

dog_cfg <- study_config(
  column_map = c(
    activity    = "a_mean",
    temperature = "t_mean",
    time        = "summary_period"
  ),
  time_parser = list(
    delimiter      = "to",
    units          = c("hr", "hours"),
    allow_unitless = TRUE
  ),
  endpoints = list(
    q_tc = list(
      alpha0_pop_mean = 250,
      value_min = 150,
      value_max = 400
    ),
    hr = list(
      alpha0_pop_mean = 70,
      value_min = 30,
      value_max = 300
    ),
    sbp = list(alpha0_pop_mean = 80),
    dbp = list(alpha0_pop_mean = 60),
    rr_i = list(alpha0_pop_mean = 500),
    qrs_i = list(
      alpha0_pop_mean = 50,
      value_min = 20,
      value_max = 200
    )
  )
)

nhp_cfg <- study_config(
  column_map = c(
    activity    = "a_mean",
    temperature = "t_mean",
    time        = "summary_period"
  ),
  time_parser = list(
    delimiter      = "to",
    units          = c("hr", "hours"),
    allow_unitless = TRUE
  ),
  endpoints = list(
    q_tc = list(
      alpha0_pop_mean = 250,
      value_min = 150,
      value_max = 400
    ),
    hr = list(
      alpha0_pop_mean = 150,
      value_min = 30,
      value_max = 300
    ),
    sbp = list(alpha0_pop_mean = 80),
    dbp = list(alpha0_pop_mean = 60),
    rr_i = list(alpha0_pop_mean = 400),
    qrs_i = list(
      alpha0_pop_mean = 50,
      value_min = 20,
      value_max = 200
    )
  )
)


# The names must match build_priors_cfg$model_files exactly. Keep dog_cfg
# and nhp_cfg separate even when some current settings coincide: they are
# controlled species-specific configuration objects and may diverge.
species_configs <- list(
  dog = dog_cfg,
  nhp = nhp_cfg
)

configured_species <- names(build_priors_cfg$model_files)
missing_configs <- setdiff(configured_species, names(species_configs))
unused_configs <- setdiff(names(species_configs), configured_species)

if (length(missing_configs) > 0L || length(unused_configs) > 0L) {
  cli::cli_abort(c(
    "Species configuration and build-input names do not match.",
    "x" = "Missing configuration(s): {.val {missing_configs}}",
    "x" = "Configuration(s) without matching build input: {.val {unused_configs}}",
    "i" = "Ensure {.code names(species_configs)} exactly matches",
    " " = "{.code names(build_priors_cfg$model_files)}."
  ))
}

get_species_config <- function(species) {
  species_cfg <- species_configs[[species]]
  
  if (is.null(species_cfg)) {
    cli::cli_abort(
      "No study configuration is defined for species {.val {species}}."
    )
  }
  
  species_cfg
}


# importing data ----------------------------------------------------------

list_dat <- build_priors_cfg$model_files |>
  purrr::imap(function(species_files, species) {
    species_cfg <- get_species_config(species)
    
    import_historical_data(
      data_path_dir = file.path(
        build_priors_cfg$historical_data_path,
        species_files
      ),
      config = species_cfg
    ) |>
      time_bins(
        time_col = "time",
        config = species_cfg
      )
  })


# pivoting for prior build ------------------------------------------------

list_long_dat <- list_dat |>
  purrr::imap(function(species_dat, species) {
    species_cfg <- get_species_config(species)
    parameter_cols <- names(species_cfg$endpoints)
    
    species_dat |>
      droplevels() |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(parameter_cols),
        names_to = "parameter",
        values_to = "value",
        values_drop_na = TRUE
      ) |>
      dplyr::mutate(
        # import_historical_data() ensures this exists when `period` is
        # available; retain the canonical analytical identifier.
        period_code = as.numeric(.data$period_code)
      )
  })


list_long_dat |>
  purrr::iwalk(function(dat, species) {
    stopifnot(
      !anyNA(dat$period_code),
      !anyNA(dat$time1),
      !anyNA(dat$time2),
      all(dat$time2 > dat$time1)
    )
    
    message(species, ": historical model-input checks passed")
  })


# fit priors --------------------------------------------------------------

start_time <- Sys.time()

species_p_out1_list <- list_long_dat |>
  purrr::imap(function(species_dat, species) {
    species_cfg <- get_species_config(species)
    prior_selection <- names(species_cfg$endpoints)
    
    missing_endpoints <- setdiff(
      prior_selection,
      unique(species_dat$parameter)
    )
    
    if (length(missing_endpoints) > 0L) {
      cli::cli_abort(c(
        "Historical data are missing configured endpoint(s) for species {.val {species}}.",
        "x" = "Missing: {.val {missing_endpoints}}",
        "i" = "Configured endpoints: {.val {prior_selection}}"
      ))
    }
    
    list_x_long_dat <- species_dat |>
      plyr::dlply("parameter", identity) |>
      purrr::imap(~ .x |>
                    tibble::tibble() |>
                    dplyr::filter(!is.na(.data$value)) |>
                    dplyr::mutate(
                      variance = sd(.data$value),
                      .by = c("study"),
                      .before = 1
                    ) |>
                    dplyr::mutate(
                      study_rank = forcats::fct_reorder(
                        factor(.data$study),
                        .data$variance
                      ),
                      .before = 1
                    ) |>
                    dplyr::mutate(
                      animal_code = paste0(
                        as.numeric(.data$study_rank),
                        "-",
                        .data$animal
                      )
                    )
      )
    
    p_out1_list <- list_x_long_dat[prior_selection] |>
      purrr::imap(function(endpoint_dat, endpoint) {
        chain_priors_from_data(
          dat = endpoint_dat,
          treatment_col = "treatment_code",
          study_col = "study_rank",
          animal_col = "animal_code",
          period_col = "period_code",
          assay_names_col = "parameter",
          assay_name = endpoint,
          n.iter.sample = 2e4,
          config = species_cfg
        )[[endpoint]]
      })
    
    p_out1_list
  })


end_time <- Sys.time()
total_time <- end_time - start_time
total_time


# optional interactive inspection ----------------------------------------

# convergence_diag(species_p_out1_list$dog$sbp)
# convergence_diag(species_p_out1_list$dog$q_tc)
#
# species_p_out1_list$dog$sbp$trace_plot
# species_p_out1_list$dog$q_tc$trace_plot


# package output ----------------------------------------------------------

package_prior_dir <- file.path(
  getwd(),
  "inst",
  "extdata",
  "priors"
)

dir.create(
  package_prior_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

names(species_p_out1_list)


# slim down object and retain convergence evidence -------------------------
#
# make_runtime_prior_bundle() is package code (see
# R/BuildRuntimePriorBundle.R) — the internal, tested source of truth for
# runtime-bundle eligibility. This script only orchestrates fitting, calls
# the package helper, saves the resulting RDS, and validates the generated
# artifact.

runtime_priors <- PriorRhythm:::make_runtime_prior_bundle(species_p_out1_list)


# save priors -------------------------------------------------------------

saveRDS(
  runtime_priors,
  file = file.path(
    package_prior_dir,
    "list_historical_prior.RDS"
  ),
  compress = "xz"
)


# Validate the exact generated artifact before proceeding.
outPriors <- PriorRhythm:::resolve_prior_bundle(
  file.path(package_prior_dir, "list_historical_prior.RDS")
)

list.files(package_prior_dir)
