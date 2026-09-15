library(PriorRhythm)

SynHist <- system.file("extdata", "SyntheticHistorical",
  package = "PriorRhythm", mustWork = TRUE
)

Long_dat <- readRDS(file = file.path(SynHist, "Syn_long.RDS"))

# normal use
p_out <- Long_dat |>
  dplyr::filter(.data$study_code %in% c(1, 2)) |>
  chain_priors_from_data(
    treatment_col    = "treatment_code",
    study_col        = "study_code",
    animal_col       = "animal_code",
    period_col       = "period_code",
    assay_names_col  = "parameter",
    assay_name       = "assay1",
    alpha0_pop_mean = 250
  )

prior_plots <- chain_prior_plot(
  interval_start_times = c(0, 3, 6, 12, 18),
  interval_end_times   = c(3, 6, 12, 18, 24),
  doses                = seq(0, 3),
  prior_list           = p_out,
  prior_name           = "assay1"
)

cat("Plot names:", names(prior_plots), "\n")
print(prior_plots$time_v_prior)
print(prior_plots$timebins_v_prior)
