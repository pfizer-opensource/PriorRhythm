library(PriorRhythm)

SynHist <- system.file("extdata", "SyntheticHistorical",
  package = "PriorRhythm", mustWork = TRUE
)
Long_dat <- readRDS(file.path(SynHist, "Syn_long.RDS"))

stage1 <- Long_dat |>
  dplyr::filter(.data$study_code %in% c(1, 2)) |>
  chain_priors_from_data_helper(
    animal_col       = "animal_code",
    treatment_col    = "treatment_code",
    period_col       = "period_code",
    study_col        = "study_code",
    assay_names_col  = "parameter",
    assay_values_col = "value",
    assay_name       = "assay1",
    n.iter.update    = 2E2L,
    n.iter.sample    = 2E2L,
    n.chains         = 2L,
    verbose          = FALSE,
    alpha0_pop_mean = 250
  )

diag_out <- convergence_diag(stage1) |> 
  testthat::expect_warning()
