library(PriorRhythm)

SynHist <- system.file("extdata", "SyntheticHistorical",
  package = "PriorRhythm", mustWork = TRUE
)

Long_dat <- readRDS(file = file.path(SynHist, "Syn_long.RDS"))
cat("Columns:", names(Long_dat), "\n")
cat("Studies:", unique(Long_dat$study_code), "\n")
cat("Parameters:", unique(Long_dat$parameter), "\n")

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

cat("Prior output names:", names(p_out), "\n")
cat("Posterior mean parameters:", names(p_out$assay1$pm), "\n")

# error handling
tryCatch(
  chain_priors_from_data(dat = Long_dat, assay_name = NULL),
  error = function(e) message(conditionMessage(e))
)
