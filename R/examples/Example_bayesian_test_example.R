library(PriorRhythm)

SynHist <- system.file("extdata", "SyntheticHistorical",
  package = "PriorRhythm", mustWork = TRUE
)
SynTest <- system.file("extdata", "SyntheticTest",
  package = "PriorRhythm", mustWork = TRUE
)

Long_dat <- readRDS(file = file.path(SynHist, "Syn_long.RDS"))
Test_dat <- readRDS(file = file.path(SynTest, "Syn_test.RDS"))

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

test_out <- bayesian_test_example(
  input_tdat    = Test_dat,
  animal_col    = "animal_code",
  treatment_col = "treatment_code",
  period_col    = "period_code",
  time1_col     = "time1",
  time2_col     = "time2",
  y_col         = "value",
  n_exclude     = 2,
  pa            = p_out$assay1$pa
)

cat("Output structure:", names(test_out), "\n")
cat("Time points:", names(test_out$out), "\n")
cat("Model types per time point:", names(test_out$out[[1]]), "\n")

# error handling
tryCatch(
  bayesian_test_example(input_tdat = NULL),
  error = function(e) message(conditionMessage(e))
)
