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

prob_plot_pos <- prob_statement_plots(test_out, threshold = 10)
print(prob_plot_pos)

prob_plot_0 <- prob_statement_plots(test_out, threshold = 0)
print(prob_plot_0)

prob_plot_neg <- prob_statement_plots(test_out, threshold = -5)
print(prob_plot_neg)

prob_plot_sub <- prob_statement_plots(
  test_out,
  threshold    = 10,
  input_method = c("Historical prior \u2014 full data" = "Informative")
)
print(prob_plot_sub)
