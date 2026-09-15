library(testthat)
library(PriorRhythm)

# ci_plots argument validation tests ----------------------------------------

test_that("ci_plots default arguments have correct values", {
  args <- formals(ci_plots)
  expect_null(args$mod)
  expect_equal(args$threshold, 0)
  expect_equal(args$animal_col, "Animal")
  expect_equal(args$treatment_col, "treatment_code")
  expect_equal(args$linewidth, 0.5)
})

test_that("prob_statement_plots default arguments have correct values", {
  args <- formals(prob_statement_plots)
  expect_null(args$mod)
  expect_equal(args$threshold, 0)
})

# Integration tests (slow — skipped on CRAN) --------------------------------

test_that("ci_plots returns a ggplot object", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")
  skip_if_not_installed("emmeans")

  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr", "3 to 6 hr"))

  set.seed(42)
  n_samp <- 100
  pa <- data.frame(
    alpha0 = rnorm(n_samp, 40, 5),
    A0 = rnorm(n_samp, 0, 1),
    beta0 = rnorm(n_samp, 0, 1),
    phi0 = rnorm(n_samp, 0, 0.5)
  )

  mod <- bayesian_test_example(
    input_tdat = dat,
    animal_col = "animal_code",
    treatment_col = "treatment_code",
    period_col = "period_code",
    time1_col = "time1",
    time2_col = "time2",
    y_col = "value",
    n_exclude = 1,
    nitt = 1500,
    thin = 1,
    burnin = 500,
    pa = pa
  )

  result <- ci_plots(
    mod = mod,
    animal_col = "animal_code",
    treatment_col = "treatment_code",
    period_col = "period_code",
    time1_col = "time1",
    time2_col = "time2",
    y_col = "y",
    threshold = 0
  )

  expect_s3_class(result, "ggplot")
})

test_that("prob_statement_plots returns a ggplot object", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")
  skip_if_not_installed("emmeans")

  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr", "3 to 6 hr"))

  set.seed(42)
  n_samp <- 100
  pa <- data.frame(
    alpha0 = rnorm(n_samp, 40, 5),
    A0 = rnorm(n_samp, 0, 1),
    beta0 = rnorm(n_samp, 0, 1),
    phi0 = rnorm(n_samp, 0, 0.5)
  )

  mod <- bayesian_test_example(
    input_tdat = dat,
    animal_col = "animal_code",
    treatment_col = "treatment_code",
    period_col = "period_code",
    time1_col = "time1",
    time2_col = "time2",
    y_col = "value",
    n_exclude = 1,
    nitt = 1500,
    thin = 1,
    burnin = 500,
    pa = pa
  )

  # Positive threshold
  result_pos <- prob_statement_plots(mod, threshold = 10,
    input_method = c("Non_informative"))
  expect_s3_class(result_pos, "ggplot")

  # Negative threshold
  result_neg <- prob_statement_plots(mod, threshold = -5,
    input_method = c("Non_informative"))
  expect_s3_class(result_neg, "ggplot")
})

test_that("ci_plots_combined default arguments have correct values", {
  args <- formals(ci_plots_combined)
  expect_null(args$mod)
  expect_equal(args$threshold, 0)
  expect_equal(args$endpoint, "")
  expect_equal(args$endpoint_unit, "")
  expect_equal(
    names(eval(args$model_colours)),
    c(
      "A: weak-prior comparator \u2014 full data (reference)",
      "B: weak-prior comparator \u2014 partial data",
      "C: historical prior \u2014 partial data",
      "D: historical prior \u2014 full data"
    )
  )
})

# ci_plots_combined integration test (slow — skipped on CRAN) ----------------

test_that("ci_plots_combined returns a ggplot object", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")
  skip_if_not_installed("emmeans")

  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr", "3 to 6 hr"))

  set.seed(42)
  n_samp <- 100
  pa <- data.frame(
    alpha0 = rnorm(n_samp, 40, 5),
    A0     = rnorm(n_samp, 0, 1),
    beta0  = rnorm(n_samp, 0, 1),
    phi0   = rnorm(n_samp, 0, 0.5)
  )

  mod <- bayesian_test_example(
    input_tdat    = dat,
    animal_col    = "animal_code",
    treatment_col = "treatment_code",
    period_col    = "period_code",
    time1_col     = "time1",
    time2_col     = "time2",
    y_col         = "value",
    n_exclude     = 1,
    nitt          = 1500,
    thin          = 1,
    burnin        = 500,
    pa            = pa
  )

  result <- ci_plots_combined(
    mod           = mod,
    threshold     = 10,
    treatment_col = "treatment_code",
    endpoint      = "HR",
    endpoint_unit = "bpm"
  )

  expect_s3_class(result, "ggplot")
  expect_equal(
    levels(result$data$model),
    c(
      "A: weak-prior comparator \u2014 full data (reference)",
      "B: weak-prior comparator \u2014 partial data",
      "C: historical prior \u2014 partial data",
      "D: historical prior \u2014 full data"
    )
  )
  expect_equal(
    levels(result$data$comparison),
    c(
      "A/B/C: partial data vs weak-prior full-data reference",
      "A/D: weak-prior comparator vs historical prior (full data)"
    )
  )
})

# Regression: default named input_method must not cause scale/data mismatch ----

test_that("ci_plots default named input_method produces no 'No shared levels' warning", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")
  skip_if_not_installed("emmeans")

  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr", "3 to 6 hr"))

  set.seed(42)
  n_samp <- 100
  pa <- data.frame(
    alpha0 = rnorm(n_samp, 40, 5),
    A0     = rnorm(n_samp, 0, 1),
    beta0  = rnorm(n_samp, 0, 1),
    phi0   = rnorm(n_samp, 0, 0.5)
  )

  mod <- bayesian_test_example(
    input_tdat    = dat,
    animal_col    = "animal_code",
    treatment_col = "treatment_code",
    period_col    = "period_code",
    time1_col     = "time1",
    time2_col     = "time2",
    y_col         = "value",
    n_exclude     = 1,
    nitt          = 1500,
    thin          = 1,
    burnin        = 500,
    pa            = pa
  )

  # Use the default named input_method (display label → internal ID)
  result <- expect_no_warning(
    ci_plots(
      mod           = mod,
      animal_col    = "animal_code",
      treatment_col = "treatment_code",
      period_col    = "period_code",
      time1_col     = "time1",
      time2_col     = "time2",
      y_col         = "y",
      threshold     = 0,
      input_method  = c(
        "Weak-prior comparator \u2014 full data" = "Non_informative",
        "Historical prior \u2014 full data"      = "Informative"
      )
    ),
    message = "No shared levels"
  )

  expect_s3_class(result, "ggplot")

  # Verify method column contains internal IDs, not display labels
  plot_data <- ggplot2::ggplot_build(result)$data[[1]]
  expect_false(any(c("Weak-prior comparator \u2014 full data",
                     "Historical prior \u2014 full data") %in%
                     unique(as.character(plot_data$colour))),
               info = "method column should contain internal IDs, not display labels")
})

test_that("ci_plots data keeps internal IDs while table labels match legend", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")
  skip_if_not_installed("emmeans")

  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr", "3 to 6 hr"))

  set.seed(42)
  n_samp <- 100
  pa <- data.frame(
    alpha0 = rnorm(n_samp, 40, 5),
    A0     = rnorm(n_samp, 0, 1),
    beta0  = rnorm(n_samp, 0, 1),
    phi0   = rnorm(n_samp, 0, 0.5)
  )

  mod <- bayesian_test_example(
    input_tdat    = dat,
    animal_col    = "animal_code",
    treatment_col = "treatment_code",
    period_col    = "period_code",
    time1_col     = "time1",
    time2_col     = "time2",
    y_col         = "value",
    n_exclude     = 1,
    nitt          = 1500,
    thin          = 1,
    burnin        = 500,
    pa            = pa
  )

  result <- ci_plots(
    mod           = mod,
    animal_col    = "animal_code",
    treatment_col = "treatment_code",
    period_col    = "period_code",
    time1_col     = "time1",
    time2_col     = "time2",
    y_col         = "y",
    threshold     = 0
  )

  plot_methods <- unique(as.character(result$data$method))
  expect_true(all(plot_methods %in% c("Informative", "Informative_Incomplete",
                                      "Non_informative",
                                      "Non_informative_Incomplete")))

  table_dat <- PriorRhythm:::.apply_method_labels(result$data)
  expect_true(all(unique(table_dat$method) %in%
                    c("Historical prior \u2014 full data",
                      "Historical prior \u2014 partial data",
                      "Weak-prior comparator \u2014 full data",
                      "Weak-prior comparator \u2014 partial data")))
  expect_equal(as.character(result$data$method), table_dat$method_id)
})
