library(testthat)
library(PriorRhythm)

# bayesian_test_example argument validation tests ---------------------------

test_that("bayesian_test_example default arguments have correct values", {
  args <- formals(bayesian_test_example)
  expect_null(args$input_tdat)
  expect_null(args$pa)
  expect_null(args$exclude_by_id)
  expect_equal(args$animal_col, "animal")
  expect_equal(args$treatment_col, "dose")
  expect_equal(args$n_exclude, 2)
  expect_equal(args$nitt, 15e3)
  expect_equal(args$thin, 5)
  expect_equal(args$burnin, 1e4)
  expect_equal(args$scale, 0.1)
  expect_equal(args$mcmc_seed, 1235L)
  expect_equal(args$seed, 732L)
})

# MCMC integration test (slow — skipped on CRAN) ----------------------------

test_that("bayesian_test_example returns correct output structure", {
  skip_on_cran()
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCglmm")

  # Build minimal synthetic data
  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr", "3 to 6 hr"))

  # Create a minimal prior by using a simple data.frame
  # (substituting a pre-made pa object with the expected columns)
  set.seed(42)
  n_samp <- 100
  pa <- data.frame(
    alpha0 = rnorm(n_samp, 40, 5),
    A0 = rnorm(n_samp, 0, 1),
    beta0 = rnorm(n_samp, 0, 1),
    phi0 = rnorm(n_samp, 0, 0.5)
  )

  result <- bayesian_test_example(
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

  expect_type(result, "list")
  expect_true("Choices_all" %in% names(result))
  expect_true("out" %in% names(result))
  expect_true(length(result$out) > 0)

  # Check model types in first time point
  expect_true("Non_informative" %in% names(result$out[[1]]))
  expect_true("Informative" %in% names(result$out[[1]]))
})

test_that("bayesian_test_example respects exclude_by_id argument", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")

  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr"))
  set.seed(42)
  n_samp <- 100
  pa <- data.frame(
    alpha0 = rnorm(n_samp, 40, 5),
    A0 = rnorm(n_samp, 0, 1),
    beta0 = rnorm(n_samp, 0, 1),
    phi0 = rnorm(n_samp, 0, 0.5)
  )

  # Should run without error when exclude_by_id is provided
  expect_no_error(
    bayesian_test_example(
      input_tdat = dat,
      animal_col = "animal_code",
      treatment_col = "treatment_code",
      period_col = "period_code",
      time1_col = "time1",
      time2_col = "time2",
      y_col = "value",
      exclude_by_id = "animal_1",
      nitt = 1500,
      thin = 1,
      burnin = 500,
      pa = pa
    )
  )
})

test_that("bayesian_test_example has animal_exclude argument with NULL default", {
  args <- formals(bayesian_test_example)
  expect_null(args$animal_exclude)
})

test_that("exceedance_prob returns correct structure with minimal mock mod", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")

  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr"))

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

  result <- exceedance_prob(mod, threshold = 5)
  expect_type(result, "list")
  expect_true(all(c("time_window", "p_exceed_weak_prior", "p_exceed_historical_prior") %in% names(result)))
  expect_equal(nrow(result), length(mod$out))
})

# rope_analysis argument validation tests ------------------------------------

test_that("rope_analysis default arguments are correct", {
  args <- formals(rope_analysis)
  expect_null(args$mod)
  expect_equal(args$threshold, 0)
})

test_that("rope_analysis aborts when mod is NULL", {
  expect_error(rope_analysis(mod = NULL), class = "rlang_error")
})

test_that("rope_analysis aborts when mod lacks required names", {
  expect_error(rope_analysis(mod = list(foo = 1)), class = "rlang_error")
})

# rope_analysis integration test (slow — skipped on CRAN) --------------------

test_that("rope_analysis returns correct structure", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")

  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr"))

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

  result <- rope_analysis(mod, threshold = 5)

  expect_type(result, "list")
  expect_true(all(c("model_A", "model_D") %in% names(result)))
  expect_length(result$model_A, length(mod$out))
  expect_length(result$model_D, length(mod$out))
})



# verifying time bin shuffling is controlled ------------------------------

test_that("time bin pairs are correct regardless of row order in tdat", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")
  
  dat_ordered <- make_test_dat(
    n_animals  = 4,
    treatments = 1:2,
    periods    = 1:2,
    time_bins  = c("0 to 3 hr", "3 to 6 hr",
                   "6 to 10.75 hr", "10.75 to 16.5 hr")
  )
  
  set.seed(99)
  dat_shuffled <- dat_ordered[sample(nrow(dat_ordered)), ]
  
  # Confirm the shuffle actually changed row order (test precondition)
  expect_false(identical(dat_ordered$time1, dat_shuffled$time1))
  
  # Also confirm that independent unique() on the shuffled data
  # would NOT preserve the correct (time1, time2) pairing —
  # i.e. the bug scenario is actually present in the shuffled input
  ut1_naive <- unique(dat_shuffled$time1)
  ut2_naive <- unique(dat_shuffled$time2)
  paired_naive    <- data.frame(time1 = ut1_naive, time2 = ut2_naive)
  paired_correct  <- dat_ordered |>
    dplyr::distinct(time1, time2) |>
    dplyr::arrange(time1)
  expect_false(identical(paired_naive, paired_correct),
  label = "shuffled unique() pairs must differ from correct pairs for this test to be meaningful")
  
  set.seed(42)
  pa <- data.frame(
    alpha0 = rnorm(100, 40, 5),
    A0     = rnorm(100, 0, 1),
    beta0  = rnorm(100, 0, 1),
    phi0   = rnorm(100, 0, 0.5)
  )
  
  res_ordered <- bayesian_test_example(
    input_tdat    = dat_ordered,
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
  
  set.seed(42)
  pa <- data.frame(
    alpha0 = rnorm(100, 40, 5),
    A0     = rnorm(100, 0, 1),
    beta0  = rnorm(100, 0, 1),
    phi0   = rnorm(100, 0, 0.5)
  )
  
  res_shuffled <- bayesian_test_example(
    input_tdat    = dat_shuffled,
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
  
  # Despite different row order, time bin names must be identical
  expect_equal(names(res_ordered$out), names(res_shuffled$out))
})
