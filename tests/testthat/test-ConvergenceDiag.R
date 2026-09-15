library(testthat)
library(PriorRhythm)

test_that("convergence_diag errors when stage1 is NULL", {
  expect_error(
    convergence_diag(NULL),
    "stage1"
  )
})

test_that("convergence_diag errors when mcmc_diag is NULL with informative message", {
  expect_error(
    convergence_diag(list(mcmc_diag = NULL)),
    "mcmc_diag"
  )
})

test_that("convergence_diag default arguments have correct values", {
  args <- formals(convergence_diag)
  expect_null(args$stage1)
  expect_equal(eval(args$parameters), c("alpha0", "A0", "beta0", "phi0"))
})

test_that("convergence_diag returns correct structure on a fresh fit", {
  skip_on_cran()
  skip_if_not_installed("rjags")

  dat <- make_long_dat(n_studies = 2, n_animals = 2, treatments = 1:2,
                       periods = 1:2, time_bins = c("0 to 3 hr"),
                       parameters = "assay1")

  stage1 <- chain_priors_from_data_helper(
    dat              = dat,
    assay_name       = "assay1",
    animal_col       = "animal_code",
    treatment_col    = "treatment_code",
    study_col        = "study_code",
    period_col       = "period_code",
    assay_names_col  = "parameter",
    assay_values_col = "value",
    n.iter.update    = 100L,
    n.iter.sample    = 100L,
    n.chains         = 2L,
    jags_seed        = 8L,
    alpha0_pop_mean  = 250,
    verbose          = FALSE
  )

  params <- c("alpha0", "A0", "beta0", "phi0")
  result <- convergence_diag(stage1, parameters = params)

  expect_type(result, "list")
  expect_named(result, c("rhat", "ess"))

  expect_true(is.matrix(result$rhat))
  expect_true(all(rownames(result$rhat) %in% params))

  expect_true(is.numeric(result$ess))
  expect_true(all(names(result$ess) %in% params))

  expect_false(any(is.nan(result$rhat)), info = "rhat must not contain NaN")
  expect_false(any(is.na(result$rhat)),  info = "rhat must not contain NA")
  expect_false(any(is.nan(result$ess)),  info = "ess must not contain NaN")
  expect_false(any(is.na(result$ess)),   info = "ess must not contain NA")
})

test_that("convergence_diag works after cache load and mcmc_diag is non-NULL", {
  skip_on_cran()
  skip_if_not_installed("rjags")

  dat <- make_long_dat(n_studies = 2, n_animals = 2, treatments = 1:2,
                       periods = 1:2, time_bins = c("0 to 3 hr"),
                       parameters = "assay1")

  cache_path <- tempfile(fileext = ".rds")
  on.exit(unlink(cache_path), add = TRUE)

  stage1_fresh <- chain_priors_from_data_helper(
    dat              = dat,
    assay_name       = "assay1",
    animal_col       = "animal_code",
    treatment_col    = "treatment_code",
    study_col        = "study_code",
    period_col       = "period_code",
    assay_names_col  = "parameter",
    assay_values_col = "value",
    n.iter.update    = 100L,
    n.iter.sample    = 100L,
    n.chains         = 2L,
    jags_seed        = 8L,
    alpha0_pop_mean  = 250,
    verbose          = FALSE,
    cache_file       = cache_path
  )

  stage1_cached <- chain_priors_from_data_helper(
    dat              = dat,
    assay_name       = "assay1",
    animal_col       = "animal_code",
    treatment_col    = "treatment_code",
    study_col        = "study_code",
    period_col       = "period_code",
    assay_names_col  = "parameter",
    assay_values_col = "value",
    n.iter.update    = 100L,
    n.iter.sample    = 100L,
    n.chains         = 2L,
    alpha0_pop_mean  = 250,
    verbose          = FALSE,
    cache_file       = cache_path
  )

  expect_false(is.null(stage1_cached$mcmc_diag),
               info = "mcmc_diag must be non-NULL when loading from cache")

  expect_no_error(convergence_diag(stage1_cached))
})

test_that("convergence_diag warns when any parameter has R-hat > 1.1", {
  skip_if_not_installed("coda")

  params <- c("alpha0", "A0")
  set.seed(1L)
  chain1 <- coda::mcmc(matrix(c(rnorm(200L, mean = 0,  sd = 0.01),
                                rnorm(200L, mean = 0,  sd = 0.01)),
                              ncol = 2L,
                              dimnames = list(NULL, params)))
  chain2 <- coda::mcmc(matrix(c(rnorm(200L, mean = 100, sd = 0.01),
                                rnorm(200L, mean = 0,   sd = 0.01)),
                              ncol = 2L,
                              dimnames = list(NULL, params)))
  bad_mcmc <- coda::as.mcmc.list(list(chain1, chain2))

  stage1_bad <- list(mcmc_diag = bad_mcmc)

  expect_warning(
    convergence_diag(stage1_bad, parameters = params),
    "Convergence not achieved"
  )
})
