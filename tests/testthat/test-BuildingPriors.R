# tests/testthat/test-BuildingPriors.R

library(testthat)
library(PriorRhythm)

# chain_priors_from_data tests ----------------------------------------------

test_that("chain_priors_from_data errors when Assay_name is NULL", {
  dat <- make_long_dat(n_studies = 2)
  expect_error(
    chain_priors_from_data(dat = dat, assay_name = NULL)
  )
})

test_that("chain_priors_from_data_helper errors when Assay_name is NULL", {
  dat <- make_long_dat(n_studies = 2)
  expect_error(
    chain_priors_from_data_helper(dat = dat, assay_name = NULL)
  )
})

test_that("chain_priors_from_data_helper errors when Assay_name length > 1", {
  dat <- make_long_dat(n_studies = 2)
  expect_error(
    chain_priors_from_data_helper(dat = dat, assay_name = c("assay1", "assay2"))
  )
})

test_that("chain_priors_from_data_helper aborts when alpha0_pop_mean is NULL and no config", {
  dat <- make_long_dat(n_studies = 2, parameters = "assay1")
  expect_error(
    chain_priors_from_data_helper(
      dat = dat, assay_name = "assay1",
      animal_col = "animal_code", treatment_col = "treatment_code",
      study_col = "study_code", period_col = "period_code",
      assay_names_col = "parameter", assay_values_col = "value",
      alpha0_pop_mean = NULL
    ),
    "alpha0_pop_mean is NULL"
  )
})

test_that("chain_priors_from_data default arguments have correct values", {
  args <- formals(chain_priors_from_data)
  expect_null(args$dat)
  expect_null(args$assay_name)
  expect_equal(args$animal_col, "animal")
  expect_equal(args$treatment_col, "dose")
  expect_equal(args$n.iter.update, 1e4)
  expect_equal(args$n.iter.sample, 1e4)
  expect_equal(args$jags_seed, 8L)
  expect_null(args$alpha0_pop_mean)
  expect_null(args$cache_file)
  expect_equal(args$verbose, TRUE)
})

test_that("chain_priors_from_data_helper default arguments have correct values", {
  args <- formals(chain_priors_from_data_helper)
  expect_equal(args$jags_seed, 8L)
  expect_null(args$alpha0_pop_mean)
  expect_null(args$cache_file)
  expect_equal(args$verbose, TRUE)
})

test_that("Stage 1 functions do not expose Stan backend arguments", {
  helper_args  <- formals(chain_priors_from_data_helper)
  generic_args <- formals(chain_priors_from_data)
  default_args <- formals(chain_priors_from_data.default)
  checker_args <- formals(check_jags_inputs)

  expect_false("backend" %in% names(helper_args))
  expect_false("stan_seed" %in% names(helper_args))
  expect_false("backend" %in% names(generic_args))
  expect_false("stan_seed" %in% names(generic_args))
  expect_false("backend" %in% names(default_args))
  expect_false("stan_seed" %in% names(default_args))
  expect_false("backend" %in% names(checker_args))
  expect_false("stan_seed" %in% names(checker_args))
})

test_that("chain_priors_from_data.default errors when Assay_name is NULL", {
  dat <- make_long_dat(n_studies = 2)
  expect_error(
    chain_priors_from_data.default(dat = dat, assay_name = NULL)
  )
})

# MCMC tests (slow, skipped on CRAN) ----------------------------------------

test_that("chain_priors_from_data returns correct structure for single assay", {
  skip_on_cran()
  skip_if_not_installed("rjags")

  dat <- make_long_dat(n_studies = 2, n_animals = 2, treatments = 1:2,
                       periods = 1:2, time_bins = c("0 to 3 hr"),
                       parameters = "assay1")

  # Use minimal MCMC iterations for speed
  result <- chain_priors_from_data(
    dat = dat,
    animal_col = "animal_code",
    treatment_col = "treatment_code",
    study_col = "study_code",
    period_col = "period_code",
    assay_names_col = "parameter",
    assay_values_col = "value",
    assay_name = "assay1",
    n.iter.update = 100,
    n.iter.sample = 100,
    n.chains = 1,
    alpha0_pop_mean = 250
  )

  expect_type(result, "list")
  expect_true("assay1" %in% names(result))
  expect_true("pa" %in% names(result$assay1))
  expect_true("pm" %in% names(result$assay1))
  expect_true("rmse" %in% names(result$assay1))
  expect_true("r2" %in% names(result$assay1))
  expect_true("cors" %in% names(result$assay1))
  expect_true("y_obs" %in% names(result$assay1))
  expect_true("trace_plot" %in% names(result$assay1))
  expect_true("gof_plots" %in% names(result$assay1))
})

test_that("chain_priors_from_data_helper returns non-NaN Gelman-Rubin R-hat with n.chains = 2", {
  skip_on_cran()
  skip_if_not_installed("rjags")

  dat <- make_long_dat(n_studies = 2, n_animals = 2, treatments = 1:2,
                       periods = 1:2, time_bins = c("0 to 3 hr"),
                       parameters = "assay1")

  result <- chain_priors_from_data_helper(
    dat              = dat,
    assay_name       = "assay1",
    animal_col       = "animal_code",
    treatment_col    = "treatment_code",
    study_col        = "study_code",
    period_col       = "period_code",
    assay_names_col  = "parameter",
    assay_values_col = "value",
    n.iter.update    = 100,
    n.iter.sample    = 100,
    n.chains         = 2,
    jags_seed        = 8L,
    alpha0_pop_mean  = 250,
    verbose          = FALSE
  )

  # Minimal iterations suffice: we only need to detect NaN R-hat (a structural
  # bug), not validate good convergence.
  para_nam    <- c("alpha0", "A0", "beta0", "phi0")
  samps       <- rjags::coda.samples(result$m, variable.names = para_nam, n.iter = 100)
  chain_samples <- coda::as.mcmc.list(lapply(samps, function(ch) {
    coda::mcmc(as.matrix(ch)[, para_nam, drop = FALSE])
  }))
  gd   <- coda::gelman.diag(chain_samples, multivariate = FALSE)
  rhat <- gd$psrf[, "Point est."]
  expect_false(any(is.nan(rhat)), info = "Gelman-Rubin R-hat must not be NaN with n.chains = 2")
})



test_that("chain_prior_plot default arguments have correct values", {
  args <- formals(chain_prior_plot)
  expect_null(args$prior_list)
  expect_null(args$prior_name)
  expect_equal(args$doses, quote(seq(0, 3)))
})

test_that("chain_prior_plot returns a named list of two ggplot objects", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  n_samp <- 100
  pa <- data.frame(
    alpha0 = rnorm(n_samp, 40, 5),
    A0     = rnorm(n_samp, 0, 1),
    beta0  = rnorm(n_samp, 0, 1),
    phi0   = rnorm(n_samp, 0, 0.5)
  )

  prior_list <- list(assay1 = list(pa = pa))

  result <- chain_prior_plot(
    interval_start_times = c(0, 3),
    interval_end_times   = c(3, 6),
    doses                = seq(0, 2),
    prior_list           = prior_list,
    prior_name           = "assay1"
  )

  expect_type(result, "list")
  expect_named(result, c("time_v_prior", "timebins_v_prior"))
  expect_s3_class(result$time_v_prior,     "ggplot")
  expect_s3_class(result$timebins_v_prior, "ggplot")
})

# Cache file tests -----------------------------------------------------------

test_that("chain_priors_from_data_helper loads from cache when cache_file exists", {
  skip_if_not_installed("rjags")

  cache_path <- tempfile(fileext = ".rds")
  on.exit(unlink(cache_path), add = TRUE)

  set.seed(42)
  n_obs <- 10
  fake_mout <- data.frame(
    alpha0 = rnorm(100, 40, 5),
    A0     = rnorm(100, 0, 1),
    beta0  = rnorm(100, 0, 0.5),
    phi0   = rnorm(100, 0, 0.3)
  )
  for (i in seq_len(n_obs)) {
    fake_mout[[paste0("mu[", i, "]")]] <- rnorm(100, 40, 3)
  }
  fake_y_obs <- rnorm(n_obs, 40, 3)

  saveRDS(list(mout = fake_mout, y_obs = fake_y_obs), cache_path)

  dat <- make_long_dat(n_studies = 2, n_animals = 2, treatments = 1:2,
                       periods = 1:2, time_bins = "0 to 3 hr",
                       parameters = "assay1")

  result <- chain_priors_from_data_helper(
    dat             = dat,
    assay_name      = "assay1",
    animal_col      = "animal_code",
    treatment_col   = "treatment_code",
    study_col       = "study_code",
    period_col      = "period_code",
    assay_names_col = "parameter",
    assay_values_col = "value",
    alpha0_pop_mean = 250,
    cache_file      = cache_path,
    verbose         = FALSE
  )

  expect_type(result, "list")
  expect_true(all(c("pa", "pm", "rmse", "r2", "cors", "y_obs",
                    "trace_plot", "gof_plots") %in% names(result)))
  expect_null(result$m)
})

# New function argument tests ------------------------------------------------

test_that("exceedance_prob errors when mod is NULL", {
  expect_error(exceedance_prob(mod = NULL))
})

test_that("exceedance_prob default arguments have correct values", {
  args <- formals(exceedance_prob)
  expect_null(args$mod)
  expect_equal(args$threshold, 0)
})

test_that("rope_analysis errors when mod is NULL", {
  expect_error(rope_analysis(mod = NULL))
})

test_that("rope_analysis default arguments have correct values", {
  args <- formals(rope_analysis)
  expect_null(args$mod)
  expect_equal(args$threshold, 0)
})

# check_jags_indices tests ---------------------------------------------------

describe("check_jags_indices", {

  make_valid_input <- function() {
    list(
      a  = c(1L, 1L, 2L, 2L),
      b  = c(1L, 2L, 1L, 2L),
      s  = c(1L, 1L, 2L, 2L),
      na = 2L,
      nb = 2L,
      ns = 2L,
      N  = 4L,
      sa = c(1L, 2L)
    )
  }

  test_that("returns TRUE invisibly for valid input", {
    input <- make_valid_input()
    result <- withVisible(check_jags_indices(input))
    expect_true(result$value)
    expect_false(result$visible)
  })

  test_that("errors when 'a' contains NA", {
    input <- make_valid_input()
    input$a[[2]] <- NA_integer_
    expect_error(check_jags_indices(input), class = "rlang_error")
  })

  test_that("errors when 'a' has a gap", {
    input <- make_valid_input()
    input$a <- c(1L, 1L, 3L, 3L)
    expect_error(check_jags_indices(input), class = "rlang_error")
  })

  test_that("errors when 'b' has a gap", {
    input <- make_valid_input()
    input$b <- c(1L, 3L, 1L, 3L)
    expect_error(check_jags_indices(input), class = "rlang_error")
  })

  test_that("errors when 's' has a gap", {
    input <- make_valid_input()
    input$s <- c(1L, 1L, 3L, 3L)
    expect_error(check_jags_indices(input), class = "rlang_error")
  })

  test_that("errors when 'a' minimum is 0", {
    input <- make_valid_input()
    input$a <- c(0L, 0L, 1L, 1L)
    expect_error(check_jags_indices(input), class = "rlang_error")
  })

  test_that("errors when na != length(sa)", {
    input <- make_valid_input()
    input$sa <- c(1L, 2L, 1L)
    expect_error(check_jags_indices(input), class = "rlang_error")
  })

})
