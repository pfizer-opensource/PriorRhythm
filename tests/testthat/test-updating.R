test_that("chain_priors_from_data_helper respects n.iter.update (burn-in fix)", {
  # Regression test for the n.iter.update -> n.iter bug in stats::update().
  # Before the fix, stats::update(m, n.iter.update = x) silently ignored x
  # because rjags::update() uses n.iter, not n.iter.update.
  # Strategy: expose the JAGS model object via stage1$m and check m$iter(),
  # which rjags increments by exactly n.iter.update after update().
  skip_on_cran()
  skip_if_not_installed("rjags")
  
  dat <- make_long_dat(
    n_studies  = 2,
    n_animals  = 2,
    treatments = 1:2,
    periods    = 1:2,
    time_bins  = "0 to 3 hr",
    parameters = "assay1"
  )
  
  common_args <- list(
    dat              = dat,
    assay_name       = "assay1",
    animal_col       = "animal_code",
    treatment_col    = "treatment_code",
    study_col        = "study_code",
    period_col       = "period_code",
    assay_names_col  = "parameter",
    assay_values_col = "value",
    n.iter.sample    = 50L,
    n.chains         = 2L,
    jags_seed        = 8L,
    alpha0_pop_mean  = 250,
    verbose          = FALSE
  )
  
  n_update_short <- 10L
  n_update_long  <- 500L
  
  fit_short <- do.call(chain_priors_from_data_helper,
                       c(common_args, list(n.iter.update = n_update_short)))
  fit_long  <- do.call(chain_priors_from_data_helper,
                       c(common_args, list(n.iter.update = n_update_long)))
  
  iter_short <- fit_short$m$iter()
  iter_long  <- fit_long$m$iter()
  
  cat(sprintf("\n  n.iter.update=%d:  m$iter() = %d (expected %d)\n",
              n_update_short, iter_short, 1000L + n_update_short + 50L))
  cat(sprintf("  n.iter.update=%d: m$iter() = %d (expected %d)\n",
              n_update_long,  iter_long,  1000L + n_update_long  + 50L))
  
  jags_adapt <- 1000L  # rjags::jags.model() always runs 1000 adaptation steps
  
  expect_equal(iter_short, jags_adapt + n_update_short + 50L,
               label = "m$iter() must equal adapt + n.iter.update + n.iter.sample (short)")
  expect_equal(iter_long,  jags_adapt + n_update_long  + 50L,
               label = "m$iter() must equal adapt + n.iter.update + n.iter.sample (long)")
  
  # Difference cancels adaptation constant — cleanest isolation of n.iter.update
  expect_equal(iter_long - iter_short, n_update_long - n_update_short,
               label = "difference in m$iter() must equal difference in n.iter.update")
})