# tests/testthat/test-chain_model.R

library(testthat)
library(PriorRhythm)

test_that(".chain_model_jags is a non-empty character string", {
  expect_type(PriorRhythm:::.chain_model_jags, "character")
  expect_true(nchar(PriorRhythm:::.chain_model_jags) > 0)
})

test_that(".chain_model_jags contains expected JAGS model keywords", {
  expect_true(grepl("model\\{", PriorRhythm:::.chain_model_jags))
  expect_true(grepl("dnorm", PriorRhythm:::.chain_model_jags))
  expect_true(grepl("alpha0", PriorRhythm:::.chain_model_jags))
})

test_that("inst/config/mcmc_defaults.yaml is accessible and parseable", {
  skip_if_not_installed("yaml")
  path <- system.file("config", "mcmc_defaults.yaml", package = "PriorRhythm")
  expect_true(nchar(path) > 0, info = "mcmc_defaults.yaml not found in inst/config/")
  cfg <- yaml::read_yaml(path)
  expect_true("jags" %in% names(cfg))
  expect_true("mcmcglmm" %in% names(cfg))
})



