# tests/testthat/test-logo_path.R

library(testthat)
library(PriorRhythm)

test_that("get_package_logo_path is an exported function", {
  expect_true(is.function(get_package_logo_path))
})

test_that("get_package_logo_path has package parameter with 'PriorRhythm' default", {
  args <- formals(get_package_logo_path)
  expect_true("package" %in% names(args))
  expect_equal(args$package, "PriorRhythm")
})

test_that("get_package_logo_path returns a character string", {
  result <- get_package_logo_path()
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("get_package_logo_path returns path ending in logo.png when package is installed", {
  skip_if_not_installed("PriorRhythm")
  result <- get_package_logo_path()
  if (nchar(result) > 0) {
    expect_true(grepl("logo\\.png$", result))
  }
})
