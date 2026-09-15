# tests/testthat/test-report_generation.R

library(testthat)
library(PriorRhythm)

# generate_borrowing_report argument validation tests -----------------------

test_that("generate_borrowing_report default arguments have correct values", {
  args <- formals(generate_borrowing_report)
  expect_null(args$mod)
  expect_null(args$outfile)
  expect_equal(args$endpoint, "")
  expect_equal(args$endpoint_unit, "")
  expect_equal(args$threshold, 0)
  expect_equal(args$compound, "")
  expect_null(args$ut1)
  expect_null(args$ut2)
})

test_that("generate_borrowing_report errors when mod is NULL", {
  expect_error(generate_borrowing_report(mod = NULL, outfile = "test.docx"))
})

test_that("generate_borrowing_report errors when outfile is NULL", {
  expect_error(generate_borrowing_report(mod = list(), outfile = NULL))
})
