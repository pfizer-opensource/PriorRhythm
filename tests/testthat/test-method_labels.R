library(testthat)
library(PriorRhythm)

test_that(".method_label_map maps all four internal IDs to display labels", {
  label_map <- PriorRhythm:::.method_label_map()

  expect_equal(unname(label_map[["Informative"]]), "Historical prior \u2014 full data")
  expect_equal(unname(label_map[["Informative_Incomplete"]]),
               "Historical prior \u2014 partial data")
  expect_equal(unname(label_map[["Non_informative"]]),
               "Weak-prior comparator \u2014 full data")
  expect_equal(unname(label_map[["Non_informative_Incomplete"]]),
               "Weak-prior comparator \u2014 partial data")
})

test_that(".method_label_map wraps the partial-data qualifier when requested", {
  label_map <- PriorRhythm:::.method_label_map(wrap = TRUE)

  expect_equal(unname(label_map[["Informative_Incomplete"]]),
               "Historical prior \u2014\npartial data")
  expect_equal(unname(label_map[["Non_informative_Incomplete"]]),
               "Weak-prior comparator \u2014\npartial data")
  expect_equal(unname(label_map[["Informative"]]),
               "Historical prior \u2014\nfull data")
})

test_that(".method_choices inverts the label map for selectors and plots", {
  choices <- PriorRhythm:::.method_choices()

  expect_equal(unname(choices[["Historical prior \u2014 full data"]]), "Informative")
  expect_equal(unname(choices[["Historical prior \u2014 partial data"]]),
               "Informative_Incomplete")
  expect_equal(unname(choices[["Weak-prior comparator \u2014 full data"]]),
               "Non_informative")
  expect_equal(unname(choices[["Weak-prior comparator \u2014 partial data"]]),
               "Non_informative_Incomplete")
})

test_that("plotting defaults reuse the shared method mapping", {
  expect_equal(formals(ci_plots)$input_method,
               quote(.method_choices(wrap = TRUE)))
  expect_equal(formals(prob_statement_plots)$input_method,
               quote(.method_choices(wrap = TRUE)))
})

test_that(".apply_method_labels displays labels and retains internal IDs", {
  dat <- data.frame(
    method   = c("Informative", "Informative_Incomplete",
                 "Non_informative", "Non_informative_Incomplete"),
    estimate = 1:4,
    stringsAsFactors = FALSE
  )

  result <- PriorRhythm:::.apply_method_labels(dat)

  expect_equal(result$method,
               c("Historical prior \u2014 full data",
                 "Historical prior \u2014 partial data",
                 "Weak-prior comparator \u2014 full data",
                 "Weak-prior comparator \u2014 partial data"))
  expect_equal(result$method_id, dat$method)
  expect_equal(names(result), c("method", "method_id", "estimate"))
  expect_equal(result$estimate, 1:4)
})

test_that(".apply_method_labels handles factor columns and unknown IDs", {
  dat <- data.frame(
    method = factor(c("Informative", "Custom_Method"),
                    levels = c("Informative", "Custom_Method"))
  )

  result <- PriorRhythm:::.apply_method_labels(dat)

  expect_equal(result$method, c("Historical prior \u2014 full data", "Custom_Method"))
  expect_equal(result$method_id, c("Informative", "Custom_Method"))
})

test_that(".apply_method_labels is a no-op without a method column", {
  dat <- data.frame(estimate = 1:2)

  result <- PriorRhythm:::.apply_method_labels(dat)

  expect_equal(result, dat)
  expect_null(PriorRhythm:::.apply_method_labels(NULL))
})
