library(testthat)
library(PriorRhythm)

# Helper: build a two-species bundle with disjoint endpoint sets
make_disjoint_bundle <- function() {
  pa_df <- function() {
    data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1)
  }
  list(
    schema_version = 1L,
    metadata = list(),
    priors = list(
      dog = list(
        q_tc = list(pa = pa_df())
      ),
      monkey = list(
        qt_cf = list(pa = pa_df())
      )
    )
  )
}

test_that("species-specific endpoint lookup returns correct endpoints for each species", {
  bundle <- make_disjoint_bundle()
  priors_tree <- bundle[["priors"]]

  dog_endpoints <- names(priors_tree[["dog"]])
  monkey_endpoints <- names(priors_tree[["monkey"]])

  expect_equal(dog_endpoints, "q_tc")
  expect_equal(monkey_endpoints, "qt_cf")

  # Endpoints are disjoint
  expect_length(intersect(dog_endpoints, monkey_endpoints), 0L)
})

test_that("selecting dog species does not expose monkey-only endpoint qt_cf", {
  bundle <- make_disjoint_bundle()
  priors_tree <- bundle[["priors"]]

  selected_species <- "dog"
  choices_for_species <- names(priors_tree[[selected_species]])

  expect_false("qt_cf" %in% choices_for_species)
  expect_true("q_tc" %in% choices_for_species)
})

test_that("selecting monkey species does not expose dog-only endpoint q_tc", {
  bundle <- make_disjoint_bundle()
  priors_tree <- bundle[["priors"]]

  selected_species <- "monkey"
  choices_for_species <- names(priors_tree[[selected_species]])

  expect_false("q_tc" %in% choices_for_species)
  expect_true("qt_cf" %in% choices_for_species)
})

test_that("accessing an unavailable species/endpoint combination returns NULL", {
  bundle <- make_disjoint_bundle()
  priors_tree <- bundle[["priors"]]

  # Simulates what happens when prior_analysis = 'qt_cf' but species_analysis = 'dog'
  result <- priors_tree[["dog"]][["qt_cf"]]
  expect_null(result)
})

test_that("species with no endpoints returns NULL from names()", {
  bundle <- list(
    schema_version = 1L,
    metadata = list(),
    priors = list(
      dog = list()
    )
  )
  priors_tree <- bundle[["priors"]]
  choices <- names(priors_tree[["dog"]])
  expect_null(choices)
})
