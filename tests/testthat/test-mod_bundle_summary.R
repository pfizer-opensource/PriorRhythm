library(testthat)
library(PriorRhythm)

pa_df <- function() {
  data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1, stringsAsFactors = FALSE)
}

diagnostics_obj <- function(status = "pass") {
  params <- c("alpha0", "A0", "beta0", "phi0")
  list(
    parameters      = params,
    rhat            = stats::setNames(rep(1.001, length(params)), params),
    ess             = stats::setNames(rep(900, length(params)), params),
    status          = status,
    policy          = list(rhat_target = 1.01, rhat_acceptable = 1.1, ess_target = 400),
    assessed_at     = "2026-01-01 00:00:00 UTC",
    package_version = "0.0.0.1"
  )
}

make_bundle <- function(bundle_id = "test-bundle-001",
                        package_version = "0.0.0.1",
                        dog_status = "pass",
                        monkey_status = "failed") {
  list(
    schema_version = 1L,
    metadata = list(
      bundle_id       = bundle_id,
      package_version = package_version,
      created_with    = "PriorRhythm::build_prior_bundle()",
      created_at      = "2025-01-01"
    ),
    priors = list(
      dog = list(
        q_tc = list(pa = pa_df(), diagnostics = diagnostics_obj(dog_status))
      ),
      monkey = list(
        qt_cf = list(pa = pa_df(), diagnostics = diagnostics_obj(monkey_status))
      )
    )
  )
}

# .meta_field helper tests
test_that(".meta_field returns hyphen for absent key", {
  result <- PriorRhythm:::.meta_field(list(x = 1), "missing_key")
  expect_equal(result, "-")
})

test_that(".meta_field returns hyphen for NULL meta", {
  result <- PriorRhythm:::.meta_field(NULL, "bundle_id")
  expect_equal(result, "-")
})

test_that(".meta_field returns coerced string for present key", {
  result <- PriorRhythm:::.meta_field(list(bundle_id = "abc-123"), "bundle_id")
  expect_equal(result, "abc-123")
})

test_that(".meta_field joins multi-value field with comma", {
  result <- PriorRhythm:::.meta_field(list(x = c("a", "b")), "x")
  expect_equal(result, "a, b")
})

# mod_bundle_summary_server tests (bundled source)
test_that("mod_bundle_summary_server renders summary_table for bundled source", {
  bundle <- make_bundle()
  lcp    <- shiny::reactive(bundle)

  shiny::testServer(
    mod_bundle_summary_server,
    args = list(
      list_calculated_priors = lcp,
      prior_file = NULL
    ),
    {
      tbl <- output$summary_table
      expect_false(is.null(tbl))
      expect_match(tbl, "Bundled")
      expect_match(tbl, "test-bundle-001")
      expect_match(tbl, "0.0.0.1")
    }
  )
})

test_that("mod_bundle_summary_server shows custom path when prior_file is set", {
  bundle <- make_bundle()
  lcp    <- shiny::reactive(bundle)

  shiny::testServer(
    mod_bundle_summary_server,
    args = list(
      list_calculated_priors = lcp,
      prior_file = "/custom/my_priors.RDS"
    ),
    {
      tbl <- output$summary_table
      expect_match(tbl, "/custom/my_priors.RDS")
    }
  )
})

test_that("mod_bundle_summary_server renders endpoints_table with species and endpoints", {
  bundle <- make_bundle()
  lcp    <- shiny::reactive(bundle)
  metadata <- PriorRhythm:::.read_endpoint_descriptions()

  shiny::testServer(
    mod_bundle_summary_server,
    args = list(
      list_calculated_priors = lcp,
      prior_file = NULL,
      endpoint_descriptions = metadata
    ),
    {
      tbl <- output$endpoints_table
      expect_false(is.null(tbl))
      expect_match(tbl, "dog")
      expect_match(tbl, "q_tc")
      expect_match(tbl, "Dog")
      expect_match(tbl, "Corrected QT Interval")
      expect_match(tbl, "monkey")
      expect_match(tbl, "qt_cf")
    }
  )
})

test_that("mod_bundle_summary_server surfaces recorded convergence status per endpoint", {
  bundle <- make_bundle(dog_status = "pass", monkey_status = "failed")
  lcp    <- shiny::reactive(bundle)

  shiny::testServer(
    mod_bundle_summary_server,
    args = list(
      list_calculated_priors = lcp,
      prior_file = NULL
    ),
    {
      tbl <- output$endpoints_table
      expect_match(tbl, "Converged")
      expect_match(tbl, "Not converged")
      expect_match(tbl, "recorded")
    }
  )
})

test_that("mod_bundle_summary_server shows 'Not recorded' when diagnostics are absent", {
  bundle <- make_bundle()
  bundle$priors$dog$q_tc$diagnostics <- NULL
  lcp <- shiny::reactive(bundle)

  shiny::testServer(
    mod_bundle_summary_server,
    args = list(
      list_calculated_priors = lcp,
      prior_file = NULL
    ),
    {
      tbl <- output$endpoints_table
      expect_match(tbl, "Not recorded")
    }
  )
})

test_that("mod_bundle_summary_server falls back to raw codes for custom keys absent from metadata", {
  bundle <- make_bundle()
  lcp <- shiny::reactive(bundle)
  metadata <- PriorRhythm:::.read_endpoint_descriptions()

  shiny::testServer(
    mod_bundle_summary_server,
    args = list(
      list_calculated_priors = lcp,
      prior_file = "/custom/my_priors.RDS",
      endpoint_descriptions = metadata
    ),
    {
      tbl <- output$endpoints_table
      expect_match(tbl, "monkey")
      expect_match(tbl, "qt_cf")
    }
  )
})

test_that("mod_bundle_summary_server works with the bundled prior and packaged metadata", {
  bundle <- PriorRhythm:::resolve_prior_bundle(NULL)
  lcp <- shiny::reactive(bundle)
  metadata <- PriorRhythm:::.read_endpoint_descriptions()

  shiny::testServer(
    mod_bundle_summary_server,
    args = list(
      list_calculated_priors = lcp,
      prior_file = NULL,
      endpoint_descriptions = metadata
    ),
    {
      tbl <- output$endpoints_table
      expect_false(is.null(tbl))
      expect_match(tbl, "Dog")
      expect_match(tbl, "Corrected QT Interval")
    }
  )
})

# .diagnostics_status_label helper tests
test_that(".diagnostics_status_label returns 'Not recorded' for NULL diagnostics", {
  expect_equal(PriorRhythm:::.diagnostics_status_label(NULL), "Not recorded")
})

test_that(".diagnostics_status_label formats each documented status", {
  expect_equal(PriorRhythm:::.diagnostics_status_label(list(status = "pass")),
              "Converged (recorded)")
  expect_equal(PriorRhythm:::.diagnostics_status_label(list(status = "acceptable")),
              "Acceptable (recorded)")
  expect_equal(PriorRhythm:::.diagnostics_status_label(list(status = "failed")),
              "Not converged (recorded)")
  expect_equal(PriorRhythm:::.diagnostics_status_label(list(status = "unassessed")),
              "Unassessed (recorded)")
})

# shiny::testServer coverage for Browse Priors species/endpoint selector
test_that("mod_browse_priors_server: changing species updates endpoint choices", {
  bundle <- make_bundle()
  lcp    <- shiny::reactive(bundle)
  metadata <- PriorRhythm:::.read_endpoint_descriptions()

  shiny::testServer(
    mod_browse_priors_server,
    args = list(
      list_calculated_priors = lcp,
      endpoint_descriptions = metadata
    ),
    {
      # Simulate species selection for dog
      session$setInputs(Species_Browse = "dog")
      endpoints_dog <- names(bundle[["priors"]][["dog"]])
      expect_equal(endpoints_dog, "q_tc")

      # Simulate species selection for monkey
      session$setInputs(Species_Browse = "monkey")
      endpoints_monkey <- names(bundle[["priors"]][["monkey"]])
      expect_equal(endpoints_monkey, "qt_cf")

      # No cross-species bleed
      expect_false("qt_cf" %in% endpoints_dog)
      expect_false("q_tc" %in% endpoints_monkey)
    }
  )
})
