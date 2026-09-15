library(testthat)
library(PriorRhythm)

make_valid_diagnostics_inventory <- function(status = "pass") {
  params <- c("alpha0", "A0", "beta0", "phi0")

  rhat <- switch(
    status,
    pass = stats::setNames(rep(1.001, length(params)), params),
    acceptable = stats::setNames(rep(1.05, length(params)), params),
    failed = stats::setNames(rep(1.2, length(params)), params),
    unassessed = stats::setNames(rep(NA_real_, length(params)), params)
  )

  ess <- if (identical(status, "unassessed")) {
    stats::setNames(rep(100, length(params)), params)
  } else {
    stats::setNames(rep(800, length(params)), params)
  }

  output <- list(
    parameters = params,
    rhat = rhat,
    ess = ess,
    status = status,
    policy = list(rhat_target = 1.01, rhat_acceptable = 1.1, ess_target = 400),
    assessed_at = "2026-01-01 00:00:00 UTC",
    package_version = "0.0.0.72"
  )

  return(output)
}

make_custom_inventory_bundle <- function() {

  output <- list(
    schema_version = 1L,
    metadata = list(created_at = "2026-01-01 00:00:00 UTC"),
    priors = list(
      dog = list(
        q_tc = list(
          pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
          diagnostics = make_valid_diagnostics_inventory("acceptable")
        )
      ),
      nhp = list(
        rr_i = list(
          pa = data.frame(alpha0 = 2, A0 = 2, beta0 = 2, phi0 = 2),
          diagnostics = make_valid_diagnostics_inventory("pass")
        )
      )
    )
  )

  return(output)
}

test_that("list_prior_bundle_endpoints returns expected shape for bundled priors", {
  inventory <- list_prior_bundle_endpoints()
  bundle <- PriorRhythm:::resolve_prior_bundle(NULL)
  
  expected_n <- sum(vapply(bundle$priors, length, integer(1)))
  
  expect_s3_class(inventory, "data.frame")
  expect_named(inventory, c("species", "endpoint", "convergence_status"))
  expect_equal(nrow(inventory), expected_n)
  expect_identical(
    anyDuplicated(paste(inventory$species, inventory$endpoint, sep = "::")),
    0L
  )
})

test_that("list_prior_bundle_endpoints preserves raw species and endpoint keys", {
  inventory <- list_prior_bundle_endpoints()

  if ("nhp" %in% names(PriorRhythm:::resolve_prior_bundle(NULL)$priors)) {
    expect_true("nhp" %in% inventory$species)
  }
  expect_false("monkey" %in% inventory$species)
})

test_that("list_prior_bundle_endpoints extracts convergence status from diagnostics", {
  tmp <- tempfile(fileext = ".RDS")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(make_custom_inventory_bundle(), tmp)

  inventory <- list_prior_bundle_endpoints(tmp)

  expect_equal(
    inventory$convergence_status[inventory$species == "dog" & inventory$endpoint == "q_tc"],
    "acceptable"
  )
  expect_equal(
    inventory$convergence_status[inventory$species == "nhp" & inventory$endpoint == "rr_i"],
    "pass"
  )
})

test_that("list_prior_bundle_endpoints supports compatible custom bundles", {
  tmp <- tempfile(fileext = ".RDS")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(make_custom_inventory_bundle(), tmp)

  inventory <- list_prior_bundle_endpoints(tmp)

  expect_equal(nrow(inventory), 2)
  expect_equal(unique(inventory$species), c("dog", "nhp"))
  expect_equal(unique(inventory$endpoint), c("q_tc", "rr_i"))
})

test_that("list_prior_bundle_endpoints output excludes confidential or posterior fields", {
  inventory <- list_prior_bundle_endpoints()

  expect_named(inventory, c("species", "endpoint", "convergence_status"), ignore.order = FALSE)
  expect_false(any(c("pa", "raw_data", "animal", "subject", "source_file") %in% names(inventory)))
})

test_that("list_prior_bundle_endpoints prior_file validation follows existing rules", {
  expect_error(
    list_prior_bundle_endpoints("/nonexistent/path/priors.RDS"),
    "not found"
  )

  expect_error(
    list_prior_bundle_endpoints(42L),
    "single non-empty character string"
  )
})

test_that(".build_endpoint_descriptions_candidate is deterministic and preserves reviewed annotations", {
  inventory <- data.frame(
    species = c("dog", "dog", "nhp"),
    endpoint = c("q_tc", "hr", "q_tc"),
    convergence_status = c("pass", "pass", "acceptable"),
    stringsAsFactors = FALSE
  )

  reviewed <- list(
    dog = list(
      label = "Dog",
      endpoints = list(
        q_tc = list(label = "QTc", unit = "ms", description = "Corrected QT interval"),
        stale_endpoint = list(label = "Stale", unit = "x", description = "stale")
      )
    ),
    rat = list(
      label = "Rat",
      endpoints = list()
    )
  )

  candidate_a <- PriorRhythm:::.build_endpoint_descriptions_candidate(
    bundle_inventory = inventory,
    reviewed_descriptions = reviewed
  )
  candidate_b <- PriorRhythm:::.build_endpoint_descriptions_candidate(
    bundle_inventory = inventory,
    reviewed_descriptions = reviewed
  )

  expect_identical(candidate_a, candidate_b)
  expect_equal(candidate_a$candidate$dog$label, "Dog")
  expect_equal(candidate_a$candidate$dog$endpoints$q_tc$label, "QTc")
  expect_equal(candidate_a$candidate$dog$endpoints$hr$label, "")
  expect_equal(candidate_a$candidate$nhp$endpoints$q_tc$unit, "")

  expect_true(any(
    candidate_a$new_keys$species == "dog" & candidate_a$new_keys$endpoint == "hr"
  ))
  expect_true(any(
    candidate_a$stale_keys$species == "dog" &
      candidate_a$stale_keys$endpoint == "stale_endpoint"
  ))
  expect_true(any(
    candidate_a$stale_keys$species == "rat" &
      is.na(candidate_a$stale_keys$endpoint)
  ))
  expect_true(any(
    candidate_a$incomplete_keys$species == "dog" &
      candidate_a$incomplete_keys$endpoint == "hr"
  ))
})
