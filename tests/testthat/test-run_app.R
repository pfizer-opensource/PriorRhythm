# tests/testthat/test-run_app.R

library(testthat)
library(PriorRhythm)

# ---------------------------------------------------------------------------
# run_app signature tests
# ---------------------------------------------------------------------------

test_that("run_app is an exported function", {
  expect_true(is.function(run_app))
})

test_that("run_app has prior_file parameter with NULL default", {
  args <- formals(run_app)
  expect_true("prior_file" %in% names(args))
  expect_null(args$prior_file)
})

test_that("run_app does not have deprecated app_support_dat parameter", {
  args <- formals(run_app)
  expect_false("app_support_dat" %in% names(args))
})

test_that("run_app is accessible from the package namespace", {
  expect_true(isNamespaceLoaded("PriorRhythm"))
  expect_true(exists("run_app", envir = asNamespace("PriorRhythm")))
})

# ---------------------------------------------------------------------------
# app_server signature tests
# ---------------------------------------------------------------------------

test_that("app_server has prior_file parameter with NULL default", {
  args <- formals(app_server)
  expect_true("prior_file" %in% names(args))
  expect_null(args$prior_file)
})

test_that("app_server does not have deprecated app_support_dat parameter", {
  args <- formals(app_server)
  expect_false("app_support_dat" %in% names(args))
})

# ---------------------------------------------------------------------------
# resolve_prior_bundle tests — uses test fixtures, not production data
# ---------------------------------------------------------------------------

make_valid_diagnostics <- function(status = "pass", rhat = NULL, ess = NULL, policy = NULL) {
  params <- c("alpha0", "A0", "beta0", "phi0")
  if (is.null(rhat)) {
    rhat <- switch(status,
      pass       = stats::setNames(rep(1.001, length(params)), params),
      acceptable = stats::setNames(rep(1.05, length(params)), params),
      failed     = stats::setNames(rep(1.5, length(params)), params),
      unassessed = stats::setNames(rep(NA_real_, length(params)), params)
    )
  }
  if (is.null(ess)) {
    ess <- stats::setNames(rep(900, length(params)), params)
  }
  if (is.null(policy)) {
    policy <- list(rhat_target = 1.01, rhat_acceptable = 1.1, ess_target = 400)
  }
  list(
    parameters      = params,
    rhat            = rhat,
    ess             = ess,
    status          = status,
    policy          = policy,
    assessed_at     = "2026-01-01 00:00:00 UTC",
    package_version = "0.0.0.63"
  )
}

make_valid_bundle <- function() {
  list(
    dog = list(
      q_tc = list(
        pa          = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
        diagnostics = make_valid_diagnostics()
      )
    )
  )
}

test_that("resolve_prior_bundle errors when prior_file does not exist", {
  expect_error(
    resolve_prior_bundle("/nonexistent/path/priors.RDS"),
    "not found"
  )
})

test_that("resolve_prior_bundle errors for non-character prior_file", {
  expect_error(
    resolve_prior_bundle(42L),
    "single non-empty character string"
  )
})

test_that("resolve_prior_bundle errors for empty string prior_file", {
  expect_error(
    resolve_prior_bundle(""),
    "single non-empty character string"
  )
})

test_that("resolve_prior_bundle loads a valid custom bundle", {
  tmp <- tempfile(fileext = ".RDS")
  saveRDS(make_valid_bundle(), tmp)
  on.exit(unlink(tmp))

  result <- resolve_prior_bundle(tmp)
  expect_named(result, c("schema_version", "metadata", "priors"))
  expect_named(result$priors, "dog")
  expect_named(result$priors$dog, "q_tc")
  expect_true(is.data.frame(result$priors$dog$q_tc$pa))
})

# ---------------------------------------------------------------------------
# validate_prior_bundle tests
# ---------------------------------------------------------------------------

test_that("validate_prior_bundle returns normalised form for minimal valid bundle", {
  bundle <- make_valid_bundle()
  result <- validate_prior_bundle(bundle)
  expect_named(result, c("schema_version", "metadata", "priors"))
  expect_named(result$priors, "dog")
  expect_named(result$priors$dog, "q_tc")
  expect_true(is.data.frame(result$priors$dog$q_tc$pa))
})

test_that("validate_prior_bundle errors when bundle is not a named list", {
  expect_error(validate_prior_bundle(NULL), "non-empty named list")
  expect_error(validate_prior_bundle(list()), "non-empty named list")
  expect_error(validate_prior_bundle(42), "non-empty named list")
})

test_that("validate_prior_bundle errors when species entry is not a named list", {
  bundle <- list(dog = "not_a_list")
  expect_error(validate_prior_bundle(bundle), "named list of endpoints")
})

test_that("validate_prior_bundle errors when pa is missing", {
  bundle <- list(dog = list(q_tc = list(no_pa = 1)))
  expect_error(validate_prior_bundle(bundle), "pa")
})

test_that("validate_prior_bundle errors when pa is not a data frame", {
  bundle <- list(dog = list(q_tc = list(pa = c(1, 2, 3))))
  expect_error(validate_prior_bundle(bundle), "data frame")
})

test_that("validate_prior_bundle errors when pa is missing required columns", {
  bundle <- list(dog = list(q_tc = list(
    pa = data.frame(alpha0 = 1, A0 = 1)  # missing beta0, phi0
  )))
  expect_error(validate_prior_bundle(bundle), "beta0|phi0|missing")
})

test_that("validate_prior_bundle errors when diagnostics is missing", {
  bundle <- list(dog = list(q_tc = list(
    pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1)
  )))
  expect_error(validate_prior_bundle(bundle), "diagnostics")
})

test_that("validate_prior_bundle errors when pa contains confidential columns", {
  bundle <- list(dog = list(q_tc = list(
    pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1, animal = "X123"),
    diagnostics = make_valid_diagnostics()
  )))
  expect_error(validate_prior_bundle(bundle), "confidential|animal")
})

test_that("validate_prior_bundle accepts bundles with extra permitted columns in pa", {
  bundle <- list(dog = list(q_tc = list(
    pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1, sigma = 0.5),
    diagnostics = make_valid_diagnostics()
  )))
  expect_no_error(validate_prior_bundle(bundle))
})

test_that("validate_prior_bundle accepts multi-species multi-endpoint bundle", {
  bundle <- list(
    dog = list(
      q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
                 diagnostics = make_valid_diagnostics()),
      hr   = list(pa = data.frame(alpha0 = 2, A0 = 2, beta0 = 2, phi0 = 2),
                 diagnostics = make_valid_diagnostics())
    ),
    monkey = list(
      q_tc = list(pa = data.frame(alpha0 = 3, A0 = 3, beta0 = 3, phi0 = 3),
                 diagnostics = make_valid_diagnostics())
    )
  )
  result <- validate_prior_bundle(bundle)
  expect_named(result$priors, c("dog", "monkey"))
})

# ---------------------------------------------------------------------------
# Metadata / versioned-envelope regression tests
# ---------------------------------------------------------------------------

test_that("validate_prior_bundle: legacy flat shape with top-level schema_version", {
  # This is the shape the reviewed bundled RDS uses:
  # top-level metadata fields alongside species keys.
  bundle <- list(
    schema_version = 1L,
    metadata = list(bundle_id = "test-bundle", created_at = "2026-01-01"),
    dog = list(
      q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
                 diagnostics = make_valid_diagnostics())
    )
  )
  result <- validate_prior_bundle(bundle)
  expect_named(result, c("schema_version", "metadata", "priors"))
  expect_equal(result$schema_version, 1L)
  expect_equal(result$metadata$bundle_id, "test-bundle")
  expect_named(result$priors, "dog")
  expect_true(is.data.frame(result$priors$dog$q_tc$pa))
})

test_that("validate_prior_bundle: versioned-envelope shape with priors key", {
  bundle <- list(
    schema_version = 1L,
    metadata = list(created_with = "PriorRhythm 0.0.0.60"),
    priors = list(
      dog = list(
        q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
                   diagnostics = make_valid_diagnostics())
      )
    )
  )
  result <- validate_prior_bundle(bundle)
  expect_named(result, c("schema_version", "metadata", "priors"))
  expect_equal(result$schema_version, 1L)
  expect_named(result$priors, "dog")
  expect_true(is.data.frame(result$priors$dog$q_tc$pa))
})

test_that("validate_prior_bundle: metadata-only top-level keys do not fail as species", {
  # A bundle with only recognized metadata keys and no species-like keys
  # (other than via priors envelope) should not misidentify metadata as species.
  bundle <- list(
    schema_version = 1L,
    bundle_id = "b-001",
    package_version = "0.0.0.60",
    priors = list(
      rat = list(
        hr = list(pa = data.frame(alpha0 = 5, A0 = 5, beta0 = 5, phi0 = 5),
                  diagnostics = make_valid_diagnostics())
      )
    )
  )
  result <- validate_prior_bundle(bundle)
  expect_equal(result$schema_version, 1L)
  expect_named(result$priors, "rat")
})

test_that("validate_prior_bundle: legacy flat bundle preserves extra metadata keys", {
  bundle <- list(
    schema_version = 1L,
    created_at = "2026-08-01",
    dog = list(
      q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
                 diagnostics = make_valid_diagnostics())
    )
  )
  result <- validate_prior_bundle(bundle)
  # metadata key 'created_at' is captured in the top-level bundle but not
  # in metadata sub-list since it is a recognized top-level key
  expect_named(result$priors, "dog")
  expect_false("created_at" %in% names(result$priors))
})

test_that("resolve_prior_bundle: versioned-envelope RDS loads and normalises", {
  tmp <- tempfile(fileext = ".RDS")
  versioned_bundle <- list(
    schema_version = 1L,
    metadata = list(bundle_id = "versioned-test"),
    priors = list(
      dog = list(
        q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
                   diagnostics = make_valid_diagnostics())
      )
    )
  )
  saveRDS(versioned_bundle, tmp)
  on.exit(unlink(tmp))

  result <- resolve_prior_bundle(tmp)
  expect_named(result, c("schema_version", "metadata", "priors"))
  expect_equal(result$schema_version, 1L)
  expect_named(result$priors, "dog")
})

test_that("resolve_prior_bundle: legacy flat RDS with metadata keys loads correctly", {
  tmp <- tempfile(fileext = ".RDS")
  flat_bundle <- list(
    schema_version = 1L,
    dog = list(
      q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
                 diagnostics = make_valid_diagnostics())
    )
  )
  saveRDS(flat_bundle, tmp)
  on.exit(unlink(tmp))

  result <- resolve_prior_bundle(tmp)
  expect_equal(result$schema_version, 1L)
  expect_named(result$priors, "dog")
})

test_that("validate_prior_bundle: flat bundle with top-level metadata fields preserves them in metadata", {
  pa_df <- data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1)
  flat_bundle <- list(
    schema_version = 1L,
    bundle_id      = "test-bundle-001",
    created_at     = "2026-01-01",
    created_with   = "build_priors v1",
    package_version = "0.0.0.64",
    dog = list(q_tc = list(pa = pa_df, diagnostics = make_valid_diagnostics()))
  )

  result <- validate_prior_bundle(flat_bundle, path = "<test>")

  expect_equal(result$schema_version, 1L)
  expect_named(result$priors, "dog")
  expect_equal(result$metadata[["bundle_id"]],      "test-bundle-001")
  expect_equal(result$metadata[["created_at"]],     "2026-01-01")
  expect_equal(result$metadata[["created_with"]],   "build_priors v1")
  expect_equal(result$metadata[["package_version"]], "0.0.0.64")
})

test_that("validate_prior_bundle: nested metadata wins over top-level on key conflict", {
  pa_df <- data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1)
  flat_bundle <- list(
    schema_version = 1L,
    bundle_id      = "top-level-value",
    metadata       = list(bundle_id = "nested-value"),
    dog            = list(q_tc = list(pa = pa_df, diagnostics = make_valid_diagnostics()))
  )

  result <- validate_prior_bundle(flat_bundle, path = "<test>")
  expect_equal(result$metadata[["bundle_id"]], "nested-value")
})

test_that("validate_prior_bundle: flat bundle with no top-level scalar metadata returns NULL metadata", {
  pa_df <- data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1)
  flat_bundle <- list(
    schema_version = 1L,
    dog = list(q_tc = list(pa = pa_df, diagnostics = make_valid_diagnostics()))
  )

  result <- validate_prior_bundle(flat_bundle, path = "<test>")
  expect_null(result$metadata)
})

test_that("validate_prior_bundle: versioned envelope preserves metadata unchanged", {
  pa_df <- data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1)
  versioned_bundle <- list(
    schema_version = 1L,
    metadata = list(bundle_id = "env-001", created_at = "2026-06-01"),
    priors = list(dog = list(q_tc = list(pa = pa_df, diagnostics = make_valid_diagnostics())))
  )

  result <- validate_prior_bundle(versioned_bundle, path = "<test>")
  expect_equal(result$metadata[["bundle_id"]], "env-001")
  expect_equal(result$metadata[["created_at"]], "2026-06-01")
})

# Confidential data outside pa ----

test_that("validate_prior_bundle: confidential data frame nested outside pa is rejected", {
  bundle <- list(
    schema_version = 1L,
    metadata = list(),
    priors = list(
      dog = list(
        q_tc = list(
          pa              = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
          diagnostics     = make_valid_diagnostics(),
          historical_data = data.frame(animal = "A001", value = 250)
        )
      )
    )
  )
  expect_error(
    validate_prior_bundle(bundle, path = "<test>"),
    regexp = "Confidential field"
  )
})

test_that("validate_prior_bundle: confidential field in deeply nested list outside pa is rejected", {
  bundle <- list(
    schema_version = 1L,
    metadata = list(),
    priors = list(
      dog = list(
        q_tc = list(
          pa          = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
          diagnostics = c(
            make_valid_diagnostics(),
            list(raw = data.frame(subject = "S01", value = 1.2))
          )
        )
      )
    )
  )
  expect_error(
    validate_prior_bundle(bundle, path = "<test>"),
    regexp = "Confidential field"
  )
})

test_that("validate_prior_bundle: non-confidential extra endpoint component is accepted", {
  bundle <- list(
    schema_version = 1L,
    metadata = list(),
    priors = list(
      dog = list(
        q_tc = list(
          pa          = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
          diagnostics = c(
            make_valid_diagnostics(),
            list(n_draws = 4000L, converged = TRUE)
          )
        )
      )
    )
  )
  result <- validate_prior_bundle(bundle, path = "<test>")
  expect_named(result$priors, "dog")
})

test_that(".find_confidential_in_obj: returns empty for clean object", {
  obj <- list(x = data.frame(alpha0 = 1), y = list(z = 42))
  confidential_fields <- c("animal", "subject", "study", "source_file", "raw_data")
  result <- PriorRhythm:::.find_confidential_in_obj(obj, confidential_fields)
  expect_equal(result, character(0))
})

test_that(".find_confidential_in_obj: detects confidential column in nested data frame", {
  obj <- list(nested = data.frame(study = "ABC", value = 1))
  confidential_fields <- c("animal", "subject", "study", "source_file", "raw_data")
  result <- PriorRhythm:::.find_confidential_in_obj(obj, confidential_fields)
  expect_true("study" %in% result)
})

# schema_version enforcement for versioned-envelope bundles
valid_pa <- data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1)
valid_priors <- list(dog = list(q_tc = list(pa = valid_pa, diagnostics = make_valid_diagnostics())))

test_that("validate_prior_bundle: missing schema_version in envelope aborts", {
  bundle <- list(priors = valid_priors)
  expect_error(
    validate_prior_bundle(bundle, path = "<test>"),
    regexp = "missing.*schema_version|schema_version.*missing"
  )
})

test_that("validate_prior_bundle: character schema_version in envelope aborts", {
  bundle <- list(schema_version = "1", priors = valid_priors)
  expect_error(
    validate_prior_bundle(bundle, path = "<test>"),
    regexp = "scalar integer"
  )
})

test_that("validate_prior_bundle: vector schema_version in envelope aborts", {
  bundle <- list(schema_version = c(1L, 2L), priors = valid_priors)
  expect_error(
    validate_prior_bundle(bundle, path = "<test>"),
    regexp = "scalar integer"
  )
})

test_that("validate_prior_bundle: unsupported future schema_version aborts", {
  bundle <- list(schema_version = 999L, priors = valid_priors)
  expect_error(
    validate_prior_bundle(bundle, path = "<test>"),
    regexp = "Unsupported.*schema_version|schema_version.*999"
  )
})

test_that("validate_prior_bundle: valid schema_version 1L in envelope succeeds", {
  bundle <- list(
    schema_version = 1L,
    metadata = list(bundle_id = "test-bundle"),
    priors = valid_priors
  )
  result <- validate_prior_bundle(bundle, path = "<test>")
  expect_equal(result$schema_version, 1L)
})

# Confidential named list keys outside pa (non-data-frame components)
test_that("validate_prior_bundle: raw_data as named numeric vector at endpoint level is rejected", {
  bundle <- list(
    schema_version = 1L,
    metadata = list(),
    priors = list(
      dog = list(
        q_tc = list(
          pa          = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
          diagnostics = make_valid_diagnostics(),
          raw_data    = c(250, 260, 270)
        )
      )
    )
  )
  expect_error(
    validate_prior_bundle(bundle, path = "<test>"),
    regexp = "Confidential field"
  )
})

test_that("validate_prior_bundle: source_file as character at endpoint level is rejected", {
  bundle <- list(
    schema_version = 1L,
    metadata = list(),
    priors = list(
      dog = list(
        q_tc = list(
          pa          = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
          diagnostics = make_valid_diagnostics(),
          source_file = "/confidential/study_data.csv"
        )
      )
    )
  )
  expect_error(
    validate_prior_bundle(bundle, path = "<test>"),
    regexp = "Confidential field"
  )
})

test_that(".find_confidential_in_obj: detects confidential named list key (non-data-frame)", {
  obj <- list(raw_data = c(250, 260, 270), other = 1L)
  confidential_fields <- c("animal", "subject", "study", "source_file", "raw_data")
  result <- PriorRhythm:::.find_confidential_in_obj(obj, confidential_fields)
  expect_true("raw_data" %in% result)
})

test_that(".find_confidential_in_obj: detects source_file as named list key", {
  obj <- list(source_file = "/path/to/data.csv", n = 100L)
  confidential_fields <- c("animal", "subject", "study", "source_file", "raw_data")
  result <- PriorRhythm:::.find_confidential_in_obj(obj, confidential_fields)
  expect_true("source_file" %in% result)
})

# ---------------------------------------------------------------------------
# .validate_diagnostics tests
# ---------------------------------------------------------------------------

test_that(".validate_diagnostics accepts a well-formed diagnostics object", {
  expect_no_error(
    PriorRhythm:::.validate_diagnostics(
      make_valid_diagnostics(),
      species = "dog", endpoint = "q_tc", path = "<test>"
    )
  )
})

test_that(".validate_diagnostics errors when diagnostics is NULL", {
  expect_error(
    PriorRhythm:::.validate_diagnostics(NULL, species = "dog", endpoint = "q_tc"),
    "diagnostics"
  )
})

test_that(".validate_diagnostics errors when diagnostics is not a named list", {
  expect_error(
    PriorRhythm:::.validate_diagnostics(c(1, 2, 3), species = "dog", endpoint = "q_tc"),
    "named list"
  )
})

test_that(".validate_diagnostics errors when required fields are missing", {
  diag <- make_valid_diagnostics()
  diag$status <- NULL
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "status"
  )
})

test_that(".validate_diagnostics errors when rhat names do not match parameters", {
  diag <- make_valid_diagnostics()
  names(diag$rhat) <- c("alpha0", "A0", "beta0", "wrong_param")
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "rhat"
  )
})

test_that(".validate_diagnostics errors when ess is not numeric", {
  diag <- make_valid_diagnostics()
  diag$ess <- c(alpha0 = "bad", A0 = "bad", beta0 = "bad", phi0 = "bad")
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "ess"
  )
})

test_that(".validate_diagnostics errors when status is not a recognized value", {
  diag <- make_valid_diagnostics()
  diag$status <- "converged"
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "status"
  )
})

test_that(".validate_diagnostics accepts each documented status value", {
  for (s in c("pass", "acceptable", "failed", "unassessed")) {
    diag <- make_valid_diagnostics(status = s)
    expect_no_error(
      PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc")
    )
  }
})

test_that(".validate_diagnostics errors when policy is missing required fields", {
  diag <- make_valid_diagnostics()
  diag$policy <- list(rhat_target = 1.01)
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "policy"
  )
})

test_that(".validate_diagnostics errors when policy rhat_target is non-finite", {
  diag <- make_valid_diagnostics(
    policy = list(rhat_target = Inf, rhat_acceptable = 1.1, ess_target = 400)
  )
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "finite"
  )
})

test_that(".validate_diagnostics errors when policy rhat_acceptable is non-finite", {
  diag <- make_valid_diagnostics(
    policy = list(rhat_target = 1.01, rhat_acceptable = -Inf, ess_target = 400)
  )
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "finite"
  )
})

test_that(".validate_diagnostics errors when policy ess_target is non-finite", {
  diag <- make_valid_diagnostics(
    policy = list(rhat_target = 1.01, rhat_acceptable = 1.1, ess_target = NaN)
  )
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "finite"
  )
})

test_that(".validate_diagnostics errors when policy ess_target is zero", {
  diag <- make_valid_diagnostics(
    policy = list(rhat_target = 1.01, rhat_acceptable = 1.1, ess_target = 0)
  )
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "ess_target"
  )
})

test_that(".validate_diagnostics errors when policy ess_target is negative", {
  diag <- make_valid_diagnostics(
    policy = list(rhat_target = 1.01, rhat_acceptable = 1.1, ess_target = -400)
  )
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "ess_target"
  )
})

test_that(".validate_diagnostics errors when policy rhat_target is non-positive", {
  diag <- make_valid_diagnostics(
    policy = list(rhat_target = 0, rhat_acceptable = 1.1, ess_target = 400)
  )
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "rhat_target"
  )
})

test_that(".validate_diagnostics errors when policy rhat_target >= rhat_acceptable", {
  # Reversed thresholds: rhat_target equal to rhat_acceptable.
  diag_equal <- make_valid_diagnostics(
    policy = list(rhat_target = 1.1, rhat_acceptable = 1.1, ess_target = 400)
  )
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag_equal, species = "dog", endpoint = "q_tc"),
    "rhat_target"
  )

  # Reversed thresholds: rhat_target greater than rhat_acceptable.
  diag_reversed <- make_valid_diagnostics(
    policy = list(rhat_target = 1.2, rhat_acceptable = 1.1, ess_target = 400)
  )
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag_reversed, species = "dog", endpoint = "q_tc"),
    "rhat_target"
  )
})

test_that(".validate_diagnostics rejects an invalid policy even when rhat/ess/status are internally consistent under it", {
  # Under a reversed-threshold policy (rhat_target > rhat_acceptable), rhat
  # values of 1.05 would appear to satisfy both "target" and "acceptable"
  # criteria, making a recorded "pass" look self-consistent. The policy
  # itself must still be rejected.
  params <- c("alpha0", "A0", "beta0", "phi0")
  diag <- list(
    parameters      = params,
    rhat            = stats::setNames(rep(1.05, length(params)), params),
    ess             = stats::setNames(rep(900, length(params)), params),
    status          = "pass",
    policy          = list(rhat_target = 1.2, rhat_acceptable = 1.1, ess_target = 400),
    assessed_at     = "2026-01-01 00:00:00 UTC",
    package_version = "0.0.0.63"
  )
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "rhat_target"
  )
})

test_that(".validate_diagnostics errors when assessed_at is not a character string", {
  diag <- make_valid_diagnostics()
  diag$assessed_at <- 20260101
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "assessed_at"
  )
})

test_that(".validate_diagnostics errors when package_version is empty", {
  diag <- make_valid_diagnostics()
  diag$package_version <- ""
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "package_version"
  )
})

test_that(".validate_diagnostics errors when 'pass' is recorded with R-hat at/above the failure threshold", {
  diag <- make_valid_diagnostics(status = "pass")
  params <- diag$parameters
  diag$rhat <- stats::setNames(rep(1.5, length(params)), params)
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "inconsistent"
  )
})

test_that(".validate_diagnostics errors when 'pass' is recorded with insufficient ESS", {
  diag <- make_valid_diagnostics(status = "pass")
  params <- diag$parameters
  diag$ess <- stats::setNames(rep(10, length(params)), params)
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "inconsistent"
  )
})

test_that(".validate_diagnostics errors when 'pass' is recorded with missing R-hat", {
  diag <- make_valid_diagnostics(status = "pass")
  params <- diag$parameters
  diag$rhat <- stats::setNames(rep(NA_real_, length(params)), params)
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "inconsistent"
  )
})

test_that(".validate_diagnostics errors when 'unassessed' is recorded with fully populated finite R-hat", {
  diag <- make_valid_diagnostics(status = "unassessed")
  params <- diag$parameters
  diag$rhat <- stats::setNames(rep(1.001, length(params)), params)
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "inconsistent"
  )
})

test_that(".validate_diagnostics errors when ess contains non-finite or non-positive values", {
  diag_inf <- make_valid_diagnostics(status = "pass")
  params <- diag_inf$parameters
  diag_inf$ess <- stats::setNames(c(Inf, 900, 900, 900), params)
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag_inf, species = "dog", endpoint = "q_tc"),
    "ess"
  )

  diag_zero <- make_valid_diagnostics(status = "pass")
  diag_zero$ess <- stats::setNames(c(0, 900, 900, 900), params)
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag_zero, species = "dog", endpoint = "q_tc"),
    "ess"
  )

  diag_na <- make_valid_diagnostics(status = "pass")
  diag_na$ess <- stats::setNames(c(NA_real_, 900, 900, 900), params)
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag_na, species = "dog", endpoint = "q_tc"),
    "ess"
  )
})

test_that(".validate_diagnostics errors when rhat contains a non-finite value", {
  diag <- make_valid_diagnostics(status = "failed")
  params <- diag$parameters
  diag$rhat <- stats::setNames(c(Inf, 1.5, 1.5, 1.5), params)
  expect_error(
    PriorRhythm:::.validate_diagnostics(diag, species = "dog", endpoint = "q_tc"),
    "rhat"
  )
})

# ---------------------------------------------------------------------------
# convergence_policy / .convergence_status / summarize_convergence_diagnostics
# tests
# ---------------------------------------------------------------------------

test_that("convergence_policy returns documented thresholds", {
  policy <- convergence_policy()
  expect_equal(policy$rhat_target, 1.01)
  expect_equal(policy$rhat_acceptable, 1.1)
  expect_equal(policy$ess_target, 400)
})

test_that(".convergence_status returns 'unassessed' when rhat is NULL or has NA", {
  policy <- convergence_policy()
  expect_equal(
    PriorRhythm:::.convergence_status(rhat = NULL, ess = c(a = 500), policy = policy),
    "unassessed"
  )
  expect_equal(
    PriorRhythm:::.convergence_status(
      rhat = c(a = NA_real_), ess = c(a = 500), policy = policy
    ),
    "unassessed"
  )
})

test_that(".convergence_status returns 'pass' when rhat < target and ess > target", {
  policy <- convergence_policy()
  status <- PriorRhythm:::.convergence_status(
    rhat = c(a = 1.001, b = 1.005),
    ess  = c(a = 900, b = 800),
    policy = policy
  )
  expect_equal(status, "pass")
})

test_that(".convergence_status returns 'acceptable' when rhat is between target and acceptable", {
  policy <- convergence_policy()
  status <- PriorRhythm:::.convergence_status(
    rhat = c(a = 1.05, b = 1.02),
    ess  = c(a = 900, b = 800),
    policy = policy
  )
  expect_equal(status, "acceptable")
})

test_that(".convergence_status returns 'failed' when rhat exceeds acceptable threshold", {
  policy <- convergence_policy()
  status <- PriorRhythm:::.convergence_status(
    rhat = c(a = 1.2, b = 1.02),
    ess  = c(a = 900, b = 800),
    policy = policy
  )
  expect_equal(status, "failed")
})

test_that(".convergence_status returns 'acceptable' when rhat passes target but ess is low", {
  policy <- convergence_policy()
  status <- PriorRhythm:::.convergence_status(
    rhat = c(a = 1.001, b = 1.001),
    ess  = c(a = 100, b = 100),
    policy = policy
  )
  expect_equal(status, "acceptable")
})

test_that("summarize_convergence_diagnostics errors when stage1 is NULL", {
  expect_error(
    PriorRhythm:::summarize_convergence_diagnostics(NULL),
    "stage1"
  )
})

test_that("summarize_convergence_diagnostics errors when mcmc_diag is NULL", {
  expect_error(
    PriorRhythm:::summarize_convergence_diagnostics(list(mcmc_diag = NULL)),
    "mcmc_diag"
  )
})

test_that("summarize_convergence_diagnostics returns a well-formed structure", {
  skip_if_not_installed("coda")

  params <- c("alpha0", "A0", "beta0", "phi0")
  set.seed(1L)
  chain1 <- coda::mcmc(matrix(rnorm(400L * length(params)), ncol = length(params),
                              dimnames = list(NULL, params)))
  chain2 <- coda::mcmc(matrix(rnorm(400L * length(params)), ncol = length(params),
                              dimnames = list(NULL, params)))
  mcmc_diag <- coda::as.mcmc.list(list(chain1, chain2))
  stage1 <- list(mcmc_diag = mcmc_diag)

  diag <- PriorRhythm:::summarize_convergence_diagnostics(
    stage1     = stage1,
    parameters = params
  )

  expect_named(
    diag,
    c("parameters", "rhat", "ess", "status", "policy", "assessed_at", "package_version")
  )
  expect_equal(diag$parameters, params)
  expect_named(diag$rhat, params)
  expect_named(diag$ess, params)
  expect_true(diag$status %in% c("pass", "acceptable", "failed", "unassessed"))
  expect_no_error(PriorRhythm:::.validate_diagnostics(
    diag, species = "dog", endpoint = "q_tc", path = "<test>"
  ))
})

test_that("summarize_convergence_diagnostics records 'unassessed' with a single chain", {
  skip_if_not_installed("coda")

  params <- c("alpha0", "A0")
  chain1 <- coda::mcmc(matrix(rnorm(400L * length(params)), ncol = length(params),
                              dimnames = list(NULL, params)))
  mcmc_diag <- coda::as.mcmc.list(list(chain1))
  stage1 <- list(mcmc_diag = mcmc_diag)

  diag <- PriorRhythm:::summarize_convergence_diagnostics(stage1 = stage1, parameters = params)
  expect_equal(diag$status, "unassessed")
  expect_true(all(is.na(diag$rhat)))
})

test_that("summarize_convergence_diagnostics errors when mcmc_diag omits a requested parameter", {
  skip_if_not_installed("coda")

  available <- c("alpha0", "A0", "beta0")
  chain1 <- coda::mcmc(matrix(rnorm(400L * length(available)), ncol = length(available),
                              dimnames = list(NULL, available)))
  chain2 <- coda::mcmc(matrix(rnorm(400L * length(available)), ncol = length(available),
                              dimnames = list(NULL, available)))
  mcmc_diag <- coda::as.mcmc.list(list(chain1, chain2))
  stage1 <- list(mcmc_diag = mcmc_diag)

  expect_error(
    PriorRhythm:::summarize_convergence_diagnostics(
      stage1     = stage1,
      parameters = c("alpha0", "A0", "beta0", "phi0")
    ),
    "phi0"
  )
})

# ---------------------------------------------------------------------------
# .filter_priors_by_convergence tests
# ---------------------------------------------------------------------------

make_assessed_item <- function(species, endpoint, status) {
  list(
    species     = species,
    endpoint    = endpoint,
    pa          = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
    diagnostics = make_valid_diagnostics(status = status)
  )
}

test_that(".filter_priors_by_convergence retains 'pass' endpoints without warning", {
  assessed <- list(make_assessed_item("dog", "q_tc", "pass"))
  expect_no_warning(
    result <- PriorRhythm:::.filter_priors_by_convergence(assessed)
  )
  expect_named(result, "dog")
  expect_named(result$dog, "q_tc")
})

test_that(".filter_priors_by_convergence retains 'acceptable' endpoints with a warning", {
  assessed <- list(make_assessed_item("dog", "q_tc", "acceptable"))
  expect_warning(
    result <- PriorRhythm:::.filter_priors_by_convergence(assessed),
    "acceptable"
  )
  expect_named(result$dog, "q_tc")
})

test_that(".filter_priors_by_convergence excludes and reports 'failed' endpoints", {
  assessed <- list(
    make_assessed_item("dog", "q_tc", "pass"),
    make_assessed_item("dog", "hr", "failed")
  )
  expect_warning(
    result <- PriorRhythm:::.filter_priors_by_convergence(assessed),
    "Excluding.*dog.*hr"
  )
  expect_true("q_tc" %in% names(result$dog))
  expect_false("hr" %in% names(result$dog))
})

test_that(".filter_priors_by_convergence excludes and reports 'unassessed' endpoints", {
  assessed <- list(
    make_assessed_item("dog", "q_tc", "pass"),
    make_assessed_item("monkey", "qt_cf", "unassessed")
  )
  expect_warning(
    result <- PriorRhythm:::.filter_priors_by_convergence(assessed),
    "Excluding.*monkey.*qt_cf"
  )
  expect_false("monkey" %in% names(result))
})

test_that(".filter_priors_by_convergence aborts when no endpoints are eligible", {
  assessed <- list(
    make_assessed_item("dog", "q_tc", "failed"),
    make_assessed_item("monkey", "qt_cf", "unassessed")
  )
  expect_error(
    suppressWarnings(PriorRhythm:::.filter_priors_by_convergence(assessed)),
    "No species/endpoint priors met the convergence policy"
  )
})

test_that(".filter_priors_by_convergence emits an informational build summary", {
  assessed <- list(
    make_assessed_item("dog", "q_tc", "pass"),
    make_assessed_item("monkey", "qt_cf", "pass")
  )
  expect_message(
    PriorRhythm:::.filter_priors_by_convergence(assessed),
    "Packaging 2 prior.*dog/q_tc.*monkey/qt_cf|Packaging 2 prior.*monkey/qt_cf.*dog/q_tc"
  )
})

# ---------------------------------------------------------------------------
# make_runtime_prior_bundle tests
# ---------------------------------------------------------------------------

make_stage1_result <- function(params, chain_means, n_per_chain = 500L) {
  skip_if_not_installed("coda")
  chains <- lapply(chain_means, function(mu) {
    coda::mcmc(matrix(
      rnorm(n_per_chain * length(params), mean = mu),
      ncol = length(params),
      dimnames = list(NULL, params)
    ))
  })
  list(
    pa        = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
    mcmc_diag = coda::as.mcmc.list(chains)
  )
}

test_that("make_runtime_prior_bundle packages well-converged endpoints", {
  skip_if_not_installed("coda")
  set.seed(2L)
  params <- c("alpha0", "A0", "beta0", "phi0")

  stage1_priors <- list(
    dog = list(
      q_tc = make_stage1_result(params, chain_means = c(0, 0, 0))
    )
  )

  bundle <- PriorRhythm:::make_runtime_prior_bundle(stage1_priors, parameters = params)

  expect_named(bundle, c("schema_version", "metadata", "priors"))
  expect_named(bundle$priors, "dog")
  expect_named(bundle$priors$dog, "q_tc")
  expect_no_error(PriorRhythm:::.validate_diagnostics(
    bundle$priors$dog$q_tc$diagnostics, species = "dog", endpoint = "q_tc"
  ))
})

test_that("make_runtime_prior_bundle accepts a valid supplied POSIXct created_at", {
  skip_if_not_installed("coda")
  set.seed(21L)
  params <- c("alpha0", "A0", "beta0", "phi0")

  stage1_priors <- list(
    dog = list(
      q_tc = make_stage1_result(params, chain_means = c(0, 0, 0))
    )
  )

  ts <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  bundle <- PriorRhythm:::make_runtime_prior_bundle(
    stage1_priors, parameters = params, created_at = ts
  )

  expect_equal(bundle$metadata$created_at, format(ts, tz = "UTC", usetz = TRUE))
})

test_that("make_runtime_prior_bundle rejects a character created_at", {
  stage1_priors <- list(
    dog = list(
      q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1), mcmc_diag = NULL)
    )
  )
  expect_error(
    PriorRhythm:::make_runtime_prior_bundle(
      stage1_priors, created_at = "2026-01-01 00:00:00"
    ),
    "must be a single non-missing"
  )
})

test_that("make_runtime_prior_bundle rejects an NA created_at", {
  stage1_priors <- list(
    dog = list(
      q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1), mcmc_diag = NULL)
    )
  )
  expect_error(
    PriorRhythm:::make_runtime_prior_bundle(
      stage1_priors, created_at = as.POSIXct(NA)
    ),
    "must be a single non-missing"
  )
})

test_that("make_runtime_prior_bundle rejects a vector of created_at timestamps", {
  stage1_priors <- list(
    dog = list(
      q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1), mcmc_diag = NULL)
    )
  )
  expect_error(
    PriorRhythm:::make_runtime_prior_bundle(
      stage1_priors,
      created_at = as.POSIXct(c("2026-01-01 00:00:00", "2026-01-02 00:00:00"), tz = "UTC")
    ),
    "must be a single non-missing"
  )
})

test_that("make_runtime_prior_bundle excludes not-converged endpoints and keeps the rest", {
  skip_if_not_installed("coda")
  set.seed(3L)
  params <- c("alpha0", "A0", "beta0", "phi0")

  stage1_priors <- list(
    dog = list(
      q_tc = make_stage1_result(params, chain_means = c(0, 0, 0)),
      # Widely separated chain means force a large Gelman-Rubin R-hat.
      hr   = make_stage1_result(params, chain_means = c(0, 10, 20))
    )
  )

  expect_warning(
    bundle <- PriorRhythm:::make_runtime_prior_bundle(stage1_priors, parameters = params),
    "Excluding.*dog.*hr"
  )
  expect_true("q_tc" %in% names(bundle$priors$dog))
  expect_false("hr" %in% names(bundle$priors$dog))
})

test_that("make_runtime_prior_bundle aborts when no endpoints are eligible", {
  skip_if_not_installed("coda")
  set.seed(4L)
  params <- c("alpha0", "A0", "beta0", "phi0")

  stage1_priors <- list(
    dog = list(
      hr = make_stage1_result(params, chain_means = c(0, 10, 20))
    )
  )

  expect_error(
    suppressWarnings(
      PriorRhythm:::make_runtime_prior_bundle(stage1_priors, parameters = params)
    ),
    "No species/endpoint priors met the convergence policy"
  )
})

test_that("make_runtime_prior_bundle errors on missing posterior columns", {
  stage1_priors <- list(
    dog = list(
      q_tc = list(pa = data.frame(alpha0 = 1, A0 = 1), mcmc_diag = NULL)
    )
  )
  expect_error(
    PriorRhythm:::make_runtime_prior_bundle(stage1_priors),
    "missing required posterior columns"
  )
})

