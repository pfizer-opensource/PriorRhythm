test_that("the packaged default prior bundle has required diagnostics", {
  bundle <- PriorRhythm:::resolve_prior_bundle(NULL)
  
  for (species in bundle$priors) {
    for (endpoint in species) {
      expect_no_error(
        PriorRhythm:::.validate_diagnostics(endpoint$diagnostics)
      )
    }
  }
})