# convergence_diag ----
#' Convergence Diagnostics for a Stage 1 Prior Object
#'
#' Computes and prints Gelman-Rubin R-hat and Effective Sample Size (ESS)
#' diagnostics for the four hyperparameters (`alpha0`, `A0`, `beta0`, `phi0`)
#' from the `mcmc_diag` field of a `chain_priors_from_data_helper()` output.
#'
#' @param stage1 named list returned by `chain_priors_from_data_helper()`.
#'   Must contain an `mcmc_diag` element (an `mcmc.list` of the four
#'   hyperparameters). If `mcmc_diag` is `NULL` the function aborts with an
#'   informative message.
#' @param parameters character vector of hyperparameter names to diagnose.
#'   Default is `c("alpha0", "A0", "beta0", "phi0")`.
#'
#' @return A named list (invisibly) with two elements:
#'   - `rhat`: matrix from `coda::gelman.diag()` (`Point est.` and `Upper C.I.`)
#'   - `ess`:  named numeric vector from `coda::effectiveSize()`
#'
#' @export
#' @importFrom coda gelman.diag effectiveSize mcmc as.mcmc.list
#' @importFrom cli cli_abort cli_inform
#' @example R/examples/Example_convergence_diag.R
convergence_diag <- function(
  stage1     = NULL,
  parameters = c("alpha0", "A0", "beta0", "phi0")
) {

  if(FALSE){
    
    SynHist <- system.file("extdata", "SyntheticHistorical",
                           package = "PriorRhythm", mustWork = TRUE
    )
    
    Long_dat <- readRDS(file.path(SynHist, "Syn_long.RDS"))
    
    dat <- Long_dat |>
      dplyr::filter(.data$study_code %in% c(1, 2))
    
    stage1     <- chain_priors_from_data_helper(
      dat              = dat,
      assay_name       = "assay1",
      animal_col       = "animal_code",
      treatment_col    = "treatment_code",
      study_col        = "study_code",
      period_col       = "period_code",
      assay_names_col  = "parameter",
      assay_values_col = "value",
      n.iter.update    = 200L,
      n.iter.sample    = 200L,
      n.chains         = 2L,
      verbose          = FALSE
    )
    parameters <- c("alpha0", "A0", "beta0", "phi0")
  }

  if (is.null(stage1)) {
    cli::cli_abort("{.arg stage1} must not be NULL.")
  }

  mcmc_diag <- stage1$mcmc_diag

  if (is.null(mcmc_diag)) {
    cli::cli_abort(
      c(
        "{.field mcmc_diag} is NULL in the supplied {.arg stage1} object.",
        "i" = "This can happen when loading a cache built with an older version of PriorRhythm.",
        "i" = "Delete the cache file and rerun {.fun chain_priors_from_data_helper} to regenerate it."
      )
    )
  }

  all_params <- colnames(as.matrix(mcmc_diag[[1]]))
  keep       <- intersect(parameters, all_params)
  
  if (length(keep) == 0L) {
    cli::cli_abort(
      "None of {.val {parameters}} found in {.field mcmc_diag}. Available: {.val {all_params}}."
    )
  }

  diag_list <- coda::as.mcmc.list(
    lapply(mcmc_diag, function(ch) {
      coda::mcmc(as.matrix(ch)[, keep, drop = FALSE])
    })
  )

  rhat <- NULL
  
  if (length(diag_list) >= 2L) {
    gd   <- coda::gelman.diag(diag_list, multivariate = FALSE)
    rhat <- gd$psrf
    cli::cli_inform(
      "Gelman-Rubin R-hat (target < 1.01; acceptable < 1.1; > 1.1 = not converged):"
    )
    print(rhat)
    bad_params <- rownames(rhat)[rhat[, 1L] > 1.1]
    if (length(bad_params) > 0L) {
      cli::cli_warn(
        c(
          "Convergence not achieved for: {.val {bad_params}}",
          "i" = "R-hat > 1.1 indicates chains have not mixed. Consider increasing {.arg n.iter.update} or {.arg n.iter.sample}."
        )
      )
    }
  } else {
    cli::cli_inform(
      "Gelman-Rubin R-hat requires >= 2 chains; only {length(diag_list)} chain(s) found. Skipping."
    )
  }

  ess <- coda::effectiveSize(diag_list)
  cli::cli_inform("Effective Sample Size (should be > 400):")
  print(round(ess, 1L))

  output <- list(rhat = rhat, ess = ess)
  return(invisible(output))
}


# convergence_policy ----
#' Convergence Policy Thresholds for Packaged Prior Diagnostics
#'
#' Returns the documented convergence-acceptance thresholds applied when
#' packaging Stage 1 posterior draws (`alpha0`, `A0`, `beta0`, `phi0`) into a
#' runtime prior bundle. These are the single source of truth used both when
#' assessing Stage 1 fits during the controlled build/refresh workflow (see
#' `summarize_convergence_diagnostics()`) and when validating any recorded
#' diagnostics carried by a packaged bundle (see `validate_prior_bundle()`).
#'
#' @return A named list with elements:
#'   - `rhat_target`: Gelman-Rubin R-hat below this value is considered
#'     fully converged (default `1.01`).
#'   - `rhat_acceptable`: R-hat below this value (but at or above
#'     `rhat_target`) is considered acceptable with review; R-hat at or
#'     above it indicates the chains have not mixed (default `1.1`).
#'   - `ess_target`: Effective Sample Size above this value per parameter is
#'     considered adequate (default `400`).
#' @export
convergence_policy <- function() {

  policy <- list(
    rhat_target     = 1.01,
    rhat_acceptable = 1.1,
    ess_target      = 400
  )

  return(policy)
}


#' Classify Convergence Status from R-hat and ESS
#'
#' Internal helper that classifies a set of per-parameter R-hat and ESS
#' values against `convergence_policy()` thresholds.
#'
#' @param rhat Named numeric vector of R-hat point estimates (`NA` values
#'   indicate R-hat could not be assessed, e.g. fewer than two chains).
#' @param ess Named numeric vector of effective sample sizes.
#' @param policy Named list as returned by `convergence_policy()`.
#' @return Scalar character: one of `"pass"`, `"acceptable"`, `"failed"`, or
#'   `"unassessed"` (R-hat could not be computed).
#' @keywords internal
#' @noRd
.convergence_status <- function(rhat = NULL, ess = NULL, policy = NULL) {

  if (FALSE) {
    rhat   <- c(alpha0 = 1.001, A0 = 1.02)
    ess    <- c(alpha0 = 900, A0 = 500)
    policy <- convergence_policy()
  }

  if (is.null(rhat) || length(rhat) == 0L || anyNA(rhat)) {
    return("unassessed")
  }

  ess_ok         <- length(ess) > 0L && !anyNA(ess) && all(ess > policy$ess_target)
  rhat_target_ok <- all(rhat < policy$rhat_target)
  rhat_accept_ok <- all(rhat < policy$rhat_acceptable)

  status <- if (rhat_target_ok && ess_ok) {
    "pass"
  } else if (rhat_accept_ok) {
    "acceptable"
  } else {
    "failed"
  }

  return(status)
}


# summarize_convergence_diagnostics ----
#' Build a Structured, Non-Confidential Convergence Diagnostics Summary
#'
#' Computes chain-aware Gelman-Rubin R-hat and Effective Sample Size (ESS)
#' diagnostics from the `mcmc_diag` element of a Stage 1 prior fit (the same
#' `coda::mcmc.list` used by `convergence_diag()`) and returns them as a
#' compact, stable structure suitable for embedding alongside `pa` in a
#' packaged runtime prior bundle (see `validate_prior_bundle()`).
#'
#' Unlike `convergence_diag()`, this function does not print to the console
#' and never carries chain draws, trace data, or any raw historical fields -
#' only scalar convergence summary statistics. It is the recommended source
#' of the `diagnostics` object recorded per species/endpoint during the
#' controlled prior-refresh/build workflow.
#'
#' @param stage1 named list as returned by `chain_priors_from_data_helper()`.
#'   Must contain a non-NULL `mcmc_diag` element (a `coda::mcmc.list`).
#' @param parameters character vector of hyperparameter names to summarize.
#'   Default is `c("alpha0", "A0", "beta0", "phi0")`.
#' @return A named list (the `diagnostics` object) with elements:
#'   `parameters`, `rhat`, `ess`, `status`, `policy`, `assessed_at`, and
#'   `package_version`.
#' @importFrom coda gelman.diag effectiveSize mcmc as.mcmc.list
#' @importFrom stats setNames
#' @importFrom utils packageVersion
#' @importFrom cli cli_abort
#' @keywords internal
#' @noRd
summarize_convergence_diagnostics <- function(
    stage1 = NULL,
    parameters = c("alpha0", "A0", "beta0", "phi0")
) {
  if (is.null(stage1)) {
    cli::cli_abort("{.arg stage1} must not be NULL.")
  }
  
  mcmc_diag <- stage1$mcmc_diag
  
  if (is.null(mcmc_diag)) {
    cli::cli_abort(
      "{.field mcmc_diag} is NULL in the supplied {.arg stage1} object."
    )
  }
  
  all_params <- colnames(as.matrix(mcmc_diag[[1]]))
  missing_parameters <- setdiff(parameters, all_params)

  if (length(missing_parameters) > 0L) {
    cli::cli_abort(
      "Requested convergence parameter(s) are missing from {.field mcmc_diag}: {.val {missing_parameters}}."
    )
  }

  keep <- parameters
  
  diag_list <- coda::as.mcmc.list(
    lapply(mcmc_diag, function(ch) {
      coda::mcmc(as.matrix(ch)[, keep, drop = FALSE])
    })
  )
  
  rhat <- stats::setNames(rep(NA_real_, length(keep)), keep)
  
  if (length(diag_list) >= 2L) {
    gelman <- coda::gelman.diag(diag_list, multivariate = FALSE)
    rhat[rownames(gelman$psrf)] <- gelman$psrf[, "Point est."]
  }
  
  ess <- coda::effectiveSize(diag_list)[keep]
  policy <- convergence_policy()
  
  output <- list(
    parameters = keep,
    rhat = rhat,
    ess = ess,
    status = .convergence_status(
      rhat = rhat,
      ess = ess,
      policy = policy
    ),
    policy = policy,
    assessed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_version = as.character(utils::packageVersion("PriorRhythm"))
  )
  
  return(output)

}


# .filter_priors_by_convergence ----
#' Filter Assessed Priors by the Convergence Policy
#'
#' Internal helper used by `make_runtime_prior_bundle()` to apply the
#' packaging policy (see `convergence_policy()`) to a flat list of assessed
#' species/endpoint priors by filtering rather than aborting the whole
#' build:
#'
#' - `"pass"` and `"acceptable"` endpoints are retained; `"acceptable"`
#'   endpoints emit a warning identifying the species/endpoint.
#' - `"failed"` and `"unassessed"` endpoints are excluded, each with a
#'   warning naming the species/endpoint and recorded status.
#' - Packaging aborts only when no endpoint remains eligible after
#'   filtering.
#'
#' An informational message (`cli::cli_inform()`) lists every retained
#' `species/endpoint` prior, so the controlled build log states exactly
#' what was packaged.
#'
#' @param assessed List of `list(species, endpoint, pa, diagnostics)`
#'   entries, one per endpoint (see `make_runtime_prior_bundle()`).
#' @return A nested named list: species -> endpoint ->
#'   `list(pa, diagnostics)`, containing only retained endpoints.
#' @importFrom cli cli_abort cli_warn cli_inform
#' @keywords internal
#' @noRd
.filter_priors_by_convergence <- function(assessed = NULL) {

  if (FALSE) {
    assessed <- list(
      list(
        species     = "dog",
        endpoint    = "q_tc",
        pa          = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
        diagnostics = list(status = "pass")
      )
    )
  }

  statuses <- vapply(assessed, function(x) x[["diagnostics"]][["status"]], character(1L))

  excluded <- assessed[statuses %in% c("failed", "unassessed")]
  for (item in excluded) {
    cli::cli_warn(c(
      "Excluding {.val {item$species}}/{.val {item$endpoint}} from the packaged prior bundle.",
      "x" = "Status: {.val {item$diagnostics$status}}",
      "i" = "Convergence policy requires status {.val {c('pass', 'acceptable')}}."
    ))
  }

  retained <- assessed[statuses %in% c("pass", "acceptable")]

  if (length(retained) == 0L) {
    cli::cli_abort(c(
      "No species/endpoint priors met the convergence policy; nothing to package.",
      "i" = "Re-fit with more iterations/chains, or review the exclusions above."
    ))
  }

  acceptable <- retained[
    vapply(retained, function(x) identical(x[["diagnostics"]][["status"]], "acceptable"), logical(1L))
  ]
  for (item in acceptable) {
    cli::cli_warn(c(
      "Retaining {.val {item$species}}/{.val {item$endpoint}} with acceptable \\
       (not fully converged) diagnostics.",
      "i" = "Status: {.val {item$diagnostics$status}}"
    ))
  }

  labels <- vapply(retained, function(x) paste0(x$species, "/", x$endpoint), character(1L))
  cli::cli_inform(
    "Packaging {length(retained)} prior(s) that met the convergence policy: {.val {labels}}"
  )

  species_priors <- list()
  for (item in retained) {
    if (is.null(species_priors[[item$species]])) {
      species_priors[[item$species]] <- list()
    }
    species_priors[[item$species]][[item$endpoint]] <- list(
      pa          = item$pa,
      diagnostics = item$diagnostics
    )
  }

  return(species_priors)
}
