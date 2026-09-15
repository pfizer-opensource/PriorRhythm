# make_runtime_prior_bundle ----
#' Build a Runtime Prior Bundle from Stage 1 Fits
#'
#' Internal, tested source of truth for turning a species/endpoint tree of
#' Stage 1 `chain_priors_from_data()` results into the slim runtime prior
#' bundle shipped with the package (see `validate_prior_bundle()`). Used by
#' the controlled prior-refresh/build workflow
#' (`data-raw/build_priors_for_package.R`), which is limited to
#' orchestrating fitting, calling this function, saving the resulting RDS,
#' and validating the generated artifact.
#'
#' For each endpoint, chain-aware convergence evidence is computed once from
#' the Stage 1 fit's `mcmc_diag` (see `summarize_convergence_diagnostics()`)
#' and the packaging policy (see `convergence_policy()`) is applied by
#' filtering (see `.filter_priors_by_convergence()`) rather than aborting
#' the whole build:
#'
#' - `"pass"` endpoints are retained.
#' - `"acceptable"` endpoints are retained with a warning identifying the
#'   species/endpoint.
#' - `"failed"` and `"unassessed"` endpoints are excluded, each with a
#'   warning naming the species/endpoint and status.
#' - Packaging aborts only when no endpoint remains eligible after
#'   filtering.
#'
#' Only pooled posterior draws (`pa`) and the compact `diagnostics` summary
#' are retained per endpoint — no chain draws, trace data, or raw
#' historical fields are included.
#'
#' @param stage1_priors Named list keyed by species, each a named list keyed
#'   by endpoint, of Stage 1 results (each containing `pa` and `mcmc_diag`;
#'   see `chain_priors_from_data()`).
#' @param parameters Character vector of hyperparameter names to summarize
#'   and package. Default `c("alpha0", "A0", "beta0", "phi0")`.
#' @param schema_version Integer schema version recorded in the bundle
#'   envelope. Default `1L`.
#' @param created_at A single non-missing `POSIXct` timestamp recorded in
#'   bundle metadata. Default `Sys.time()`.
#' @param package_version Character package version recorded in bundle
#'   metadata. Default the installed `PriorRhythm` version.
#' @return A normalised runtime prior bundle: a named list with
#'   `schema_version`, `metadata`, and `priors` (species -> endpoint ->
#'   `list(pa, diagnostics)`), containing only endpoints that met the
#'   convergence policy.
#' @importFrom cli cli_abort
#' @importFrom utils packageVersion
#' @keywords internal
#' @noRd
make_runtime_prior_bundle <- function(
  stage1_priors   = NULL,
  parameters      = c("alpha0", "A0", "beta0", "phi0"),
  schema_version  = 1L,
  created_at      = Sys.time(),
  package_version = utils::packageVersion("PriorRhythm")
) {

  if (FALSE) {
    stage1_priors <- list(
      dog = list(
        q_tc = list(
          pa        = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
          mcmc_diag = NULL
        )
      )
    )
    parameters      <- c("alpha0", "A0", "beta0", "phi0")
    schema_version  <- 1L
    created_at      <- Sys.time()
    package_version <- "0.0.0.64"
  }

  if (is.null(stage1_priors) || !is.list(stage1_priors) ||
      is.null(names(stage1_priors)) || length(stage1_priors) == 0L) {
    cli::cli_abort("{.arg stage1_priors} must be a non-empty named list of species.")
  }

  if (!inherits(created_at, "POSIXct") || length(created_at) != 1L || is.na(created_at)) {
    cli::cli_abort(
      "{.arg created_at} must be a single non-missing {.cls POSIXct} value."
    )
  }

  assessed <- list()

  for (species in names(stage1_priors)) {
    endpoint_priors <- stage1_priors[[species]]

    for (endpoint in names(endpoint_priors)) {
      stage1_result <- endpoint_priors[[endpoint]]
      pa <- as.data.frame(stage1_result[["pa"]])

      if (!all(parameters %in% names(pa))) {
        cli::cli_abort(
          "Prior {.field {species}/{endpoint}} is missing required posterior columns: \\
           {.val {parameters}}."
        )
      }

      diagnostics <- summarize_convergence_diagnostics(
        stage1     = stage1_result,
        parameters = parameters
      )

      assessed[[length(assessed) + 1L]] <- list(
        species     = species,
        endpoint    = endpoint,
        pa          = pa[, parameters, drop = FALSE],
        diagnostics = diagnostics
      )
    }
  }

  species_priors <- .filter_priors_by_convergence(assessed)

  bundle <- list(
    schema_version = schema_version,
    metadata = list(
      created_at        = format(created_at, tz = "UTC", usetz = TRUE),
      package_version   = as.character(package_version),
      posterior_columns = parameters,
      source            = "reviewed precomputed historical priors"
    ),
    priors = species_priors
  )

  return(bundle)
}
