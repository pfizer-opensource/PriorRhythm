# bayesian_test_example ----
#' Stats Analysis: Compare Historical-Prior Borrowing With Weak-Prior Comparator
#' @importFrom cli cli_inform
#' @importFrom stats as.formula cov model.matrix na.omit
#' @importFrom MCMCglmm MCMCglmm
#' @importFrom glue glue
#' @importFrom dplyr select any_of all_of filter mutate across pull distinct arrange
#' @importFrom purrr set_names
#' @inheritParams chain_priors_from_data
#' @inheritParams MCMCglmm::MCMCglmm
#' @param input_tdat data.frame with study data
#' @param y_col string column name containing response variable.
#' @param pa output from `chain_priors_from_data()`
#' @param n_exclude numeric, number of subjects to exclude (randomly selected).
#'   Default is 2.
#' @param exclude_by_id vector indicating which animals to exclude.
#'   If `NULL` (default), animals will be excluded randomly based on the
#'   value supplied to `n_exclude`.
#' @param animal_exclude alias for `exclude_by_id` provided for compatibility
#'   with Dean's workflow. When both are supplied, `exclude_by_id` takes
#'   precedence. Default is `NULL`.
#' @param seed integer passed to [base::set.seed()] before the random exclusion
#'   draw, ensuring reproducible animal selection across calls. Set to `NULL`
#'   to skip seeding. Default is `732`.
#' @param mcmc_seed integer seed base for `MCMCglmm` MCMC sampling. A unique
#'   starting seed is derived for each model variant and time bin via
#'   `mcmc_seed + (time_bin_index - 1) * 4 + model_offset`, where offsets are
#'   0 = Non_informative (weak-prior comparator, full data),
#'   1 = Non_informative_Incomplete (weak-prior comparator, partial data),
#'   2 = Informative_Incomplete (historical prior, partial data),
#'   3 = Informative (historical prior, full data).
#'   This gives each chain a
#'   distinct position in R's Mersenne Twister RNG stream, preventing the
#'   artificial correlation caused by resetting all chains to the same seed.
#'   Note: this is arithmetic offset seeding within a single RNG stream, not
#'   the parallel-stream approach of `RNGkind("L'Ecuyer-CMRG")`. Overlap
#'   between streams is negligible in practice given the Mersenne Twister's
#'   period (2^19937) and typical MCMC run lengths, but is not formally
#'   guaranteed. The stride of 4 matches the current number of model variants;
#'   it must be updated if additional variants are added. Set to `NULL` to skip
#'   seeding entirely. Default is `1235L`, which reproduces the existing
#'   `Non_informative` result for the first time bin.
#' @param scale numeric. Scaling factor applied to the historical prior
#'   variance matrix: `V = diag(diag(b_v_info / scale))`. A value of `1`
#'   uses the raw covariance from the prior; values less than 1 inflate the
#'   variance (more diffuse prior); values greater than 1 shrink it (more
#'   concentrated). Default is `0.1`.
#' @return A named list containing `Choices_all` (available model type identifiers:
#'   `"Non_informative"` (weak-prior comparator, full data) = Model A,
#'   `"Non_informative_Incomplete"` (weak-prior comparator, partial data) = Model B,
#'   `"Informative_Incomplete"` (historical prior, partial data) = Model C,
#'   `"Informative"` (historical prior, full data) = Model D)
#'   and `out` (a list of fitted model objects per time point). Both branches
#'   are fitted with `MCMCglmm`; Models A/B omit the `prior` argument and use
#'   `MCMCglmm`'s weak default prior, while Models C/D supply the Stage 1
#'   historical prior derived from `pa`.
#' @export
#' @example R/examples/Example_bayesian_test_example.R
bayesian_test_example <- function(input_tdat    = NULL,
                                  animal_col    = "animal",
                                  treatment_col = "dose",
                                  period_col    = "period_code",
                                  time1_col     = "time1",
                                  time2_col     = "time2",
                                  y_col         = "value",
                                  n_exclude     = 2,
                                  exclude_by_id = NULL,
                                  animal_exclude = NULL,
                                  nitt          = 15e3,
                                  thin          = 5,
                                  burnin        = 1e4,
                                  pa            = NULL,
                                  seed          = 732L,
                                  mcmc_seed     = 1235L,
                                  scale         = 0.1) {

  # animal_exclude is an alias for exclude_by_id; exclude_by_id takes precedence
  if (is.null(exclude_by_id) && !is.null(animal_exclude)) {
    exclude_by_id <- animal_exclude
  }

  all_input_cols <- c(animal_col, treatment_col, period_col,
                      y_col, time1_col, time2_col)
  
  tdat <- input_tdat |>
    dplyr::select(dplyr::any_of(all_input_cols)) |>
    as.data.frame()
  
  lm_form_string <- glue::glue("{y_col} ~ as.factor({treatment_col}) - 1")
  cli::cli_inform("lm: {lm_form_string}")
  
  random_string <- glue::glue("~ {animal_col} + {period_col}")
  cli::cli_inform("random effects: {random_string}")
  
  if (!is.null(seed)) set.seed(seed)
  
  # Derive time bin pairs explicitly — never rely on positional matching of
  # independent unique() vectors (see issue #57)
  time_pairs <- tdat |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(time1_col, time2_col)))) |>
    dplyr::arrange(.data[[time1_col]])

  ut1 <- time_pairs[[time1_col]]
  ut2 <- time_pairs[[time2_col]]
  nt  <- nrow(time_pairs)
  
  out <- purrr::set_names(as.list(rep(NA, nt)), ut1)
  
  if (is.null(exclude_by_id)) {
    animal_exclude <- tdat |>
      dplyr::pull(animal_col) |>
      unique() |>
      sample(size = n_exclude)
  } else {
    animal_exclude <- exclude_by_id
  }
  
  if (!is.null(animal_exclude)) {
    cli::cli_inform("excluding: {animal_exclude}")
  }
  
  for (i in seq_len(nt)) {
    mat <- tdat |>
      dplyr::select(dplyr::any_of(all_input_cols)) |>
      dplyr::filter(.data[[time1_col]] == ut1[i]) |>
      droplevels() |>
      stats::na.omit() |>
      dplyr::mutate(dplyr::across(dplyr::any_of(c(animal_col, period_col)), as.factor)) |>
      as.data.frame()
    
    nd <- length(ud <- sort(unique(mat[, treatment_col])))
    
    ff <- stats::as.formula(lm_form_string)
    p  <- ncol(stats::model.matrix(ff, data = mat))
    
    prior_mat <- matrix(NA, nrow(pa), nd)
    for (j in seq_len(nd)) {
      prior_mat[, j] <- pa$alpha0 +
        sinusoidal_interval_mean(A = pa$A0, t1 = ut1[i], t2 = ut2[i], phi = pa$phi0) +
        pa$beta0 * ud[j]
    }
    b_mu_info <- colMeans(prior_mat)
    b_v_info  <- stats::cov(prior_mat)
    
    # Non_informative: seed offset 0
    if (!is.null(mcmc_seed)) set.seed(mcmc_seed + (i - 1L) * 4L + 0L)
    m <- MCMCglmm::MCMCglmm(
      stats::as.formula(lm_form_string),
      family  = "gaussian",
      verbose = FALSE,
      nitt    = nitt,
      thin    = thin,
      burnin  = burnin,
      data    = mat,
      random  = stats::as.formula(random_string)
    )
    m$data <- mat
    
    # Non_informative_Incomplete: seed offset 1
    if (!is.null(mcmc_seed)) set.seed(mcmc_seed + (i - 1L) * 4L + 1L)
    dat_incomplete <- droplevels(mat[!(mat[, animal_col] %in% animal_exclude), ])
    m1 <- MCMCglmm::MCMCglmm(
      stats::as.formula(lm_form_string),
      data    = dat_incomplete,
      family  = "gaussian",
      verbose = FALSE,
      nitt    = nitt,
      thin    = thin,
      burnin  = burnin,
      random  = stats::as.formula(random_string)
    )
    m1$data <- dat_incomplete
    
    # Informative_Incomplete: seed offset 2
    if (!is.null(mcmc_seed)) set.seed(mcmc_seed + (i - 1L) * 4L + 2L)
    m2 <- MCMCglmm::MCMCglmm(
      stats::as.formula(lm_form_string),
      data    = dat_incomplete,
      random  = stats::as.formula(random_string),
      family  = "gaussian",
      verbose = FALSE,
      nitt    = nitt,
      thin    = thin,
      burnin  = burnin,
      prior   = list(
        R = list(V = 1, nu = 0.002),
        G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)),
        B = list(mu = b_mu_info, V = diag(diag(b_v_info / scale)))
      )
    )
    m2$data <- dat_incomplete
    
    # Informative: seed offset 3
    if (!is.null(mcmc_seed)) set.seed(mcmc_seed + (i - 1L) * 4L + 3L)
    m3 <- MCMCglmm::MCMCglmm(
      stats::as.formula(lm_form_string),
      data    = mat,
      random  = stats::as.formula(random_string),
      family  = "gaussian",
      verbose = FALSE,
      nitt    = nitt,
      thin    = thin,
      burnin  = burnin,
      prior   = list(
        R = list(V = 1, nu = 0.002),
        G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)),
        B = list(mu = b_mu_info, V = diag(diag(b_v_info / scale)))
      )
    )
    m3$data <- mat
    
    out[[i]] <- list(
      Non_informative            = m,
      Non_informative_Incomplete = m1,
      Informative_Incomplete     = m2,
      Informative                = m3
    )
  }
  
  choices_all <- c("Non_informative", "Non_informative_Incomplete",
                   "Informative_Incomplete", "Informative")
  
  list(Choices_all = choices_all, out = out)
}

# exceedance_prob ----
#' @title Posterior Exceedance Probability for Models A and D
#' @description Computes \eqn{P(\Delta > \mathrm{threshold})} at each time
#'   window for the weak-prior comparator (Model A, `Non_informative`) and
#'   historical-prior (Model D, `Informative`) models.
#' @importFrom cli cli_abort
#' @importFrom dplyr filter
#' @param mod output of [bayesian_test_example()].
#' @param threshold numeric decision threshold. Default is `0`.
#' @return A data frame with columns `time_window` (numeric start time),
#'   `p_exceed_weak_prior` (\eqn{P(\Delta > \mathrm{threshold})} under the
#'   weak-prior comparator, Model A), and `p_exceed_historical_prior`
#'   (\eqn{P(\Delta > \mathrm{threshold})} under the historical-prior model,
#'   Model D). The probability is computed for the highest-dose treatment
#'   code vs the lowest (control).
#' @export
#' @example R/examples/Example_exceedance_prob.R
exceedance_prob <- function(mod = NULL, threshold = 0) {
  if (is.null(mod)) cli::cli_abort("{.arg mod} must not be NULL")
  if (!all(c("out", "Choices_all") %in% names(mod))) {
    cli::cli_abort("{.arg mod} must be the output of {.fn bayesian_test_example}")
  }

  nt <- length(mod$out)
  ut1 <- names(mod$out)

  out_df <- data.frame(
    time_window               = as.numeric(ut1),
    p_exceed_weak_prior       = NA_real_,
    p_exceed_historical_prior = NA_real_
  )

  for (i in seq_len(nt)) {
    sol_A <- mod$out[[i]]$Non_informative$Sol
    sol_D <- mod$out[[i]]$Informative$Sol
    b_A   <- grep("treatment", colnames(sol_A))
    b_D   <- grep("treatment", colnames(sol_D))

    if (length(b_A) >= 2) {
      out_df$p_exceed_weak_prior[i] <- mean(
        as.vector(sol_A[, b_A[length(b_A)]]) -
          as.vector(sol_A[, b_A[1]]) > threshold
      )
    }
    if (length(b_D) >= 2) {
      out_df$p_exceed_historical_prior[i] <- mean(
        as.vector(sol_D[, b_D[length(b_D)]]) -
          as.vector(sol_D[, b_D[1]]) > threshold
      )
    }
  }

  out_df
}

# rope_analysis ----
#' ROPE Analysis for Models A and D
#'
#' Runs Region of Practical Equivalence (ROPE) analysis via
#'   \code{bayestestR::rope()} for Models A (`Non_informative`) and D
#'   (`Informative`) at each time window.
#' @importFrom bayestestR rope
#' @importFrom cli cli_abort
#' @param mod output of [bayesian_test_example()].
#' @param threshold numeric, half-width of the ROPE interval:
#'   `range = c(-threshold, threshold)`. Default is `0`.
#' @return A named list with elements `model_A` and `model_D`, each a list of
#'   \code{bayestestR::rope()} results, one per time window.
#' @export
#' @example R/examples/Example_rope_analysis.R
rope_analysis <- function(mod = NULL, threshold = 0) {
  if (is.null(mod)) cli::cli_abort("{.arg mod} must not be NULL")
  if (!all(c("out", "Choices_all") %in% names(mod))) {
    cli::cli_abort("{.arg mod} must be the output of {.fn bayesian_test_example}")
  }

  nt <- length(mod$out)
  rope_range <- c(-threshold, threshold)

  results_A <- vector("list", nt)
  results_D <- vector("list", nt)

  for (i in seq_len(nt)) {
    sol_A <- mod$out[[i]]$Non_informative$Sol
    sol_D <- mod$out[[i]]$Informative$Sol
    b_A   <- grep("treatment", colnames(sol_A))
    b_D   <- grep("treatment", colnames(sol_D))

    if (length(b_A) >= 2) {
      ctr_A <- as.data.frame(
        sol_A[, b_A[-1], drop = FALSE] - as.vector(sol_A[, b_A[1]])
      )
      results_A[[i]] <- bayestestR::rope(ctr_A, range = rope_range)
    }
    if (length(b_D) >= 2) {
      ctr_D <- as.data.frame(
        sol_D[, b_D[-1], drop = FALSE] - as.vector(sol_D[, b_D[1]])
      )
      results_D[[i]] <- bayestestR::rope(ctr_D, range = rope_range)
    }
  }

  output <- list(model_A = results_A, model_D = results_D)
  return(output)
}
