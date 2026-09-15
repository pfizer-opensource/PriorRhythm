# check_jags_indices ----
#' Validate JAGS Index Vectors for the Chain Model
#'
#' Checks that the animal (`a`), period (`b`), and study (`s`) index vectors
#' in a prepared JAGS input list are valid gap-free 1:N integer sequences, as
#' required by the chain model. Calls [cli::cli_abort()] with an informative
#' message for each violation found.
#'
#' This function is called automatically inside [chain_priors_from_data_helper()]
#' before the JAGS model is compiled. It is also exported so that users can
#' validate their prepared data frame **before** passing it to
#' [chain_priors_from_data()].
#'
#' @param input_list a named list as produced internally by
#'   [chain_priors_from_data_helper()], containing at minimum:
#'   \describe{
#'     \item{`a`}{integer vector of animal indices (length N)}
#'     \item{`b`}{integer vector of period indices (length N)}
#'     \item{`s`}{integer vector of study indices (length N)}
#'     \item{`na`}{scalar — expected number of unique animals}
#'     \item{`nb`}{scalar — expected number of unique periods}
#'     \item{`ns`}{scalar — expected number of unique studies}
#'     \item{`N`}{scalar — number of observations}
#'   }
#' @return Invisibly returns `TRUE` if all checks pass. Calls
#'   [cli::cli_abort()] on the first violation detected.
#' @importFrom cli cli_abort
#' @export
#' @example R/examples/Example_check_jags_indices.R
check_jags_indices <- function(input_list = NULL) {

  if(FALSE){
    input_list <- list(
      a  = c(1L, 1L, 2L, 2L),
      b  = c(1L, 2L, 1L, 2L),
      s  = c(1L, 1L, 2L, 2L),
      na = 2L,
      nb = 2L,
      ns = 2L,
      N  = 4L,
      sa = c(1L, 2L)
    )
  }

  N  <- input_list$N
  na <- input_list$na
  nb <- input_list$nb
  ns <- input_list$ns

  index_specs <- list(
    list(name = "a", vec = input_list$a, n = na),
    list(name = "b", vec = input_list$b, n = nb),
    list(name = "s", vec = input_list$s, n = ns)
  )

  for (spec in index_specs) {
    name <- spec$name
    vec  <- spec$vec
    n    <- spec$n

    # Check 1: length
    if (length(vec) != N) {
      cli::cli_abort(
        "Index vector '{name}' has length {length(vec)} but N = {N}."
      )
    }

    # Check 2: numeric and no NAs
    if (!is.numeric(vec) || anyNA(vec)) {
      cli::cli_abort(
        "Index vector '{name}' contains NA or non-numeric values."
      )
    }

    # Check 3: minimum value = 1
    vec_min <- min(vec)
    if (vec_min < 1) {
      cli::cli_abort(
        "Index vector '{name}' minimum value is {vec_min} (must be 1). Ensure indices are encoded as as.numeric(as.factor(.x))."
      )
    }

    # Check 4: gap-free 1:n sequence
    unique_vals <- sort(unique(vec))
    if (length(unique_vals) != n || !all(unique_vals == seq_len(n))) {
      cli::cli_abort(
        "Index vector '{name}' is not a gap-free 1:{n} sequence (found {length(unique_vals)} unique values but expected {n}). Check that all grouping variables are coded with composite keys before as.numeric(as.factor(.x))."
      )
    }
  }

  # Check 5: na consistency
  length_sa <- length(input_list$sa)
  if (length_sa != na) {
    cli::cli_abort(
      "'na' ({na}) does not equal length(sa) ({length_sa})."
    )
  }

  output <- TRUE
  return(invisible(output))
}


# chain_priors_from_data ----
#' @title Stats Analysis: Builds Priors From Historical Data Using Chain Model
#' @importFrom emmeans emmeans
#' @importFrom rjags jags.model coda.samples
#' @importFrom coda gelman.diag effectiveSize mcmc as.mcmc.list HPDinterval
#' @importFrom cli cli_abort cli_inform
#' @importFrom dplyr filter mutate across all_of distinct pull bind_rows group_by summarise
#' @importFrom rlang .env
#' @importFrom stats cor lm update median sd quantile
#' @importFrom ggplot2 ggplot aes geom_line geom_density geom_vline geom_point
#' @importFrom ggplot2 geom_abline geom_histogram geom_hline geom_smooth
#' @importFrom ggplot2 facet_wrap stat_qq stat_qq_line labs theme_bw theme
#' @importFrom ggplot2 element_blank element_text scale_fill_manual
#' @importFrom tidyr pivot_longer
#' @param dat is a data.frame containing historical data in a long format
#' with assay names in the assay_names_col column, and assay values
#' in the assay_name column.
#' @param animal_col string indicating the column name for animal ids. Values
#' must be unique numeric identifiers representing individual animals
#' (order-independent).
#' @param treatment_col string indicating the column name for treatment level
#' (default is "dose"). Values must be numeric mapping to treatment level
#' order (such as 1, 2, 3, 4).
#' @param assay_names_col string indicating the column name for parameters
#' (default is "parameter").
#' @param assay_name string indicating the assay name from assay_names_col
#' (default is NULL)
#' @param period_col string indicating the column name for period code
#' (default is "period_code"). Values must be numeric,
#' mapping to period level order (such as 1, 2, 3, 4).
#' @param study_col string indicating the column name for study number
#' (default is "study").  Values
#' must be unique numeric identifiers representing studies
#' (order-independent).
#' @param time1_col string indicating the column name for time point 1
#' (default is "time1")
#' @param time2_col string indicating the column name for time point 2
#' (default is "time2")
#' @param assay_values_col string indicating the column name for values to use
#' as y variable (default is "value").
#' @param n.iter.update numeric, default is 1e4. Passed to update() function.
#' @param n.iter.sample numeric, default is 1e4.
#' Passed to `coda.sample()` function
#' @param jags_seed integer seed passed to the JAGS chain RNG via `.RNG.seed`
#'   in the `inits` list. Controls the JAGS sampler's random-number generator
#'   independently of R's global PRNG (note: R-level `set.seed()` has no effect
#'   on JAGS and has been removed). Each chain receives a distinct seed derived
#'   as `jags_seed + (k - 1L)` for chain `k`, ensuring independent chains for
#'   valid Gelman-Rubin R-hat diagnostics. Set to `NULL` to use JAGS default
#'   (non-reproducible). Default is `8L`.
#' @param alpha0_pop_mean numeric, population mean for the `alpha0`
#'   (baseline) hyperprior: `dnorm(alpha0_pop_mean, 0.0004) T(0,)`.
#'   No default — must be supplied explicitly or via `study_config()`.
#'   Typical values: QTc ~250 ms, HR ~70 bpm, MBP ~80 mmHg.
#'   Passing `NULL` aborts with an informative error.
#' @param cache_file character path to an RDS cache file. If the file exists,
#'   the cached result is loaded and returned immediately (skipping MCMC).
#'   If `NULL` (default) or the file does not exist, MCMC is run and, when
#'   `cache_file` is not `NULL`, the result is saved to that path as
#'   `list(mout = mout, y_obs = y_obs)`.
#' @param verbose logical. When `TRUE` (default), prints Gelman-Rubin R-hat
#'   and effective sample size diagnostics for the four hyperparameters
#'   after a fresh JAGS fit (not applicable when loading from cache).
#' @inheritParams rjags::jags.model
#'
#' @param config an optional `PriorRhythmConfig` object created by
#'   [study_config()] or [read_study_config()]. When supplied and
#'   `active_endpoint` is set (or resolved via `endpoint`):
#'   - `config_get_endpoint(config)$alpha0_pop_mean` is used as the default
#'     for `alpha0_pop_mean` (only when `alpha0_pop_mean` is not explicitly
#'     provided).
#'   - `config_get_endpoint(config)$value_min` / `value_max` are applied as
#'     an outlier filter on assay values before MCMC. Explicit function
#'     arguments always win over config values.
#' @return A named list with model outputs for each assay, including posterior
#' samples (`pa`), model object (`m`), posterior means (`pm`), RMSE, R-squared,
#' correlation, the input data list, observed values (`y_obs`),
#' a trace plot ggplot (`trace_plot`), a list of GOF diagnostic ggplots
#' (`gof_plots`: `obs_vs_fitted`, `residual_hist`, `qq_plot`,
#' `resid_vs_fitted`), and `mcmc_diag`: an `mcmc.list` of the four
#' hyperparameters (`alpha0`, `A0`, `beta0`, `phi0`) across all chains,
#' suitable for passing to `convergence_diag()`, `coda::gelman.diag()`, or
#' `coda::effectiveSize()`. Reconstructed from `pa` when loading from cache.
#' @export
#' @md
#' @example R/examples/Example_chain_priors_from_data.R
chain_priors_from_data <- function(
  dat              = NULL,
  animal_col       = "animal",
  treatment_col    = "dose",
  period_col       = "period_code",
  study_col        = "study",
  time1_col        = "time1",
  time2_col        = "time2",
  assay_names_col  = "parameter",
  assay_values_col = "value",
  assay_name       = NULL,
  n.iter.update    = 1e4,
  n.iter.sample    = 1e4,
  n.chains         = 3,
  jags_seed        = 8L,
  alpha0_pop_mean  = NULL,
  cache_file       = NULL,
  verbose          = TRUE,
  config           = NULL
) UseMethod("chain_priors_from_data", assay_name)

#' @rdname chain_priors_from_data
#' @export
chain_priors_from_data_helper <- function(
  dat              = NULL,
  animal_col       = "animal",
  treatment_col    = "dose",
  period_col       = "period_code",
  study_col        = "study",
  time1_col        = "time1",
  time2_col        = "time2",
  assay_names_col  = "parameter",
  assay_values_col = "value",
  assay_name       = NULL,
  n.iter.update    = 1e4,
  n.iter.sample    = 1e4,
  n.chains         = 3,
  jags_seed        = 8L,
  alpha0_pop_mean  = NULL,
  cache_file       = NULL,
  verbose          = TRUE,
  config           = NULL
) {
  if (is.null(alpha0_pop_mean) && !is.null(config)) {
    ep <- config_get_endpoint(config, endpoint = assay_name)
    if (!is.null(ep$alpha0_pop_mean)) {
      alpha0_pop_mean <- ep$alpha0_pop_mean
    }
  }
  
  if(is.null(alpha0_pop_mean)){
    cli::cli_abort(
      "alpha0_pop_mean is NULL. Please provide a proper value")
  }

  if (is.null(assay_name))   cli::cli_abort("assay_name is NULL")
  if (length(assay_name) > 1) cli::cli_abort("assay_name length > 1")

  para_nam <- c("alpha0", "A0", "beta0", "phi0")

  # --- RDS cache load --------------------------------------------------------
  if (!is.null(cache_file) && file.exists(cache_file)) {
    cli::cli_inform("Loading cached Stage 1 JAGS samples from {cache_file}")
    .s1cache <- readRDS(cache_file)
    if (is.list(.s1cache) && "mout" %in% names(.s1cache)) {
      mout  <- .s1cache$mout
      y_obs <- .s1cache$y_obs
    } else {
      mout  <- .s1cache
      y_obs <- NULL
    }
    rm(.s1cache)

    inds <- match(para_nam, names(mout))
    ys   <- mout[, -inds, drop = FALSE]
    pa   <- mout[, inds, drop = FALSE]
    pm   <- colMeans(pa)
    yhat <- colMeans(ys)

    rmse <- if (!is.null(y_obs) && ncol(ys) == length(y_obs)) {
      sqrt(mean((y_obs - yhat)^2, na.rm = TRUE))
    } else {
      NA_real_
    }
    r2 <- if (!is.null(y_obs) && ncol(ys) == length(y_obs)) {
      summary(stats::lm(y_obs ~ yhat))$r.squared
    } else {
      NA_real_
    }
    cors <- if (!is.null(y_obs) && ncol(ys) == length(y_obs)) {
      stats::cor(y_obs, yhat)
    } else {
      NA_real_
    }

    gof_plots <- .build_gof_plots(
      y_obs    = y_obs,
      ys       = ys,
      label    = assay_name
    )

    n_per_chain <- nrow(pa) %/% n.chains
    mcmc_diag <- coda::as.mcmc.list(
      lapply(seq_len(n.chains), function(k) {
        idx <- seq((k - 1L) * n_per_chain + 1L, k * n_per_chain)
        coda::mcmc(as.matrix(pa[idx, , drop = FALSE]))
      })
    )

    return(list(
      pa         = pa,
      m          = NULL,
      pm         = pm,
      rmse       = rmse,
      r2         = r2,
      cors       = cors,
      input      = NULL,
      y_obs      = y_obs,
      trace_plot = NULL,
      gof_plots  = gof_plots,
      mcmc_diag  = mcmc_diag
    ))
  }

  # --- Build data for MCMC ---------------------------------------------------
  foo <- dat |>
    dplyr::filter(
      .data[[assay_names_col]] == .env$assay_name &
        !is.na(.data[[assay_values_col]])
    )
  
  if (!is.null(config)) {
    ep <- config_get_endpoint(config, endpoint = assay_name)
    
    if (!is.null(ep$value_min) && !is.null(ep$value_max)) {
      n_studies_before <- dplyr::n_distinct(foo[[study_col]])
      
      foo <- foo |>
        dplyr::filter(
          .data[[assay_values_col]] > ep$value_min &
            .data[[assay_values_col]] < ep$value_max
        )
      
      n_studies_after <- dplyr::n_distinct(foo[[study_col]])
      n_studies_dropped <- n_studies_before - n_studies_after
      
if (n_studies_dropped > 0L) {
cli::cli_inform(c(
"i" = "Endpoint {.val {assay_name}} value filtering 
removed all observations from {n_studies_dropped} of 
{n_studies_before} study level(s).",
"i" = "Unused factor levels were dropped before JAGS index encoding."
))
}
    }
  }
  
  foo <- droplevels(foo)
  
  cli::cli_inform(
    "Using {.field alpha0_pop_mean} defined in config: {.val {alpha0_pop_mean}}"
  )
  
  foo <- foo |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c(animal_col, treatment_col, period_col, study_col)),
      \(x) as.numeric(as.factor(x))
    ))

  sa <- foo |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(animal_col, study_col)))) |>
    dplyr::arrange(.data[[animal_col]]) |>   # ← this line
    dplyr::pull(study_col)

  nb <- foo |> dplyr::pull(period_col) |> unique() |> length()
 

  input <- list(
    y               = foo |> dplyr::pull(assay_values_col),
    dose            = foo |> dplyr::pull(treatment_col),
    time1           = foo |> dplyr::pull(time1_col),
    time2           = foo |> dplyr::pull(time2_col),
    s               = foo |> dplyr::pull(study_col),
    ns              = foo |> dplyr::pull(study_col) |> max(),
    a               = foo |> dplyr::pull(animal_col),
    b               = foo |> dplyr::pull(period_col),
    sa              = sa,
    na              = length(sa),
    nb              = nb,
    N               = nrow(foo),
    alpha0_pop_mean = alpha0_pop_mean
  )

  # input_checker ----
  input_checker <- function(input_list = NULL) {
    check_loop_all <- input_list[c("s", "y", "time2", "time1", "dose", "a", "b")] |>
      purrr::map(~ length(.x) == input_list$N)

    if (isFALSE(all(check_loop_all == TRUE))) {
      cli::cli_abort("all ~length(.x)==input_list$N not TRUE")
    }

    check_jags_indices(input_list)
  }

  input_checker(input)

  y_obs <- foo |> dplyr::pull(assay_values_col)

  # --- JAGS fit --------------------------------------------------------------
  con <- base::textConnection(.chain_model_jags)
  on.exit(close(con), add = TRUE)
  jags_inits <- if (!is.null(jags_seed)) {
    lapply(seq_len(n.chains), function(k) {
      list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = jags_seed + k - 1L)
    })
  } else {
    NULL
  }
  m <- rjags::jags.model(
    con,
    data     = input,
    n.chains = n.chains,
    inits    = jags_inits
  )
  stats::update(m, n.iter = n.iter.update)


  mout1 <- rjags::coda.samples(m, variable.names = c(para_nam, "mu"),
                                n.iter = n.iter.sample)
  mout  <- as.data.frame(as.matrix(mout1))
  inds  <- match(para_nam, names(mout))
  ys    <- mout[, -inds, drop = FALSE]
  pa    <- mout[, inds, drop = FALSE]

  pm    <- colMeans(pa)
  yhat  <- colMeans(ys)
  rmse  <- sqrt(mean((y_obs - yhat)^2, na.rm = TRUE))
  r2    <- summary(stats::lm(y_obs ~ yhat))$r.squared
  cors  <- stats::cor(y_obs, yhat)

  # --- Convergence diagnostics -----------------------------------------------
  mout1_diag <- coda::as.mcmc.list(lapply(mout1, function(ch) {
    coda::mcmc(as.matrix(ch)[, para_nam, drop = FALSE])
  }))

  if (isTRUE(verbose)) {
    if (length(mout1_diag) >= 2) {
      gd  <- coda::gelman.diag(mout1_diag, multivariate = FALSE)
      cli::cli_inform("Gelman-Rubin R-hat (target < 1.01; acceptable < 1.1; > 1.1 = not converged):")
      print(gd$psrf)
    }
    ess <- coda::effectiveSize(mout1_diag)
    cli::cli_inform("Effective Sample Size (should be > 400):")
    print(ess)
  }

  # --- Trace plots -----------------------------------------------------------
  trace_plot <- .build_trace_plot(mout1_diag = mout1_diag,
                                  para_nam   = para_nam,
                                  label      = assay_name)

  # --- GOF plots -------------------------------------------------------------
  gof_plots <- .build_gof_plots(y_obs = y_obs, ys = ys, label = assay_name)

  # --- Cache save ------------------------------------------------------------
  if (!is.null(cache_file)) {
    saveRDS(list(mout = mout, y_obs = y_obs), cache_file)
    cli::cli_inform("Stage 1 JAGS samples saved to {cache_file}")
  }

  output <- list(
    pa         = pa,
    m          = m,
    pm         = pm,
    rmse       = rmse,
    r2         = r2,
    cors       = cors,
    input      = input,
    y_obs      = y_obs,
    trace_plot = trace_plot,
    gof_plots  = gof_plots,
    mcmc_diag  = mout1_diag
  )

  return(output)
}


#' @rdname chain_priors_from_data
#' @export
chain_priors_from_data.default <- function(
  dat              = NULL,
  animal_col       = "animal",
  treatment_col    = "dose",
  period_col       = "period_code",
  study_col        = "study",
  time1_col        = "time1",
  time2_col        = "time2",
  assay_names_col  = "parameter",
  assay_values_col = "value",
  assay_name       = NULL,
  n.iter.update    = 1e4,
  n.iter.sample    = 1e4,
  n.chains         = 3,
  jags_seed        = 8L,
  alpha0_pop_mean  = NULL,
  cache_file       = NULL,
  verbose          = TRUE,
  config           = NULL
) {
  if (is.null(alpha0_pop_mean) && !is.null(config)) {
    ep <- config_get_endpoint(config, endpoint = assay_name)
    if (!is.null(ep$alpha0_pop_mean)) {
      alpha0_pop_mean <- ep$alpha0_pop_mean
    }
  }

  if (is.null(alpha0_pop_mean)) {
    cli::cli_abort(
      "alpha0_pop_mean is NULL. Please provide a value appropriate for the endpoint (e.g. QTc ~250 ms, HR ~70 bpm, MBP ~80 mmHg) or set it via study_config()."
    )
  }

  if (is.null(assay_name)) {
    cli::cli_abort("assay_name must not be NULL")
  }

  prior_selection <- assay_name |> purrr::set_names(~ .x)

  output <- prior_selection |>
    purrr::imap(function(x_assay, y_assay) {
      cli::cli_inform("{y_assay}")
      chain_priors_from_data_helper(
        dat              = dat,
        animal_col       = animal_col,
        treatment_col    = treatment_col,
        period_col       = period_col,
        study_col        = study_col,
        time1_col        = time1_col,
        time2_col        = time2_col,
        assay_names_col  = assay_names_col,
        assay_values_col = assay_values_col,
        assay_name       = x_assay,
        n.iter.update    = n.iter.update,
        n.iter.sample    = n.iter.sample,
        n.chains         = n.chains,
        jags_seed        = jags_seed,
        alpha0_pop_mean  = alpha0_pop_mean,
        cache_file       = cache_file,
        verbose          = verbose,
        config           = config
      )
    })

  return(output)
}

# .build_trace_plot ----
# Internal helper: build ggplot trace plot for JAGS hyperparameter chains.
.build_trace_plot <- function(mout1_diag = NULL, para_nam = NULL, label = NULL) {
  pa_trace <- do.call(rbind, lapply(seq_along(mout1_diag), function(ch) {
    d           <- as.data.frame(as.matrix(mout1_diag[[ch]])[, para_nam, drop = FALSE])
    d$iteration <- seq_len(nrow(d))
    d$chain     <- paste0("chain ", ch)
    d
  })) |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(para_nam),
      names_to  = "parameter",
      values_to = "value"
    )

  ggplot2::ggplot(pa_trace,
                  ggplot2::aes(x     = .data[["iteration"]],
                               y     = .data[["value"]],
                               colour = .data[["chain"]])) +
    ggplot2::geom_line(alpha = 0.6, linewidth = 0.4) +
    ggplot2::facet_wrap(~ .data[["parameter"]], scales = "free_y") +
    ggplot2::labs(
      title  = paste("Stage 1 trace plots \u2014", label, "hyperparameters"),
      x      = "Iteration",
      y      = NULL,
      colour = NULL
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position  = "bottom",
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold")
    )
}

# .build_gof_plots ----
# Internal helper: build Stage 1 goodness-of-fit diagnostic plots.
# Returns a named list of four ggplot objects, or NULLs when y_obs is
# unavailable or the dimensions do not match.
.build_gof_plots <- function(y_obs = NULL, ys = NULL, label = NULL) {
  if (is.null(y_obs) || is.null(ys) || ncol(ys) != length(y_obs)) {
    return(list(obs_vs_fitted   = NULL,
                residual_hist   = NULL,
                qq_plot         = NULL,
                resid_vs_fitted = NULL))
  }

  mu_hat   <- colMeans(ys)
  ss_res   <- sum((y_obs - mu_hat)^2)
  ss_tot   <- sum((y_obs - mean(y_obs))^2)
  bayes_r2 <- 1 - ss_res / ss_tot

  gof_df <- data.frame(
    observed = y_obs,
    fitted   = mu_hat,
    residual = y_obs - mu_hat
  )

  p_ovf <- ggplot2::ggplot(gof_df,
                            ggplot2::aes(x = .data[["fitted"]],
                                         y = .data[["observed"]])) +
    ggplot2::geom_point(alpha = 0.4, colour = "steelblue") +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         colour = "firebrick", linewidth = 0.8) +
    ggplot2::labs(
      title    = paste("Stage 1: observed vs fitted \u2014", label),
      subtitle = sprintf("Bayesian R\u00b2 = %.4f", bayes_r2),
      x        = "Posterior mean fitted value",
      y        = "Observed"
    ) +
    ggplot2::theme_bw(base_size = 11)

  p_rhist <- ggplot2::ggplot(gof_df,
                              ggplot2::aes(x = .data[["residual"]])) +
    ggplot2::geom_histogram(bins = 40, fill = "steelblue",
                             colour = "white", alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 0, colour = "firebrick", linewidth = 0.8) +
    ggplot2::labs(
      title = paste("Stage 1: residual distribution \u2014", label),
      x     = "Residual",
      y     = "Count"
    ) +
    ggplot2::theme_bw(base_size = 11)

  p_qq <- ggplot2::ggplot(gof_df,
                           ggplot2::aes(sample = .data[["residual"]])) +
    ggplot2::stat_qq(alpha = 0.4, colour = "steelblue") +
    ggplot2::stat_qq_line(colour = "firebrick", linewidth = 0.8) +
    ggplot2::labs(
      title = paste("Stage 1: residual Q-Q plot \u2014", label),
      x     = "Theoretical quantiles",
      y     = "Sample quantiles"
    ) +
    ggplot2::theme_bw(base_size = 11)

  p_rvf <- ggplot2::ggplot(gof_df,
                            ggplot2::aes(x = .data[["fitted"]],
                                         y = .data[["residual"]])) +
    ggplot2::geom_point(alpha = 0.4, colour = "steelblue") +
    ggplot2::geom_hline(yintercept = 0, colour = "firebrick", linewidth = 0.8) +
    ggplot2::geom_smooth(method = "loess", se = TRUE, colour = "darkorange",
                         linewidth = 0.7, fill = "darkorange", alpha = 0.2) +
    ggplot2::labs(
      title = paste("Stage 1: residuals vs fitted \u2014", label),
      x     = "Posterior mean fitted value",
      y     = "Residual"
    ) +
    ggplot2::theme_bw(base_size = 11)

  list(
    obs_vs_fitted   = p_ovf,
    residual_hist   = p_rhist,
    qq_plot         = p_qq,
    resid_vs_fitted = p_rvf
  )
}

# chain_prior_plot ----
#' @title Plots Prior Distributions
#' @importFrom utils head str
#' @importFrom purrr chuck map set_names imap
#' @importFrom stats quantile
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line
#' @importFrom ggplot2 scale_linetype_manual scale_x_continuous
#' @importFrom ggplot2 scale_color_brewer scale_fill_brewer
#' @importFrom ggplot2 labs ylab theme_bw theme
#' @param interval_start_times numeric vector,
#' default is c(0,3,6,12,18).
#' @param interval_end_times numeric vector,
#' default is c(3,6,12,18,24)
#' @param doses numeric vector,
#' default is seq(0,3)
#' @param prior_list Named list with each object
#' containing the output from the chain_priors_from_data function.
#' @param prior_name string for prior_list object to
#' use.
#' @return A named list with two ggplot objects: `time_v_prior` and
#' `timebins_v_prior`.
#' @export
#' @example R/examples/Example_chain_prior_plot.R
chain_prior_plot <- function(
  interval_start_times = c(0, 3, 6, 12, 18),
  interval_end_times   = c(3, 6, 12, 18, 24),
  doses                = seq(0, 3),
  prior_list           = NULL,
  prior_name           = NULL
) {
  chain_prior <- purrr::chuck(prior_list, prior_name, "pa")

  uut1 <- interval_start_times
  uut2 <- interval_end_times
  uut  <- paste(uut1, "to", uut2)
  uud  <- doses
  nt   <- length(uut1)
  nd   <- length(uud)

  # df_chain_prior_plot ----
  df_chain_prior_plot <- function(i, j) {
    A0     <- chain_prior$A0
    alpha0 <- chain_prior$alpha0
    beta0  <- chain_prior$beta0
    phi0   <- chain_prior$phi0

    skl <- alpha0 +
      sinusoidal_interval_mean(A = A0, t1 = uut1[i], t2 = uut2[i], phi = phi0) +
      beta0 * uud[j]

    out <- stats::quantile(skl, probs = c(0.05, 0.5, 0.95), names = FALSE) |>
      t() |>
      as.data.frame()
    return(out)
  }

  idx <- expand.grid(i = seq(1, nt), j = seq(1, nd))
  ttt <- Map(df_chain_prior_plot, idx$i, idx$j) |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    tibble::rownames_to_column(var = "id") |>
    dplyr::mutate(id   = as.numeric(.data$id)) |>
    dplyr::mutate(time = ((.data$id - 1) %% nt)) |>
    dplyr::mutate(dose = (ceiling(.data$id / nt)))

  p1 <- ttt |>
    dplyr::mutate(dplyr::across(dplyr::all_of("time"), as.factor)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["dose"]], linetype = .data[["time"]])) +
    list(
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[["V1"]], ymax = .data[["V3"]],
                     fill = .data[["time"]]),
        alpha = 0.2, show.legend = FALSE
      ),
      ggplot2::geom_line(ggplot2::aes(y = .data[["V2"]])),
      ggplot2::scale_linetype_manual(labels = uut, values = as.factor(uut2)),
      ggplot2::labs(linetype = "Time"),
      ggplot2::ylab(prior_name),
      ggplot2::theme_bw(base_size = 16),
      ggplot2::theme(
        plot.margin    = grid::unit(c(1, 1, 1, 1), "cm"),
        legend.position = "bottom"
      ),
      ggplot2::scale_color_brewer(palette = "Dark2"),
      ggplot2::scale_fill_brewer(palette = "YlOrRd")
    )

  p2 <- ttt |>
    dplyr::mutate(dplyr::across(dplyr::all_of("dose"), as.factor)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["time"]], linetype = .data[["dose"]])) +
    list(
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[["V1"]], ymax = .data[["V3"]],
                     fill = .data[["dose"]]),
        alpha = 0.2, show.legend = FALSE
      ),
      ggplot2::geom_line(ggplot2::aes(y = .data[["V2"]])),
      ggplot2::scale_linetype_manual(labels = uud + 1, values = as.factor(uud)),
      ggplot2::labs(linetype = "Dose"),
      ggplot2::scale_x_continuous(breaks = 0:(nt - 1), labels = uut),
      ggplot2::ylab(prior_name),
      ggplot2::theme_bw(base_size = 16),
      ggplot2::theme(
        plot.margin     = grid::unit(c(1, 1, 1, 1), "cm"),
        legend.position = "bottom"
      ),
      ggplot2::scale_color_brewer(palette = "Dark2"),
      ggplot2::scale_fill_brewer(palette = "YlOrRd")
    )

  output <- list(time_v_prior = p1, timebins_v_prior = p2)
  return(output)
}


# check_jags_inputs ----
#' @title Validate and Return JAGS Input List Without Running MCMC
#' @description Builds the JAGS input list from a long-format data frame and
#'   runs all consistency checks without fitting the model. Useful for
#'   diagnosing index encoding issues before a slow MCMC run.
#' @inheritParams chain_priors_from_data
#' @return A named list as passed to [rjags::jags.model()], invisibly.
#' @importFrom dplyr filter mutate across all_of distinct pull arrange
#' @importFrom purrr map
#' @importFrom cli cli_abort cli_inform
#' @importFrom rlang .env
#' @export
check_jags_inputs <- function(
    dat              = NULL,
    animal_col       = "animal",
    treatment_col    = "dose",
    period_col       = "period_code",
    study_col        = "study",
    time1_col        = "time1",
    time2_col        = "time2",
    assay_names_col  = "parameter",
    assay_values_col = "value",
    assay_name       = NULL,
    n.iter.update    = 1e4,
    n.iter.sample    = 1e4,
    n.chains         = 3,
    jags_seed        = 8L,
    alpha0_pop_mean  = NULL,
    cache_file       = NULL,
    verbose          = TRUE,
    config           = NULL) {
  
  if(FALSE){
    dat             = NULL
    animal_col      = "animal"
    treatment_col   = "dose"
    period_col      = "period_code"
    study_col       = "study"
    time1_col       = "time1"
    time2_col       = "time2"
    assay_names_col  = "parameter"
    assay_values_col = "value"
    assay_name      = NULL
    alpha0_pop_mean = NULL
    config          = NULL
  }
  
  # --- Build data for MCMC ---------------------------------------------------
  foo <- dat |>
    dplyr::filter(
      .data[[assay_names_col]] == .env$assay_name &
        !is.na(.data[[assay_values_col]])
    )
  
  if (!is.null(config)) {
    ep <- config_get_endpoint(config, endpoint = assay_name)
    
    if (!is.null(ep$value_min) && !is.null(ep$value_max)) {
      n_studies_before <- dplyr::n_distinct(foo[[study_col]])
      
      foo <- foo |>
        dplyr::filter(
          .data[[assay_values_col]] > ep$value_min &
            .data[[assay_values_col]] < ep$value_max
        )
      
      n_studies_after <- dplyr::n_distinct(foo[[study_col]])
      n_studies_dropped <- n_studies_before - n_studies_after
      
      if (n_studies_dropped > 0L) {
        cli::cli_inform(c(
          "i" = "Endpoint {.val {assay_name}} value filtering removed all observations from {n_studies_dropped} of {n_studies_before} study level(s).",
          "i" = "Unused factor levels were dropped before JAGS index encoding."
        ))
      }
    }
  }
  
  foo <- droplevels(foo)
  
  foo <- foo |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c(animal_col, treatment_col, period_col, study_col)),
      \(x) as.numeric(as.factor(x))
    ))
  
  sa <- foo |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(animal_col, study_col)))) |>
    dplyr::arrange(.data[[animal_col]]) |>
    dplyr::pull(study_col)
  
  nb <- foo |> dplyr::pull(period_col) |> unique() |> length()
  
  input <- list(
    y               = foo |> dplyr::pull(assay_values_col),
    dose            = foo |> dplyr::pull(treatment_col),
    time1           = foo |> dplyr::pull(time1_col),
    time2           = foo |> dplyr::pull(time2_col),
    s               = foo |> dplyr::pull(study_col),
    ns              = foo |> dplyr::pull(study_col) |> max(),
    a               = foo |> dplyr::pull(animal_col),
    b               = foo |> dplyr::pull(period_col),
    sa              = sa,
    na              = length(sa),
    nb              = nb,
    N               = nrow(foo),
    alpha0_pop_mean = alpha0_pop_mean
  )
  
  # input_checker ----
  input_checker <- function(input_list = NULL) {
    check_loop_all <- input_list[c("s", "y", "time2", "time1", "dose", "a", "b")] |>
      purrr::map(~ length(.x) == input_list$N)
    
    if (isFALSE(all(check_loop_all == TRUE))) {
      cli::cli_abort("all ~length(.x)==input_list$N not TRUE")
    }
    
    # sa checks
    if (length(input_list$sa) != input_list$na) {
      cli::cli_abort("`sa` has length {length(input_list$sa)} but na = {input_list$na}.")
    }
    
    if (!all(input_list$sa[input_list$a] == input_list$s)) {
      cli::cli_abort("sa[a[i]] != s[i]: animal-to-study mapping is inconsistent.")
    }
    
    if (anyNA(input_list$time1) || anyNA(input_list$time2)) {
      cli::cli_abort("`time1` or `time2` contains NA values.")
    }
    
    if (!all(input_list$time1 < input_list$time2)) {
      cli::cli_abort("Not all `time1 < time2`.")
    }
    
    if (anyNA(input_list$y)) {
      cli::cli_abort("`y` contains NA values after filtering.")
    }
    
    check_jags_indices(input_list)
  }
  
  input_checker(input)
  
  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "v" = "All checks passed.",
      "*" = "N = {input$N}, na = {input$na}, ns = {input$ns}, nb = {input$nb}"
    ))
  }
  
  return(invisible(input))
}