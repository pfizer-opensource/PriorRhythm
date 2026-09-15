# generate_borrowing_report ----
#' @title Generate Word Report: Bayesian Borrowing Models A vs D
#' @description Produces a `.docx` Word report containing:
#'   - A formatted summary table (Model A vs D treatment effects, 95% HPD,
#'     and `P(|Δ| > threshold)`), with rows shaded grey where the
#'     exceedance probability is ≥ 0.95.
#'   - Time-on-x plots for Models A and D with a shared y-axis.
#' @importFrom cli cli_abort cli_inform
#' @importFrom dplyr mutate
#' @importFrom purrr map_dfr
#' @importFrom coda HPDinterval mcmc
#' @param mod output of [bayesian_test_example()].
#' @param endpoint character label for the endpoint (e.g. `"HR"`).
#'   Default is `""`.
#' @param endpoint_unit character unit string (e.g. `"bpm"`). Default is `""`.
#' @param threshold numeric decision threshold for `P(|Δ| > threshold)`.
#'   Default is `0`.
#' @param compound character compound name for the report header.
#'   Default is `""`.
#' @param outfile character path for the output `.docx` file.
#' @param ut1 numeric vector of time-window start values.  When `NULL`
#'   (default), these are inferred from `names(mod$out)`.
#' @param ut2 numeric vector of time-window end values.  When `NULL`
#'   (default), these are inferred from the data stored in the model fits.
#' @return Invisibly returns the path to the written `.docx` file.
#' @export
#' @example R/examples/Example_generate_borrowing_report.R
generate_borrowing_report <- function(
  mod           = NULL,
  endpoint      = "",
  endpoint_unit = "",
  threshold     = 0,
  compound      = "",
  outfile       = NULL,
  ut1           = NULL,
  ut2           = NULL
) {
  if (is.null(mod))    cli::cli_abort("{.arg mod} must not be NULL")
  if (is.null(outfile)) cli::cli_abort("{.arg outfile} must not be NULL")

  for (pkg in c("officer", "flextable")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg {pkg}} is required for {.fn generate_borrowing_report}. \\
         Install with: install.packages('{pkg}')"
      )
    }
  }

  cfg <- list(
    COMPOUND         = compound,
    PARAMETER        = endpoint,
    PARAMETER_UNIT   = endpoint_unit,
    effect_threshold = threshold
  )

  nt <- length(mod$out)
  if (is.null(ut1)) ut1 <- as.numeric(names(mod$out))
  if (is.null(ut2)) {
    ut2 <- vapply(names(mod$out), function(nm) {
      d <- mod$out[[nm]][[1]]$data
      if (!is.null(d) && "time2" %in% names(d)) unique(d$time2)[1L] else NA_real_
    }, numeric(1L))
  }

  # Helper: extract posterior contrasts from a Sol matrix
  extract_contrasts <- function(sol, ut1_val, ut2_val) {
    b      <- grep("treatment", colnames(sol))
    ctrl   <- as.numeric(sol[, b[1L]])
    non_b  <- b[-1L]
    trt_codes <- as.numeric(sub(".*\\)", "", colnames(sol)[non_b]))
    purrr::map_dfr(seq_along(non_b), function(j) {
      samp <- as.numeric(sol[, non_b[j]]) - ctrl
      hpd  <- coda::HPDinterval(coda::mcmc(samp))
      data.frame(
        time_window    = paste0(ut1_val, "\u2013", ut2_val, " hr"),
        treatment_code = trt_codes[j],
        mean_effect    = mean(samp),
        lower_HPD      = hpd[, "lower"],
        upper_HPD      = hpd[, "upper"],
        p_abs_exceed   = mean(abs(samp) > cfg$effect_threshold),
        stringsAsFactors = FALSE
      )
    })
  }

  model_a_table <- purrr::map_dfr(
    seq_len(nt),
    function(i) extract_contrasts(mod$out[[i]]$Non_informative$Sol, ut1[i], ut2[i])
  )
  model_d_table <- purrr::map_dfr(
    seq_len(nt),
    function(i) extract_contrasts(mod$out[[i]]$Informative$Sol, ut1[i], ut2[i])
  )

  col_prob <- sprintf(
    "P(|\u0394%s|>%g %s)",
    cfg$PARAMETER, cfg$effect_threshold, cfg$PARAMETER_UNIT
  )

  combined <- merge(
    model_a_table, model_d_table,
    by      = c("time_window", "treatment_code"),
    suffixes = c("_A", "_D"),
    all     = TRUE
  )

  ft_data <- data.frame(
    tw     = combined$time_window,
    trt    = as.character(combined$treatment_code),
    mn_A   = sprintf("%.2f", combined$mean_effect_A),
    lo_A   = sprintf("%.2f", combined$lower_HPD_A),
    hi_A   = sprintf("%.2f", combined$upper_HPD_A),
    mn_D   = sprintf("%.2f", combined$mean_effect_D),
    lo_D   = sprintf("%.2f", combined$lower_HPD_D),
    hi_D   = sprintf("%.2f", combined$upper_HPD_D),
    prob_D = sprintf("%.3f", combined$p_abs_exceed_D),
    stringsAsFactors = FALSE
  )

  shade_rows_D <- which(combined$p_abs_exceed_D >= 0.95)

  # Time-on-x plots with shared y limits
  shared_ylim <- range(
    c(model_a_table$lower_HPD, model_a_table$upper_HPD,
      model_d_table$lower_HPD, model_d_table$upper_HPD),
    na.rm = TRUE
  )
  shared_ylim <- shared_ylim + c(-1, 1) * 0.05 * diff(shared_ylim)

  p_model_A <- make_time_x_plot(
    tbl         = model_a_table,
    model_label = "Weak-prior comparator \u2014 full data",
    cfg         = cfg,
    ylim        = shared_ylim,
    ut1         = ut1,
    ut2         = ut2
  )
  p_model_D <- make_time_x_plot(
    tbl         = model_d_table,
    model_label = "Historical prior \u2014 full data",
    cfg         = cfg,
    ylim        = shared_ylim,
    ut1         = ut1,
    ut2         = ut2
  )

  # Build flextable
  ft <- flextable::flextable(ft_data)
  ft <- flextable::set_header_labels(
    ft,
    tw     = "Time Window",      trt  = "Trt Code",
    mn_A   = "Mean",             lo_A = "Lower 95% HPD",  hi_A = "Upper 95% HPD",
    mn_D   = "Mean",             lo_D = "Lower 95% HPD",  hi_D = "Upper 95% HPD",
    prob_D = col_prob
  )
  ft <- flextable::add_header_row(
    ft,
    values    = c("", "Weak-prior comparator \u2014 full data",
                  "Historical prior \u2014 full data"),
    colwidths = c(2L, 3L, 4L)
  )
  ft <- flextable::set_caption(
    ft,
    caption = sprintf(
      "Model A vs D \u2014 \u0394%s treatment effects vs control (95%% HPD)",
      cfg$PARAMETER
    )
  )
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::align(ft, i = 1, align = "center", part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::width(
    ft,
    j     = c("tw", "trt", "mn_A", "lo_A", "hi_A",
               "mn_D", "lo_D", "hi_D", "prob_D"),
    width = c(1.2, 0.5, 0.6, 0.75, 0.75, 0.6, 0.75, 0.75, 0.6)
  )
  ft <- flextable::set_table_properties(ft, layout = "fixed")

  if (length(shade_rows_D) > 0) {
    ft <- flextable::bg(ft, i = shade_rows_D, bg = "#d3d3d3")
  }

  # Build Word doc
  doc <- officer::read_docx()
  doc <- officer::body_add_par(
    doc,
    sprintf("Bayesian Borrowing Analysis \u2014 Models A & D (%s)", endpoint),
    style = "heading 1"
  )
  doc <- officer::body_add_par(
    doc,
    sprintf(
      "Compound: %s  |  Parameter: %s (%s)  |  Threshold: \u00b1%g %s",
      compound, endpoint, endpoint_unit, threshold, endpoint_unit
    ),
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_gg(doc, p_model_A, width = 6.5, height = 4.0)
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_gg(doc, p_model_D, width = 6.5, height = 4.0)

  print(doc, target = path.expand(outfile))
  cli::cli_inform("Word report saved to: {path.expand(outfile)}")
  invisible(outfile)
}
