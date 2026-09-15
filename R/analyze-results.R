# ci_plots ----
#' @title Plotting: Produces Plots With Confidence Intervals
#' @inheritParams chain_priors_from_data
#' @inheritParams bayesian_test_example
#' @importFrom cli cli_inform
#' @importFrom dplyr mutate across all_of pull
#' @importFrom emmeans emmeans
#' @importFrom forcats fct_relevel
#' @importFrom glue glue
#' @importFrom purrr imap_chr imap_dfr set_names map
#' @importFrom stats as.formula
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point geom_hline geom_line
#' @importFrom ggplot2 facet_wrap label_both
#' @importFrom ggplot2 scale_shape_manual scale_color_manual
#' @importFrom ggplot2 theme theme_classic element_text position_dodge
#' @importFrom ggplot2 ylab
#' @param time_code_col string indicating the column name for time codes
#'   (default is `"time_code"`).
#' @param input_method named character vector mapping display labels to
#'   internal method identifiers. To rename labels provide a named vector.
#' @param mod output of `bayesian_test_example()`.
#' @param threshold numeric y-intercept for the threshold line. Default is `0`.
#' @param linewidth numeric, passed to `geom_errorbar`. Default is `0.5`.
#' @param color_threshold character string setting the colour for the threshold
#'   horizontal line.
#' @param color_levels named character vector of colours. Names must match the
#'   method identifiers.
#' @return A ggplot object showing estimated effects with confidence intervals,
#'   faceted by time code.
#' @export
#' @example R/examples/Example_ci_plots.R
ci_plots <- function(mod             = NULL,
                     threshold       = 0,
                     animal_col      = "Animal",
                     treatment_col   = "treatment_code",
                     period_col      = "period_code",
                     time1_col       = "time1",
                     time2_col       = "time2",
                     y_col           = "y",
                     time_code_col   = "time_code",
                     input_method    = .method_choices(wrap = TRUE),
                     linewidth       = 0.5,
                     color_threshold = "darkgrey",
                     color_levels    = c(
                       Non_informative            = "#1B9E77",
                       Non_informative_Incomplete = "#D95F02",
                       Informative_Incomplete     = "#7570B3",
                       Informative                = "#E7298A"
                     )) {
  
  new_order <- c("Informative", "Informative_Incomplete",
                 "Non_informative_Incomplete", "Non_informative")
  
  if (!is.null(names(input_method))) {
    matched_methods <- input_method |>
      purrr::imap_chr(\(x, y) match.arg(arg = x, choices = new_order, several.ok = FALSE)) |>
      unlist()
    
    filt_order      <- new_order[new_order %in% matched_methods]
    matched_methods <- matched_methods[order(matched_methods, levels = filt_order)]
  } else {
    matched_methods <- match.arg(
      input_method,
      choices    = c("Non_informative", "Non_informative_Incomplete",
                     "Informative_Incomplete", "Informative"),
      several.ok = TRUE
    ) |>
      purrr::set_names(\(x) x)
    
    filt_order      <- new_order[new_order %in% matched_methods]
    matched_methods <- matched_methods[order(matched_methods, levels = filt_order)]
  }
  
  em_form_string <- glue::glue("trt.vs.ctrl~ as.factor({treatment_col})")
  cli::cli_inform("emmeans: {em_form_string}")
  
  final_method <- matched_methods[!is.na(matched_methods)]
  
  if (is.null(color_levels)) {
    break_input   <- c("Non_informative", "Non_informative_Incomplete",
                       "Informative_Incomplete", "Informative")
    colorset_init <- purrr::set_names(
      c("#1F78B4", "#A6CEE3", "#CAB2D6", "#6A3D9A"), break_input
    )
  } else {
    break_input   <- names(color_levels)
    colorset_init <- color_levels
  }
  
  shape_set_init <- purrr::set_names(c(19, 1, 1, 19), break_input)
  
  filt_break_input <- break_input[break_input %in% final_method]
  matched_breaks   <- final_method[order(final_method, levels = filt_break_input)]
  
  colorset  <- colorset_init[matched_breaks]
  shape_set <- shape_set_init[matched_breaks]
  
  all_stats <- final_method
  
  completed_stats <- mod$out |>
    purrr::imap_dfr(function(x_time, y_time) {
      cli::cli_inform("Time point: {y_time}")
      x_time[all_stats] |>
        purrr::set_names(unname(all_stats)) |>
        purrr::imap_dfr(function(x2, y2) {
          emmeans::emmeans(x2,
                           specs = stats::as.formula(em_form_string),
                           data  = x2$data)$contrasts |>
            as.data.frame() |>
            dplyr::mutate(method    = y2) |>
            dplyr::mutate(TimePoint = y_time)
        })
    }) |>
    tibble::as_tibble()
  
  completed_stats |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of("contrast"),
      function(x) gsub("treatment_code", "trt ", x)
    )) |>
    dplyr::mutate(LSD = (.data$upper.HPD - .data$lower.HPD) / 2,
                  .before = "estimate") |>
    dplyr::mutate(TimeCode = as.numeric(as.factor(as.numeric(.data$TimePoint)))) |>
    dplyr::mutate(method = forcats::fct_relevel(as.factor(.data$method), unname(all_stats))) |>
    ggplot2::ggplot(
      ggplot2::aes(
        y     = .data[["estimate"]],
        x     = .data[["contrast"]],
        shape = .data[["method"]],
        color = .data[["method"]]
      )
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[["lower.HPD"]], ymax = .data[["upper.HPD"]]),
      width     = 0.5,
      linewidth = linewidth,
      position  = ggplot2::position_dodge(0.5)
    ) +
    ggplot2::geom_point(position = ggplot2::position_dodge(0.5)) +
    ggplot2::ylab("Estimated effect") +
    ggplot2::geom_hline(yintercept = threshold,
                        linetype   = "dashed",
                        color      = color_threshold) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid") +
    ggplot2::facet_wrap(~TimeCode, labeller = label_both) +
    ggplot2::theme_classic() +
    ggplot2::scale_shape_manual(drop = TRUE, values = shape_set,
                                labels = setNames(names(matched_breaks),
                                                  unname(matched_breaks))) +
    ggplot2::scale_color_manual(drop = TRUE, values = colorset,
                                labels = setNames(names(matched_breaks),
                                                  unname(matched_breaks))) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
}

# prob_statement_plots ----
#' @title Plotting: Produces Plots With Probability of Effect Greater Than Threshold
#' @inheritParams ci_plots
#' @importFrom cli cli_inform
#' @importFrom dplyr mutate rename any_of
#' @importFrom glue glue
#' @importFrom purrr imap imap_dfr set_names map map_dfr
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_hline ylab facet_wrap
#' @importFrom ggplot2 scale_color_manual scale_y_continuous theme_classic
#' @return A ggplot object showing probability of treatment effect per time code,
#'   faceted by model method.
#' @export
#' @details
#' If `threshold >= 0`:
#' `colMeans(post[, se, drop = FALSE] - post[, 1] > threshold)`
#'
#' If `threshold < 0`:
#' `colMeans(post[, se, drop = FALSE] - post[, 1] < threshold)`
#'
#' @example R/examples/Example_prob_statement_plots.R
prob_statement_plots <- function(mod          = NULL,
                                 threshold    = 0,
                                 color_levels = c(
                                   "2" = "#66C2A5",
                                   "3" = "#FC8D62",
                                   "4" = "#8DA0CB"
                                 ),
                                 input_method = .method_choices(wrap = TRUE)) {
  
  res_rvs   <- mod$out
  new_order <- c("Informative", "Informative_Incomplete",
                 "Non_informative_Incomplete", "Non_informative")
  
  if (!is.null(names(input_method))) {
    matched_methods <- input_method |>
      purrr::imap_chr(\(x, y) match.arg(arg = x, choices = new_order, several.ok = FALSE)) |>
      unlist()
    
    filt_order      <- new_order[new_order %in% matched_methods]
    matched_methods <- matched_methods[order(matched_methods, levels = filt_order)]
  } else {
    matched_methods <- match.arg(
      input_method,
      choices    = c("Non_informative", "Non_informative_Incomplete",
                     "Informative_Incomplete", "Informative"),
      several.ok = TRUE
    ) |>
      purrr::set_names(\(x) x)
    
    filt_order      <- new_order[new_order %in% matched_methods]
    matched_methods <- matched_methods[order(matched_methods, levels = filt_order)]
  }
  
  if (threshold >= 0) {
    comparitor  <- "> "
    yaxis_label <- glue::glue("Prob(Treatment Effect {comparitor}{threshold})")
    cli::cli_inform(yaxis_label)
    prob_statement_fn <- function(post = NULL, se = NULL, threshold = NULL) {
      colMeans(post[, se, drop = FALSE] - post[, 1] > threshold)
    }
  } else {
    comparitor  <- "< "
    yaxis_label <- glue::glue("Prob(Treatment Effect {comparitor}{threshold})")
    cli::cli_inform(yaxis_label)
    prob_statement_fn <- function(post = NULL, se = NULL, threshold = NULL) {
      colMeans(post[, se, drop = FALSE] - post[, 1] < threshold)
    }
  }
  
  list_prob <- matched_methods |>
    purrr::imap(function(x1, y1) {
      prob_great <- function(x) {
        post    <- res_rvs[[x]][[x1]]$Sol
        col_len <- length(colnames(post))
        se      <- seq(2, col_len)
        cli::cli_inform("se: {se}")
        prob_statement_fn(post = post, se = se, threshold = threshold)
      }
      
      prob_greater_all <- purrr::map_dfr(seq(1, length(res_rvs)), prob_great,
                                         .id = "time_code")
      prob_greater_all |>
        tidyr::pivot_longer(!dplyr::any_of("time_code"), names_to = "treatment_code") |>
        dplyr::mutate(treatment_code = gsub("as.factor\\(treatment_code\\)", "",
                                            .data$treatment_code))
    })
  
  list_prob |>
    purrr::imap_dfr(\(x, y) dplyr::mutate(x, method = y)) |>
    dplyr::rename("probability" = "value") |>
    ggplot2::ggplot(ggplot2::aes(
      x        = .data[["time_code"]],
      y        = .data[["probability"]],
      linetype = .data[["treatment_code"]]
    )) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data[["treatment_code"]], color = .data[["treatment_code"]]),
      linetype  = 1,
      linewidth = 1
    ) +
    list(
      ggplot2::ylab(yaxis_label),
      ggplot2::scale_color_manual(values = color_levels, name = "Treatment"),
      ggplot2::scale_y_continuous(limits = c(0, 1)),
      ggplot2::geom_hline(yintercept = 0.05, linetype = 2),
      ggplot2::theme_classic(),
      ggplot2::facet_wrap(~.data$method)
    )
}
# ci_plots_combined ----
#' @title Plotting: Combined Faceted A/B/C vs A/D Comparison Plot
#' @description Produces a `facet_grid(comparison ~ time_label)` ggplot showing
#'   all four models on a shared y-axis.  Row 1 compares Models A, B, C
#'   (partial-data models against the full-data weak-prior reference); Row 2
#'   compares the full-data weak-prior comparator and historical-prior models.
#'   Percentage HPD CI overlap with Model A is annotated above each error bar,
#'   and a dotted reference line marks the threshold.
#' @importFrom cli cli_inform cli_abort
#' @importFrom dplyr mutate filter bind_rows
#' @importFrom emmeans emmeans
#' @importFrom ggplot2 ggplot aes geom_hline geom_errorbar geom_point geom_text
#' @importFrom ggplot2 scale_colour_manual facet_grid labs theme_bw theme
#' @importFrom ggplot2 element_blank element_text position_dodge
#' @importFrom glue glue
#' @importFrom purrr imap_dfr map_dfr set_names
#' @importFrom stats as.formula
#' @importFrom tibble as_tibble
#' @inheritParams ci_plots
#' @param endpoint character label used in the y-axis title (e.g. `"HR"`).
#'   Default is `""`.
#' @param endpoint_unit character unit string (e.g. `"bpm"`). Default is `""`.
#' @param model_colours named character vector of colours. Names must match the
#'   displayed labels for Models A through D shown in the function usage.
#' @return A ggplot object.
#' @export
#' @example R/examples/Example_ci_plots_combined.R
ci_plots_combined <- function(
  mod             = NULL,
  threshold       = 0,
  treatment_col   = "treatment_code",
  time1_col       = "time1",
  time2_col       = "time2",
  endpoint        = "",
  endpoint_unit   = "",
  model_colours   = c(
    "A: weak-prior comparator \u2014 full data (reference)" = "black",
    "B: weak-prior comparator \u2014 partial data"          = "red",
    "C: historical prior \u2014 partial data"               = "blue",
    "D: historical prior \u2014 full data"                  = "purple"
  )
) {
  if (is.null(mod)) cli::cli_abort("{.arg mod} must not be NULL")

  em_form_string <- glue::glue(
    "trt.vs.ctrl~ as.factor({treatment_col})"
  )
  cli::cli_inform("emmeans: {em_form_string}")

  # Map internal model names to Dean's A/B/C/D labels
  model_map <- c(
    Non_informative = "A: weak-prior comparator \u2014 full data (reference)",
    Non_informative_Incomplete = "B: weak-prior comparator \u2014 partial data",
    Informative_Incomplete = "C: historical prior \u2014 partial data",
    Informative = "D: historical prior \u2014 full data"
  )

  # Helper: compute pct HPD CI overlap between two est data frames
  .pct_overlap <- function(est1, est2) {
    len_overlap <- rep(NA_real_, nrow(est1))
    for (i in seq_len(nrow(est1))) {
      ub <- c(est1$upper.HPD[i], est2$upper.HPD[i])
      lb <- c(est1$lower.HPD[i], est2$lower.HPD[i])
      wd <- ub - lb
      j  <- which.max(ub)
      len_overlap[i] <- ifelse(
        lb[j] > ub[-j],
        0,
        (ub[-j] - max(lb)) / max(wd)
      )
    }
    len_overlap
  }

  # Collect emmeans contrasts for all four models at each time window
  all_est <- mod$out |>
    purrr::imap_dfr(function(x_time, y_time) {
      cli::cli_inform("Time point: {y_time}")
      purrr::imap_dfr(model_map, function(label, internal_nm) {
        fit <- x_time[[internal_nm]]
        emmeans::emmeans(fit,
                         specs = stats::as.formula(em_form_string),
                         data  = fit$data)$contrasts |>
          as.data.frame() |>
          dplyr::mutate(
            model       = label,
            time_window = y_time
          )
      })
    }) |>
    tibble::as_tibble()

  # Derive time labels — ensure ut2 matches sorted ut1
  ut2_raw <- vapply(names(mod$out), function(nm) {
    d <- mod$out[[nm]][[1]]$data
    if (!is.null(d) && time2_col %in% names(d)) {
      unique(d[[time2_col]])[1]
    } else {
      NA_real_
    }
  }, numeric(1))
  names(ut2_raw) <- as.character(as.numeric(names(ut2_raw)))
  ut1 <- sort(as.numeric(names(ut2_raw)))
  ut2 <- as.numeric(ut2_raw[as.character(ut1)])

  time_labels <- paste0(ut1, "\u2013", ut2, " hr")

  all_est <- all_est |>
    dplyr::mutate(
      time_num   = as.numeric(.data[["time_window"]]),
      time_label = factor(
        paste0(.data[["time_num"]], "\u2013",
               ut2[match(.data[["time_num"]], ut1)], " hr"),
        levels = time_labels
      )
    )

  # Compute % HPD overlap vs Model A
  ref_est <- all_est |>
    dplyr::filter(.data[["model"]] ==
                    "A: weak-prior comparator \u2014 full data (reference)")

  add_overlap <- function(data_sub, ref) {
    purrr::map_dfr(unique(data_sub$time_label), function(tl) {
      sub  <- data_sub[data_sub$time_label == tl, ]
      rr   <- ref[ref$time_label == tl, ]
      if (nrow(sub) == 0 || nrow(rr) == 0) return(sub)
      ov <- .pct_overlap(rr[match(sub$contrast, rr$contrast), ], sub)
      sub$pct_overlap <- round(100 * ov, 1)
      sub
    })
  }

  plot1_data <- all_est |>
    dplyr::filter(.data[["model"]] %in%
                    c("A: weak-prior comparator \u2014 full data (reference)",
                      "B: weak-prior comparator \u2014 partial data",
                      "C: historical prior \u2014 partial data")) |>
    dplyr::mutate(model = factor(.data[["model"]],
                                 levels = names(model_colours)[1:3]))
  plot1_data <- add_overlap(plot1_data,
                             ref_est[ref_est$time_label %in% plot1_data$time_label, ])
  plot1_data$pct_overlap[
    plot1_data$model == "A: weak-prior comparator \u2014 full data (reference)"
  ] <- NA_real_
  plot1_data$comparison <- "A/B/C: partial data vs weak-prior full-data reference"

  plot2_data <- all_est |>
    dplyr::filter(.data[["model"]] %in%
                    c("A: weak-prior comparator \u2014 full data (reference)",
                      "D: historical prior \u2014 full data")) |>
    dplyr::mutate(model = factor(.data[["model"]],
                                 levels = c(
                                   "A: weak-prior comparator \u2014 full data (reference)",
                                   "D: historical prior \u2014 full data"
                                 )))
  plot2_data <- add_overlap(plot2_data,
                             ref_est[ref_est$time_label %in% plot2_data$time_label, ])
  plot2_data$pct_overlap[
    plot2_data$model == "A: weak-prior comparator \u2014 full data (reference)"
  ] <- NA_real_
  plot2_data$comparison <- "A/D: weak-prior comparator vs historical prior (full data)"

  both_data <- dplyr::bind_rows(plot1_data, plot2_data) |>
    dplyr::mutate(
      comparison  = factor(.data[["comparison"]],
                           levels = c(
                             "A/B/C: partial data vs weak-prior full-data reference",
                             "A/D: weak-prior comparator vs historical prior (full data)"
                           )),
      model       = factor(.data[["model"]], levels = names(model_colours)),
      pct_overlap_label = ifelse(
        is.na(.data[["pct_overlap"]]), "",
        as.character(.data[["pct_overlap"]])
      )
    )

  y_title <- if (nchar(endpoint) > 0 && nchar(endpoint_unit) > 0) {
    sprintf("Estimated \u0394%s vs control (%s)", endpoint, endpoint_unit)
  } else {
    "Estimated effect vs control"
  }
  plot_title <- if (nchar(endpoint) > 0) {
    sprintf("%s treatment-difference estimates (shared y-scale)", endpoint)
  } else {
    "Treatment-difference estimates (shared y-scale)"
  }
  caption_str <- sprintf(
    "Numbers above bars = %% HPD CI overlap with Model A | Dotted line = %g%s",
    threshold,
    if (nchar(endpoint_unit) > 0) paste0(" ", endpoint_unit) else ""
  )

  ggplot2::ggplot(
    both_data,
    ggplot2::aes(
      x      = .data[["contrast"]],
      y      = .data[["estimate"]],
      colour = .data[["model"]],
      group  = .data[["model"]]
    )
  ) +
    ggplot2::geom_hline(yintercept = 0,
                        linewidth  = 0.4) +
    ggplot2::geom_hline(yintercept = threshold,
                        linewidth  = 0.4,
                        linetype   = "dotted",
                        colour     = "grey40") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[["lower.HPD"]],
                   ymax = .data[["upper.HPD"]]),
      position  = ggplot2::position_dodge(width = 0.4),
      width     = 0.15,
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.4),
      size     = 2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y     = .data[["upper.HPD"]],
                   label = .data[["pct_overlap_label"]]),
      position     = ggplot2::position_dodge(width = 0.4),
      vjust        = -0.5,
      size         = 2.6,
      show.legend  = FALSE
    ) +
    ggplot2::scale_colour_manual(values = model_colours, name = NULL) +
    ggplot2::facet_grid(.data[["comparison"]] ~ .data[["time_label"]]) +
    ggplot2::labs(
      x       = "Treatment contrast",
      y       = y_title,
      title   = plot_title,
      caption = caption_str
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position  = "bottom",
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold"),
      strip.text.y     = ggplot2::element_text(angle = 0, hjust = 0),
      axis.text.x      = ggplot2::element_text(angle = 45,
                                                vjust = 1, hjust = 1)
    )
}

# make_time_x_plot ----
#' @title Plotting: DSIR-Style Time-on-X Plot
#' @description Internal helper that produces a time-window-on-x-axis error-bar
#'   plot suitable for embedding in Word reports via [generate_borrowing_report()].
#' @importFrom ggplot2 ggplot aes geom_hline geom_errorbar geom_point
#' @importFrom ggplot2 scale_shape_manual scale_colour_brewer scale_y_continuous
#' @importFrom ggplot2 labs theme_bw theme element_text position_dodge
#' @importFrom dplyr mutate
#' @param tbl data frame as produced inside [generate_borrowing_report()] with
#'   columns `time_window`, `treatment_code`, `mean_effect`, `lower_HPD`,
#'   `upper_HPD`.
#' @param model_label character label for the plot title (e.g. `"Model A"`).
#' @param cfg named list with elements `COMPOUND`, `PARAMETER`,
#'   `PARAMETER_UNIT`, and `effect_threshold`.
#' @param ylim numeric length-2 vector of y-axis limits, or `NULL` for
#'   automatic scaling.
#' @param ut1 numeric vector of time-window start values (for ordering).
#' @param ut2 numeric vector of time-window end values (for ordering).
#' @return A ggplot object.
#' @keywords internal
make_time_x_plot <- function(tbl         = NULL,
                             model_label = NULL,
                             cfg         = NULL,
                             ylim        = NULL,
                             ut1         = NULL,
                             ut2         = NULL) {
  if (is.null(cfg) ||
      !all(c("COMPOUND", "PARAMETER", "PARAMETER_UNIT",
             "effect_threshold") %in% names(cfg))) {
    stop("make_time_x_plot: cfg must be a named list with elements ",
         "COMPOUND, PARAMETER, PARAMETER_UNIT, and effect_threshold")
  }
  trt_levels <- sort(unique(tbl$treatment_code))
  n_trt      <- length(trt_levels)
  pch_vals   <- c(19, 17, 15, 18, 16, 8, 11, 12)[seq_len(n_trt)]

  tw_levels <- if (!is.null(ut1) && !is.null(ut2)) {
    paste0(sort(ut1), "\u2013", sort(ut2), " hr")
  } else {
    unique(tbl$time_window)
  }

  p <- tbl |>
    dplyr::mutate(
      treatment_code = factor(.data[["treatment_code"]], levels = trt_levels),
      time_window    = factor(.data[["time_window"]],    levels = tw_levels)
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x      = .data[["time_window"]],
        y      = .data[["mean_effect"]],
        colour = .data[["treatment_code"]],
        shape  = .data[["treatment_code"]],
        group  = .data[["treatment_code"]]
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::geom_hline(yintercept =  cfg$effect_threshold,
                        linewidth  = 0.4, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = -cfg$effect_threshold,
                        linewidth  = 0.4, linetype = "dotted") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[["lower_HPD"]],
                   ymax = .data[["upper_HPD"]]),
      position  = ggplot2::position_dodge(width = 0.35),
      width     = 0.15,
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.35),
      size     = 2.5
    ) +
    ggplot2::scale_shape_manual(values = pch_vals, name = "Treatment code") +
    ggplot2::scale_colour_brewer(palette = "Dark2",  name = "Treatment code") +
    ggplot2::labs(
      x       = "Time window",
      y       = sprintf("\u0394%s vs control (%s)",
                        cfg$PARAMETER, cfg$PARAMETER_UNIT),
      title   = sprintf("%s \u2014 %s", cfg$COMPOUND, model_label),
      caption = sprintf(
        "Dotted lines = \u00b1%g %s; error bars = 95%% HPD",
        cfg$effect_threshold, cfg$PARAMETER_UNIT
      )
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1)
    )

  if (!is.null(ylim)) {
    p <- p + ggplot2::scale_y_continuous(limits = ylim)
  }

  p
}

# method labels ----

#' Authoritative Analysis-Method Label Map
#' @description Single source of truth mapping internal analysis-method
#'   identifiers to the human-readable labels shown to users in sidebars,
#'   figure legends, result tables, and exports.
#' @param wrap logical. When `TRUE`, every label is wrapped between the
#'   prior/comparator description and the data-completeness description,
#'   which is the form used by figure legends and sidebar choices. Default
#'   (`NULL`/`FALSE`) returns single-line labels.
#' @return Named character vector. Names are internal method identifiers and
#'   values are display labels.
#' @keywords internal
#' @noRd
.method_label_map <- function(wrap = NULL) {
  
  if(FALSE){
    wrap <- TRUE
  }
  
  label_map <- c(
    "Non_informative"            = "Weak-prior comparator \u2014 full data",
    "Non_informative_Incomplete" = "Weak-prior comparator \u2014 partial data",
    "Informative_Incomplete"     = "Historical prior \u2014 partial data",
    "Informative"                = "Historical prior \u2014 full data"
  )
  
  if (isTRUE(wrap)) {
    label_map <- gsub(" \u2014 ", " \u2014\n", label_map, fixed = TRUE)
  }
  
  output <- label_map
  return(output)
}

#' Analysis-Method Choices
#' @description Returns the display label to internal identifier mapping used
#'   by method selectors and by the `input_method` argument of the plotting
#'   functions. Derived from `.method_label_map()` so the mapping is defined
#'   in exactly one place.
#' @param wrap logical. When `TRUE`, every label is wrapped between the
#'   prior/comparator description and the data-completeness description,
#'   which is the form used by figure legends and sidebar choices. Default
#'   (`NULL`/`FALSE`) returns single-line labels.
#' @return Named character vector. Names are display labels and values are
#'   internal method identifiers.
#' @keywords internal
#' @noRd
.method_choices <- function(wrap = NULL) {
  
  if(FALSE){
    wrap <- TRUE
  }
  
  label_map <- .method_label_map(wrap = wrap)
  
  output <- stats::setNames(names(label_map), unname(label_map))
  return(output)
}

#' Apply Display Labels To A Method Column
#' @description Recodes the internal method identifiers held in result data so
#'   user-visible tables and exports show the same labels as the figure
#'   legends. The internal identifier is retained in a `method_id` column.
#' @param dat data frame or tibble of result data.
#' @param method_col string naming the column holding method identifiers.
#'   Default is `"method"`.
#' @return The input data with `method` recoded to display labels and an
#'   additional `method_id` column holding the internal identifiers.
#' @keywords internal
#' @noRd
.apply_method_labels <- function(dat = NULL, method_col = NULL) {
  
  if(FALSE){
    dat        <- data.frame(method = "Informative", estimate = 1)
    method_col <- "method"
  }
  
  if (is.null(method_col)) {
    method_col <- "method"
  }
  
  if (is.null(dat) || !method_col %in% names(dat)) {
    output <- dat
    return(output)
  }
  
  label_map   <- .method_label_map()
  method_ids  <- as.character(dat[[method_col]])
  method_labs <- unname(label_map[method_ids])
  
  unmapped <- is.na(method_labs)
  method_labs[unmapped] <- method_ids[unmapped]
  
  output <- dat
  output[["method_id"]]  <- method_ids
  output[[method_col]]   <- method_labs
  
  col_order <- append(names(output)[names(output) != "method_id"],
                      "method_id",
                      after = match(method_col, names(output)))
  output <- output[, col_order, drop = FALSE]
  
  return(output)
}
