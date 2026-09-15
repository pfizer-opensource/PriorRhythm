library(testthat)
library(PriorRhythm)

# mod_analysis_results_sidebar_ui: plot-customization controls --------------

test_that("results sidebar exposes title/legend font size and aspect ratio inputs", {
  ui <- mod_analysis_results_sidebar_ui(id = "test")
  html <- as.character(ui)

  expect_match(html, 'id="test-title_font_size"')
  expect_match(html, 'id="test-legend_font_size"')
  expect_match(html, 'id="test-legend_title_font_size"')
  expect_match(html, 'id="test-x_axis_font_size"')
  expect_match(html, 'id="test-y_axis_font_size"')
  expect_match(html, 'id="test-x_axis_tick_font_size"')
  expect_match(html, 'id="test-y_axis_tick_font_size"')
  expect_match(html, 'id="test-facet_font_size"')
  expect_match(html, 'id="test-aspect_ratio"')
})

test_that("plot-customization inputs default to the prior presentation values", {
  ui <- mod_analysis_results_sidebar_ui(id = "test")
  html <- as.character(ui)

  title_input       <- sub('.*id="test-title_font_size"([^>]*)>.*', "\\1", html)
  legend_input      <- sub('.*id="test-legend_font_size"([^>]*)>.*', "\\1", html)
  legend_title_input <- sub('.*id="test-legend_title_font_size"([^>]*)>.*', "\\1", html)
  x_axis_input      <- sub('.*id="test-x_axis_font_size"([^>]*)>.*', "\\1", html)
  y_axis_input      <- sub('.*id="test-y_axis_font_size"([^>]*)>.*', "\\1", html)
  x_axis_tick_input <- sub('.*id="test-x_axis_tick_font_size"([^>]*)>.*', "\\1", html)
  y_axis_tick_input <- sub('.*id="test-y_axis_tick_font_size"([^>]*)>.*', "\\1", html)
  facet_input       <- sub('.*id="test-facet_font_size"([^>]*)>.*', "\\1", html)
  ratio_input       <- sub('.*id="test-aspect_ratio"([^>]*)>.*', "\\1", html)

  expect_match(title_input, 'value="16"')
  expect_match(legend_input, 'value="16"')
  expect_match(legend_title_input, 'value="16"')
  expect_match(x_axis_input, 'value="16"')
  expect_match(y_axis_input, 'value="16"')
  expect_match(x_axis_tick_input, 'value="16"')
  expect_match(y_axis_tick_input, 'value="16"')
  expect_match(facet_input, 'value="16"')
  expect_match(ratio_input, 'value="0.5"')
})

# Integration: theme customization applied the same way mod_analysis_results
# builds its CI/probability plots (slow — skipped on CRAN) -----------------

test_that("customized title/legend size and aspect ratio are applied to result plots", {
  skip_on_cran()
  skip_if_not_installed("MCMCglmm")
  skip_if_not_installed("emmeans")

  dat <- make_test_dat(n_animals = 4, treatments = 1:2,
                       periods = 1:2,
                       time_bins = c("0 to 3 hr", "3 to 6 hr"))

  set.seed(42)
  n_samp <- 100
  pa <- data.frame(
    alpha0 = rnorm(n_samp, 40, 5),
    A0 = rnorm(n_samp, 0, 1),
    beta0 = rnorm(n_samp, 0, 1),
    phi0 = rnorm(n_samp, 0, 0.5)
  )

  mod <- bayesian_test_example(
    input_tdat = dat,
    animal_col = "animal_code",
    treatment_col = "treatment_code",
    period_col = "period_code",
    time1_col = "time1",
    time2_col = "time2",
    y_col = "value",
    n_exclude = 1,
    nitt = 1500,
    thin = 1,
    burnin = 500,
    pa = pa
  )

  title_size  <- 22
  legend_size <- 10
  legend_title_size <- 20
  x_axis_size <- 12
  y_axis_size <- 14
  x_axis_tick_size <- 8
  y_axis_tick_size <- 9
  facet_size  <- 18
  aspect      <- 0.75

  ci_plot <- ci_plots(mod = mod, threshold = 0) +
    ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size, hjust = 0.5),
      legend.text = ggplot2::element_text(size = legend_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      axis.title.x = ggplot2::element_text(size = x_axis_size),
      axis.title.y = ggplot2::element_text(size = y_axis_size),
      axis.text.x = ggplot2::element_text(size = x_axis_tick_size),
      axis.text.y = ggplot2::element_text(size = y_axis_tick_size),
      strip.text = ggplot2::element_text(size = facet_size),
      legend.position = "bottom",
      aspect.ratio = aspect)

  prob_plot <- prob_statement_plots(mod = mod, threshold = 0) +
    ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size, hjust = 0.5),
      legend.text = ggplot2::element_text(size = legend_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      axis.title.x = ggplot2::element_text(size = x_axis_size),
      axis.title.y = ggplot2::element_text(size = y_axis_size),
      axis.text.x = ggplot2::element_text(size = x_axis_tick_size),
      axis.text.y = ggplot2::element_text(size = y_axis_tick_size),
      strip.text = ggplot2::element_text(size = facet_size),
      aspect.ratio = aspect)

  expect_equal(ci_plot$theme$plot.title$size, title_size)
  expect_equal(ci_plot$theme$legend.text$size, legend_size)
  expect_equal(ci_plot$theme$legend.title$size, legend_title_size)
  expect_equal(ci_plot$theme$axis.title.x$size, x_axis_size)
  expect_equal(ci_plot$theme$axis.title.y$size, y_axis_size)
  expect_equal(ci_plot$theme$axis.text.x$size, x_axis_tick_size)
  expect_equal(ci_plot$theme$axis.text.y$size, y_axis_tick_size)
  expect_equal(ci_plot$theme$strip.text$size, facet_size)
  expect_equal(ci_plot$theme$aspect.ratio, aspect)
  expect_equal(ci_plot$theme$legend.position, "bottom")

  expect_equal(prob_plot$theme$plot.title$size, title_size)
  expect_equal(prob_plot$theme$legend.text$size, legend_size)
  expect_equal(prob_plot$theme$legend.title$size, legend_title_size)
  expect_equal(prob_plot$theme$axis.title.x$size, x_axis_size)
  expect_equal(prob_plot$theme$axis.title.y$size, y_axis_size)
  expect_equal(prob_plot$theme$axis.text.x$size, x_axis_tick_size)
  expect_equal(prob_plot$theme$axis.text.y$size, y_axis_tick_size)
  expect_equal(prob_plot$theme$strip.text$size, facet_size)
  expect_equal(prob_plot$theme$aspect.ratio, aspect)
})
