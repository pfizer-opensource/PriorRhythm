# Development launcher ----
# This file is a thin launcher intended for interactive development use.
# For normal use, call PriorRhythm::run_app() from the package.
#
# By default the reviewed prior bundle shipped with the package is used.
# To use a custom trusted local prior bundle, set prior_file before sourcing:
#   prior_file <- "/path/to/my_priors.RDS"
# If prior_file is NULL or missing, the bundled reviewed priors are loaded.

if (!exists("prior_file")) {
  prior_file <- NULL
}

server_fn <- function(input, output, session) {
  PriorRhythm::app_server(
    input, output, session,
    prior_file = prior_file
  )
}

shiny::shinyApp(
  ui = PriorRhythm::app_ui(),
  server = server_fn
)
