# app_server ----

#' App Server
#'
#' Main server function for the Safety Pharm Bayesian App. Resolves and
#' validates the prior bundle from `prior_file` (or the package-bundled
#' default when `prior_file` is `NULL`), then delegates reactive logic to
#' individual module servers.
#' @param input,output,session Standard Shiny server arguments.
#' @param prior_file `NULL` or a character string giving the path to a trusted
#'   local RDS file containing a compatible prior bundle. When `NULL`, the
#'   reviewed bundle shipped with the package is used. See `run_app()` for the
#'   prior-bundle schema.
#' @return Called for its side effect.
#' @export
app_server <- function(input, output, session, prior_file = NULL) {
  if (FALSE) {
    input <- NULL
    output <- NULL
    session <- NULL
    prior_file <- NULL
  }
  
  if (!is.null(grDevices::dev.list())) {
    grDevices::graphics.off()
  }
  
  # prior bundle loading ----
  
  prior_bundle <- resolve_prior_bundle(prior_file)
  list_calculated_priors <- shiny::reactive(prior_bundle)
  endpoint_descriptions <- .read_endpoint_descriptions()
  upload_guidance <- .read_upload_guidance()
  
  # module servers ----
  logo_news(input, session)
  
  contents_re <- mod_file_upload_server(
    "file_upload",
    upload_guidance = upload_guidance
  )
  
  mod_bundle_summary_server(
    "bundle_summary",
    list_calculated_priors = list_calculated_priors,
    prior_file = prior_file,
    endpoint_descriptions = endpoint_descriptions
  )
  
  mod_analysis_results_server(
    "analysis_results",
    list_calculated_priors = list_calculated_priors,
    contents_re = contents_re,
    endpoint_descriptions = endpoint_descriptions
  )
  
  mod_browse_priors_server(
    "browse_priors",
    list_calculated_priors = list_calculated_priors,
    endpoint_descriptions = endpoint_descriptions
  )
  
  output <- invisible(NULL)
  return(output)
}

# app_ui ----

#' App UI
#'
#' Assembles the complete Shiny UI for the Safety Pharm Bayesian
#' App from individual module UIs. Called by `run_app()`.
#' @return A `shiny::fluidPage` UI definition object.
#' @importFrom shiny fluidPage
#' @export
app_ui <- function() {
  ui_out <- shiny::fluidPage(
    shiny::titlePanel(shiny::h4(paste0(
      "PriorRhythm"))) ,
    
    theme = bslib::bs_theme(version = 5),
    
    shinyjs::useShinyjs(),
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(width = 3,
                          embed_spba_logo(),
                          mod_file_upload_sidebar_ui("file_upload"),
                          mod_analysis_results_sidebar_ui("analysis_results"),
                          mod_browse_priors_sidebar_ui("browse_priors")
      ),
      
      shiny::mainPanel(
        shiny::tabsetPanel(id = "tab",
                           mod_browse_priors_panel_ui("browse_priors"),
                           mod_bundle_summary_panel_ui("bundle_summary"),
                           mod_file_upload_panel_ui("file_upload"),
                           mod_analysis_results_setup_panel_ui("analysis_results"),
                           mod_analysis_results_panel_ui("analysis_results")
        )
      )
      
    )
  )
  return(ui_out)
}

# run_app ----

#' Run Safety Pharm Bayesian App
#'
#' Launches the Safety Pharmacology Bayesian Shiny application.
#' The app UI and server are defined as package functions (`app_ui()` and
#' `app_server()`) composed from modular Shiny components.
#'
#' The app is a **priors-only runtime**: it consumes a precomputed prior
#' bundle and does not build priors from raw historical data. By default the
#' reviewed bundle shipped with the package is used. Advanced users who have
#' built a compatible bundle with `chain_priors_from_data()` can supply it via
#' `prior_file`.
#'
#' **Prior-bundle contract** — the RDS must contain a named list organised as:
#' ```
#' list(
#'   <species> = list(
#'     <endpoint> = list(
#'       pa = <data.frame with columns alpha0, A0, beta0, phi0>
#'     )
#'   )
#' )
#' ```
#' Additional components are permitted. The bundle must not contain raw
#' historical observations or animal identifiers.
#'
#' @param prior_file `NULL` (default) or a character string giving the path to
#'   a trusted local RDS file containing a compatible prior bundle. When
#'   `NULL`, the reviewed bundle shipped with the package is used. When a path
#'   is supplied it must exist and pass structural validation; an invalid or
#'   missing file aborts startup with an actionable error — there is no silent
#'   fallback to the bundled priors.
#' @return Invisible `NULL`. Called for its side effect of launching the Shiny
#'   app.
#' @importFrom shiny runApp shinyApp
#' @export
#' @example R/examples/Example_run_app.R
run_app <- function(
    prior_file = NULL
) {
  if (FALSE) {
    prior_file <- NULL
    prior_file <- "/path/to/my_priors.RDS"
  }
  
  server_fn <- function(input, output, session) {
    app_server(input, output, session, prior_file = prior_file)
  }
  
  app_obj <- shiny::shinyApp(ui = app_ui(), server = server_fn)
  app_out <- shiny::runApp(app_obj)
  
  return(invisible(app_out))
}
