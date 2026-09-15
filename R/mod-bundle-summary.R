# mod_bundle_summary ----

#' Bundle Summary Module UI - Panel
#'
#' Read-only panel showing provenance information for the active prior bundle:
#' source (bundled vs custom), bundle ID, schema version, creation metadata,
#' and available species/endpoint combinations.
#'
#' @param id Character. Module namespace ID.
#' @return A `shiny::tabPanel` tag.
#' @keywords internal
#' @noRd
mod_bundle_summary_panel_ui <- function(id = NULL) {
  ns <- shiny::NS(id)
  ui_out <- shiny::tabPanel(
    title = "Prior Bundle Info",
    value = "Bundle_Summary",
    shiny::tags$div(
      style = "padding: 1em;",
      shiny::h4("Active Prior Bundle"),
      shiny::tableOutput(ns("summary_table")),
      shiny::br(),
      shiny::h5("Available Species / Endpoints"),
      shiny::tags$p(
        style = "font-style: italic; color: #555;",
        "Convergence status is recorded, non-confidential evidence from the",
        " controlled Stage 1 build/refresh workflow (chain-aware R-hat/ESS).",
        " It is not recalculated at runtime: the runtime bundle stores only",
        " pooled posterior draws and cannot validly reconstruct",
        " chain-aware diagnostics."
      ),
      shiny::tableOutput(ns("endpoints_table"))
    )
  )
  return(ui_out)
}


#' Bundle Summary Module Server
#'
#' Renders a read-only summary table for the active prior bundle, including
#' source label (bundled or custom path), schema version, bundle ID, creation
#' details, and available species/endpoint combinations together with each
#' endpoint's recorded convergence status.
#'
#' The convergence status column shows the `diagnostics$status` value
#' recorded in the bundle at build/refresh time (see
#' `summarize_convergence_diagnostics()` and `make_runtime_prior_bundle()`
#' in the controlled prior-refresh workflow) — it is controlled-build
#' evidence, never a value recalculated live from the runtime bundle's
#' pooled posterior draws.
#'
#' @param id Character. Module namespace ID.
#' @param list_calculated_priors A reactive returning the normalised prior
#'   bundle (output of `resolve_prior_bundle()`), containing `schema_version`,
#'   `metadata`, and `priors`.
#' @param prior_file `NULL` or a character path. Passed through from
#'   `app_server()` to label the bundle source in the UI.
#' @param endpoint_descriptions Optional named list of reviewed display metadata
#'   parsed once in `app_server()` from `inst/config/endpoint_descriptions.yaml`.
#' @return Called for its side effect.
#' @importFrom shiny moduleServer reactive renderTable req
#' @keywords internal
#' @noRd
mod_bundle_summary_server <- function(id = NULL,
                                      list_calculated_priors = NULL,
                                      prior_file = NULL,
                                      endpoint_descriptions = NULL) {

  if (FALSE) {
    id         <- "bundle_summary"
    prior_file <- NULL
    endpoint_descriptions <- .read_endpoint_descriptions()
    list_calculated_priors <- shiny::reactive(list(
      schema_version = 1L,
      metadata = list(bundle_id = "test", package_version = "0.0.0.1"),
      priors = list(dog = list(q_tc = list(
        pa = data.frame(alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1),
        diagnostics = list(status = "pass")
      )))
    ))
  }

  output <- shiny::moduleServer(id, function(input, output, session) {

    bundle <- shiny::reactive({
      shiny::req(!is.null(list_calculated_priors()))
      list_calculated_priors()
    })

    output$summary_table <- shiny::renderTable({

      b   <- bundle()
      src <- if (is.null(prior_file)) "Bundled (package default)" else prior_file
      meta <- if (is.list(b[["metadata"]])) b[["metadata"]] else list()

      rows <- list(
        c("Source",           as.character(src)),
        c("Schema version",   if (is.null(b[["schema_version"]])) "-"
                              else as.character(b[["schema_version"]])),
        c("Bundle ID",        .meta_field(meta, "bundle_id")),
        c("Created with",     .meta_field(meta, "created_with")),
        c("Created at",       .meta_field(meta, "created_at")),
        c("Package version",  .meta_field(meta, "package_version"))
      )

      df_out <- as.data.frame(
        do.call(rbind, rows),
        stringsAsFactors = FALSE
      )
      names(df_out) <- c("Field", "Value")
      return(df_out)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")


    output$endpoints_table <- shiny::renderTable({

      b     <- bundle()
      ptree <- if (is.list(b[["priors"]])) b[["priors"]] else list()

      if (length(ptree) == 0L) {
        output <- data.frame(
          Species = character(0),
          "Species label" = character(0),
          Endpoint = character(0),
          "Endpoint label" = character(0),
          Unit = character(0),
          "Convergence status" = character(0),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        return(output)
      }

      rows <- lapply(names(ptree), function(sp) {
        eps <- names(ptree[[sp]])
        species_display <- .display_metadata(
          endpoint_descriptions = endpoint_descriptions,
          species = sp
        )

        if (length(eps) == 0L) {
          output <- data.frame(
            Species = sp,
            "Species label" = species_display[["species_label"]],
            Endpoint = "-",
            "Endpoint label" = "-",
            Unit = "-",
            "Convergence status" = "-",
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          return(output)
        }

        endpoint_rows <- lapply(eps, function(ep) {
          display <- .display_metadata(
            endpoint_descriptions = endpoint_descriptions,
            species = sp,
            endpoint = ep
          )
          unit_label <- if (is.null(display[["endpoint_unit"]])) "-" else display[["endpoint_unit"]]
          status <- .diagnostics_status_label(ptree[[sp]][[ep]][["diagnostics"]])

          output <- data.frame(
            Species = sp,
            "Species label" = display[["species_label"]],
            Endpoint = ep,
            "Endpoint label" = display[["endpoint_label"]],
            Unit = unit_label,
            "Convergence status" = status,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )

          return(output)
        })

        output <- do.call(rbind, endpoint_rows)
        return(output)
      })

      df_out <- do.call(rbind, rows)
      return(df_out)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  })

  return(output)
}


#' Format a Recorded Diagnostics Status for Display
#'
#' Returns a concise, human-readable label for an endpoint's recorded
#' `diagnostics$status` value. Never recalculates diagnostics — only
#' formats what was recorded during the controlled build/refresh workflow.
#'
#' @param diagnostics The endpoint's `diagnostics` list (or `NULL`/absent).
#' @return Scalar character label.
#' @keywords internal
#' @noRd
.diagnostics_status_label <- function(diagnostics = NULL) {

  if (FALSE) {
    diagnostics <- list(status = "pass")
  }

  if (is.null(diagnostics) || is.null(diagnostics[["status"]])) {
    return("Not recorded")
  }

  status <- diagnostics[["status"]]

  label <- switch(
    status,
    pass       = "Converged (recorded)",
    acceptable = "Acceptable (recorded)",
    failed     = "Not converged (recorded)",
    unassessed = "Unassessed (recorded)",
    paste0(status, " (recorded)")
  )

  return(label)
}


#' Extract a Metadata Field as a Display String
#'
#' Returns the value of `key` from the `meta` list coerced to a single
#' printable character string, or `"-"` when the key is absent/NULL.
#'
#' @param meta Named list (may be `NULL`).
#' @param key Scalar character key name.
#' @return Scalar character string.
#' @keywords internal
#' @noRd
.meta_field <- function(meta = NULL, key = NULL) {

  if (FALSE) {
    meta <- list(bundle_id = "test-bundle")
    key  <- "bundle_id"
  }

  val <- meta[[key]]
  if (is.null(val) || (length(val) == 1L && is.na(val))) {
    return("-")
  }
  paste(as.character(val), collapse = ", ")
}
