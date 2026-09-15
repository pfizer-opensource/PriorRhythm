# mod_file_upload ----

#' @title File Upload Module UI — Sidebar
#' @description Sidebar inputs for the file upload tab.
#' @param id Character. Module namespace ID.
#' @return A `shiny::conditionalPanel` tag.
#' @keywords internal
#' @noRd
mod_file_upload_sidebar_ui <- function(id = NULL) {
  if (FALSE) {
    id <- "file_upload"
  }

  ns <- shiny::NS(id)
  ui_out <- shiny::conditionalPanel(
    condition = 'input.tab === "File_upload"',

    shiny::tags$details(
      open = "open",
      shiny::tags$summary(shiny::strong("Upload study data")),
      shiny::fileInput(ns("file1"), "Upload Study Data File",
        multiple = FALSE,
        accept = c("text/csv",
          "text/tab-separated-values",
          "text/comma-separated-values",
          ".tsv",
          ".csv")),
      shiny::tags$p(
        style = "margin-bottom: 0;",
        shiny::strong("Accepted files:"),
        " CSV and TSV only."
      )
    ),

    shiny::tags$details(
      open = "open",
      shiny::tags$summary(shiny::strong("Format data")),
      shiny::textInput(ns("anrstr"), "Set generic animal name",
        value = "monkey|dog"),
      shiny::textInput(ns("time_col"), "time column",
        value = "time"),
      shiny::textInput(ns("timesep"), "time delimiter",
        value = "to")
    )
  )
  return(ui_out)
}

#' @title File Upload Module UI — Panel
#' @description Main panel tab for the file upload feature.
#' @param id Character. Module namespace ID.
#' @return A `shiny::tabPanel` tag.
#' @keywords internal
#' @noRd
mod_file_upload_panel_ui <- function(id = NULL) {
  if (FALSE) {
    id <- "file_upload"
  }

  ns <- shiny::NS(id)
  ui_out <- shiny::tabPanel(
    title = "File upload",
    value = "File_upload",
    shiny::tags$div(
      style = "padding-bottom: 1em;",
      shiny::tags$details(
        shiny::tags$summary(shiny::strong("File requirements and workflow.")),
        shiny::uiOutput(ns("upload_guidance"))
      )
    ),
    DT::dataTableOutput(ns("contents"))
  )
  return(ui_out)
}

#' @title File Upload Module Server
#' @description Server logic for the file upload tab. Processes the uploaded
#'   file using `load_study_data()` and `.parse_time_intervals()`.
#' @param id Character. Module namespace ID.
#' @param config Optional `PriorRhythmConfig` from [study_config()] or
#'   [read_study_config()]. When supplied, its `time_parser` settings are
#'   used as defaults for parsing the time-interval column. UI inputs
#'   (delimiter text field) still override config values.
#' @param upload_guidance Optional named list of reviewed upload guidance parsed
#'   once in `app_server()` from `inst/config/upload_guidance.yaml`.
#' @return A reactive list with elements `df`, `col_codes`, `time_vars`,
#'   and `numeric_ids`.
#' @keywords internal
#' @noRd
mod_file_upload_server <- function(
    id = NULL,
    config = NULL,
    upload_guidance = NULL
) {
  if (FALSE) {
    id <- "file_upload"
    config <- NULL
    upload_guidance <- .read_upload_guidance()
  }

  output <- shiny::moduleServer(id, function(input, output, session) {

    output$upload_guidance <- shiny::renderUI({
      guidance_ui <- .upload_guidance_ui(upload_guidance = upload_guidance)
      return(guidance_ui)
    })

    contents_re <- shiny::eventReactive(input$file1, {
      df <- load_study_data(
        data_path = input$file1$datapath,
        animal_string = input$anrstr,
        config = config)

      # Parse time intervals into derived numeric time1 / time2.
      # The UI delimiter field overrides config; config overrides package defaults.
      time_col_name  <- input$time_col
      ui_delimiter   <- if (!is.null(input$timesep) && nzchar(input$timesep)) {
        input$timesep
      } else {
        NULL
      }
      if (!is.null(time_col_name) && nzchar(time_col_name) &&
          time_col_name %in% names(df)) {
        df <- .parse_time_intervals(
          df        = df,
          time_col  = time_col_name,
          delimiter = ui_delimiter,
          config    = config
        )
      }

      # Auto-derive period_code from period when the column is absent
      df <- .derive_period_code(df)

      col_codes <- df |>
        dplyr::select(dplyr::matches("_code$")) |>
        names()

      time_vars <- c(input$time_col, "time1", "time2")

      numeric_ids <- df |>
        dplyr::select(-dplyr::all_of(c(col_codes, time_vars))) |>
        dplyr::select(dplyr::where(is.numeric)) |>
        names()

      list(
        df = df,
        col_codes = col_codes,
        time_vars = time_vars,
        numeric_ids = numeric_ids
      )
    })

    output$contents <- DT::renderDataTable({
      shiny::req(input$file1)
      DT::datatable(contents_re()$df,
        rownames = FALSE,
        options = list(
          dom = "Bt",
          scrollY = 500,
          scrollX = 300,
          scrollCollapse = TRUE,
          pageLength = -1
        ))
    })

    return(contents_re)
  })

  return(output)
}
