#' Read Packaged Display YAML Safely
#'
#' Reads a reviewed YAML file shipped under `inst/config/` from the installed
#' package. Missing or malformed files fall back to an empty list so app display
#' metadata never blocks valid prior usage.
#'
#' @param config_file Character. Scalar packaged config file name under
#'   `inst/config/`.
#' @return A named list parsed from YAML, or an empty list on missing/malformed
#'   content.
#' @keywords internal
#' @noRd
.read_packaged_display_yaml <- function(config_file = NULL) {
  if (FALSE) {
    config_file <- "endpoint_descriptions.yaml"
  }
  
  if (!is.character(config_file) || length(config_file) != 1L ||
      is.na(config_file) || !nzchar(config_file)) {
    cli::cli_abort("{.arg config_file} must be a single non-empty character string.")
  }
  
  config_path <- system.file(
    "config",
    config_file,
    package = "PriorRhythm",
    mustWork = FALSE
  )
  
  if (!nzchar(config_path) || !file.exists(config_path)) {
    output <- list()
    return(output)
  }
  
  parsed <- tryCatch(
    yaml::read_yaml(config_path),
    error = function(e) {
      NULL
    }
  )
  
  if (!is.list(parsed)) {
    output <- list()
    return(output)
  }
  
  output <- parsed
  return(output)
}


#' Read Packaged Endpoint Display Metadata
#'
#' @return A named list parsed from `inst/config/endpoint_descriptions.yaml`, or
#'   an empty list when the installed file is missing or malformed.
#' @keywords internal
#' @noRd
.read_endpoint_descriptions <- function() {
  if (FALSE) {
  }
  
  output <- .read_packaged_display_yaml("endpoint_descriptions.yaml")
  return(output)
}


#' Read Packaged Upload Guidance
#'
#' @return A named list parsed from `inst/config/upload_guidance.yaml`, or an
#'   empty list when the installed file is missing or malformed.
#' @keywords internal
#' @noRd
.read_upload_guidance <- function() {
  if (FALSE) {
  }
  
  output <- .read_packaged_display_yaml("upload_guidance.yaml")
  return(output)
}


#' Normalize Guidance Text
#'
#' @param value Any R object.
#' @return Scalar character string, or `NULL` when no usable text is available.
#' @keywords internal
#' @noRd
.guidance_text <- function(value = NULL) {
  if (FALSE) {
    value <- "Upload CSV or TSV files only."
  }
  
  text_out <- .normalise_annotation_text(as.character(value)[1])
  
  if (!nzchar(text_out)) {
    output <- NULL
    return(output)
  }
  
  output <- text_out
  return(output)
}


#' Normalize Guidance Bullet Items
#'
#' @param value Any R object.
#' @return Character vector of non-empty bullet items.
#' @keywords internal
#' @noRd
.guidance_items <- function(value = NULL) {
  if (FALSE) {
    value <- list("Upload one file.", "Select a compatible prior.")
  }
  
  if (is.null(value)) {
    output <- character(0)
    return(output)
  }
  
  items <- as.character(unlist(value, recursive = TRUE, use.names = FALSE))
  items <- items[!is.na(items)]
  items <- trimws(items)
  items <- items[nzchar(items)]
  
  output <- items
  return(output)
}


#' Build Display Metadata for a Species/Endpoint Selection
#'
#' Returns friendly reviewed labels/units when available, while preserving raw
#' bundle keys as the fallback display for missing or malformed metadata.
#'
#' @param endpoint_descriptions Named list parsed from
#'   `endpoint_descriptions.yaml`, or `NULL`.
#' @param species Raw species key.
#' @param endpoint Raw endpoint key.
#' @return A named list containing raw keys together with friendly fallback-safe
#'   display fields.
#' @keywords internal
#' @noRd
.display_metadata <- function(
    endpoint_descriptions = NULL,
    species = NULL,
    endpoint = NULL
) {
  if (FALSE) {
    endpoint_descriptions <- list(
      dog = list(
        label = "Dog",
        endpoints = list(
          q_tc = list(label = "Corrected QT Interval (QTc)", unit = "ms")
        )
      )
    )
    species <- "dog"
    endpoint <- "q_tc"
  }
  
  species_key <- .normalise_annotation_text(species)
  endpoint_key <- .normalise_annotation_text(endpoint)
  
  species_label <- species_key
  endpoint_label <- endpoint_key
  endpoint_unit <- NULL
  
  if (is.list(endpoint_descriptions) && nzchar(species_key)) {
    species_entry <- endpoint_descriptions[[species_key]]
    
    if (is.list(species_entry)) {
      candidate_species_label <- .normalise_annotation_text(species_entry[["label"]])
      if (nzchar(candidate_species_label)) {
        species_label <- candidate_species_label
      }
      
      endpoint_entries <- species_entry[["endpoints"]]
      if (is.list(endpoint_entries) && nzchar(endpoint_key)) {
        endpoint_entry <- endpoint_entries[[endpoint_key]]
        
        if (is.list(endpoint_entry)) {
          candidate_endpoint_label <- .normalise_annotation_text(endpoint_entry[["label"]])
          candidate_endpoint_unit <- .normalise_annotation_text(endpoint_entry[["unit"]])
          
          if (nzchar(candidate_endpoint_label)) {
            endpoint_label <- candidate_endpoint_label
          }
          
          if (nzchar(candidate_endpoint_unit)) {
            endpoint_unit <- candidate_endpoint_unit
          }
        }
      }
    }
  }
  
  endpoint_display <- endpoint_label
  if (!is.null(endpoint_unit)) {
    endpoint_display <- as.character(
      glue::glue("{endpoint_label} [{endpoint_unit}]")
    )
  }
  
  output <- list(
    species = species_key,
    endpoint = endpoint_key,
    species_label = species_label,
    endpoint_label = endpoint_label,
    endpoint_unit = endpoint_unit,
    endpoint_display = endpoint_display
  )
  
  return(output)
}


#' Build Friendly Species Choices with Raw Values
#'
#' @param species_values Character vector of raw species keys.
#' @param endpoint_descriptions Named list of reviewed endpoint metadata.
#' @return Named character vector suitable for Shiny `choices`, with friendly
#'   labels as names and raw keys as values.
#' @keywords internal
#' @noRd
.display_species_choices <- function(
    species_values = NULL,
    endpoint_descriptions = NULL
) {
  if (FALSE) {
    species_values <- c("dog", "nhp")
    endpoint_descriptions <- .read_endpoint_descriptions()
  }
  
  if (is.null(species_values)) {
    output <- character(0)
    return(output)
  }
  
  species_values <- as.character(species_values)
  labels <- vapply(
    species_values,
    function(species_value) {
      display <- .display_metadata(
        endpoint_descriptions = endpoint_descriptions,
        species = species_value
      )
      
      output <- display[["species_label"]]
      return(output)
    },
    character(1)
  )
  
  output <- stats::setNames(species_values, labels)
  return(output)
}


#' Build Friendly Endpoint Choices with Raw Values
#'
#' @param species Raw species key used to locate endpoint display metadata.
#' @param endpoint_values Character vector of raw endpoint keys.
#' @param endpoint_descriptions Named list of reviewed endpoint metadata.
#' @return Named character vector suitable for Shiny `choices`, with friendly
#'   labels as names and raw keys as values.
#' @keywords internal
#' @noRd
.display_endpoint_choices <- function(
    species = NULL,
    endpoint_values = NULL,
    endpoint_descriptions = NULL
) {
  if (FALSE) {
    species <- "dog"
    endpoint_values <- c("q_tc", "hr")
    endpoint_descriptions <- .read_endpoint_descriptions()
  }
  
  if (is.null(endpoint_values)) {
    output <- character(0)
    return(output)
  }
  
  endpoint_values <- as.character(endpoint_values)
  labels <- vapply(
    endpoint_values,
    function(endpoint_value) {
      display <- .display_metadata(
        endpoint_descriptions = endpoint_descriptions,
        species = species,
        endpoint = endpoint_value
      )
      
      output <- display[["endpoint_display"]]
      return(output)
    },
    character(1)
  )
  
  output <- stats::setNames(endpoint_values, labels)
  return(output)
}


#' Render Upload Guidance UI
#'
#' @param upload_guidance Named list parsed from `upload_guidance.yaml`, or
#'   `NULL`.
#' @return A `shiny::tagList` containing publication-facing file upload
#'   guidance.
#' @keywords internal
#' @noRd
.upload_guidance_ui <- function(upload_guidance = NULL) {
  if (FALSE) {
    upload_guidance <- list(
      title = "Upload target-study data",
      accepted_files = "Upload CSV or TSV files only.",
      analysis_roles = list("Animal code identifies each animal."),
      workflow = list("Upload one file.", "Choose a compatible prior."),
      time_intervals = "The time column should contain interval text such as 0 to 3.",
      compatibility = "Users must confirm species and unit compatibility."
    )
  }
  
  title_text <- .guidance_text(upload_guidance[["title"]])
  accepted_files_text <- .guidance_text(upload_guidance[["accepted_files"]])
  analysis_role_items <- .guidance_items(upload_guidance[["analysis_roles"]])
  workflow_items <- .guidance_items(upload_guidance[["workflow"]])
  time_intervals_text <- .guidance_text(upload_guidance[["time_intervals"]])
  compatibility_text <- .guidance_text(upload_guidance[["compatibility"]])
  
  if (is.null(title_text)) {
    title_text <- "Upload target-study data"
  }
  
  if (is.null(accepted_files_text)) {
    accepted_files_text <- "Upload CSV or TSV files only."
  }
  
  if (is.null(time_intervals_text)) {
    time_intervals_text <- paste(
      "The selected time column should contain interval text such as",
      "\"0 to 3\" or \"0 to 3 hr\". The current default delimiter is",
      "\"to\" and can be changed in the sidebar when needed."
    )
  }
  
  if (is.null(compatibility_text)) {
    compatibility_text <- paste(
      "Users are responsible for confirming that the selected prior's species,",
      "endpoint definition, and measurement unit are compatible with the",
      "uploaded target-study data."
    )
  }
  
  sections <- list(
    shiny::h5(title_text),
    shiny::tags$p(accepted_files_text)
  )
  
  if (length(analysis_role_items) > 0L) {
    sections <- c(
      sections,
      list(
        shiny::tags$p("Expected analysis roles for the uploaded target-study file:"),
        shiny::tags$ul(lapply(analysis_role_items, shiny::tags$li))
      )
    )
  }
  
  if (length(workflow_items) > 0L) {
    sections <- c(
      sections,
      list(
        shiny::tags$p("Typical target-study workflow:"),
        shiny::tags$ul(lapply(workflow_items, shiny::tags$li))
      )
    )
  }
  
  sections <- c(
    sections,
    list(
      shiny::tags$p(time_intervals_text),
      shiny::tags$p(compatibility_text)
    )
  )
  
  output <- do.call(shiny::tagList, sections)
  return(output)
}
