# get_package_logo_path ----
#' @title Get Package Logo Path
#' @description Returns the file path to the package logo PNG, useful for
#' embedding in Shiny apps or reports. The logo is installed at
#' `inst/logo/logo.png`.
#' @param package character string, name of the package. Default is
#' `"PriorRhythm"`.
#' @return A character string with the full path to the package logo PNG file,
#' or an empty string if the logo file is not found.
#' @export
#' @example R/examples/Example_get_package_logo_path.R
get_package_logo_path <- function(
    package = "PriorRhythm"
) {
  logo_path <- system.file("logo", "logo.png", package = package)
  return(logo_path)
}

# embed_spba_logo ----

#' @title Embed the Safety Pharm Bayesian App logo in a Shiny UI
#' @description Creates a Shiny UI element containing the package logo and a
#'   clickable version number that opens the changelog modal. Use in combination
#'   with [logo_news()] in the server function.
#' @param logo_dir Character. Path to the logo file. Defaults to
#'   [get_package_logo_path()].
#' @param align Character. Horizontal alignment of the logo. One of
#'   "center", "left", "right". Default is "center".
#' @param height Character. Height of the logo image as a CSS string (e.g.
#'   "100px"). Default is "100px".
#' @param style Character. CSS style applied to the wrapping `div`. Default is
#'   "text-align: center;margin-bottom:10px;".
#' @return A `shiny::tags$div` UI element containing the logo image and a
#'   versioned action link.
#' @seealso [logo_news()]
#' @importFrom shiny tags img actionLink h5
#' @importFrom glue glue
#' @importFrom utils packageVersion packageName
#' @export
#' @example R/examples/Example_embed_spba_logo.R
embed_spba_logo <- function(
    logo_dir = get_package_logo_path(),
    align    = "center",
    height   = "100px",
    style    = "text-align: center;margin-bottom:10px;") {
  
  pkg_name   <- utils::packageName(environment(embed_spba_logo))
  final_name <- basename(logo_dir)
  shiny::addResourcePath(final_name, logo_dir)
  
  em_logo <- shiny::tags$div(
    shiny::img(
      src    = final_name,
      align  = align,
      height = height
    ),
    shiny::actionLink(
      inputId = "show_news",
      label   = shiny::h5(
        glue::glue("v {utils::packageVersion(pkg_name)} news")
      )
    ),
    style = style
  )
  return(em_logo)
}


# logo_news ----

#' @title Server-side handler for the changelog modal
#' @description Registers a [shiny::observeEvent()] that listens for clicks on
#'   the version link created by [embed_spba_logo()] and displays the package
#'   `NEWS` file in a modal dialog.
#' @param input The Shiny `input` object from the server function.
#' @param session The Shiny `session` object. Defaults to the current reactive
#'   domain via [shiny::getDefaultReactiveDomain()].
#' @return None. Called for its side effects.
#' @seealso [embed_spba_logo()]
#' @importFrom shiny observeEvent showModal modalDialog modalButton tags p
#' @importFrom glue glue
#' @importFrom utils packageVersion packageName
#' @export
logo_news <- function(
    input,
    session = shiny::getDefaultReactiveDomain()) {
  
  pkg_name <- utils::packageName(environment(logo_news))
  
  shiny::observeEvent(input$show_news, {
    
    news_path <- system.file("NEWS.md", package = pkg_name)
    
    news_content <- if (!nzchar(news_path)) {
      shiny::p(
        "No changelog found. Reinstall PriorRhythm so the packaged NEWS.md file is available."
      )
    } else {
      shiny::includeMarkdown(news_path)
    }
    
    shiny::showModal(shiny::modalDialog(
      title     = glue::glue("v{utils::packageVersion(pkg_name)} - Changelog"),
      news_content,
      easyClose = TRUE,
      size      = "l",
      footer    = shiny::modalButton("Close")
    ))
  })
}