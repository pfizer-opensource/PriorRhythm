# mod_browse_priors ----

#' @title Browse Priors Module UI — Sidebar
#' @description Sidebar inputs for the browse priors tab.
#' @param id Character. Module namespace ID.
#' @return A `shiny::conditionalPanel` tag.
#' @keywords internal
#' @noRd
mod_browse_priors_sidebar_ui <- function(id = NULL) {
  if (FALSE) {
    id <- "browse_priors"
  }

  ns <- shiny::NS(id)
  ui_out <- shiny::conditionalPanel(
    condition = 'input.tab === "Browsing_Priors_Plot"',
    
    shiny::selectInput(ns("Species_Browse"), "Species",
                       choices = "",
                       selected = ""),
    
    shiny::selectInput(ns("Prior_Browse"), "Prior",
                       choices = "")
  )
  return(ui_out)
}

#' @title Browse Priors Module UI — Panel
#' @description Main panel tab for browsing pre-calculated priors.
#' @param id Character. Module namespace ID.
#' @return A `shiny::tabPanel` tag.
#' @keywords internal
#' @noRd
mod_browse_priors_panel_ui <- function(id = NULL) {
  ns <- shiny::NS(id)
  ui_out <- shiny::tabPanel(
    title = "Browse Priors",
    value = "Browsing_Priors_Plot",
    
    shiny::plotOutput(ns("Browsing_Priors_Plot"),
                      width = "100%", height = "500px")
  )
  return(ui_out)
}

#' @title Browse Priors Module Server
#' @description Server logic for browsing and plotting pre-calculated priors.
#' @param id Character. Module namespace ID.
#' @param list_calculated_priors A reactive returning the normalised prior
#'   bundle (output of `resolve_prior_bundle()`), containing a `priors` list
#'   of species and their endpoints.
#' @param endpoint_descriptions Optional named list of reviewed display metadata
#'   parsed once in `app_server()` from `inst/config/endpoint_descriptions.yaml`.
#' @return Called for its side effect.
#' @keywords internal
#' @noRd
mod_browse_priors_server <- function(
    id = NULL,
    list_calculated_priors = NULL,
    endpoint_descriptions = NULL
) {
  if (FALSE) {
    id <- "browse_priors"
    list_calculated_priors <- shiny::reactive(list(priors = list(dog = list(q_tc = list()))))
    endpoint_descriptions <- .read_endpoint_descriptions()
  }

  output <- shiny::moduleServer(id, function(input, output, session) {

    priors_tree <- shiny::reactive({
      bundle <- list_calculated_priors()
      output <- if (is.null(bundle)) NULL else bundle[["priors"]]
      return(output)
    })

    avail_prior_species <- shiny::reactive({
      output <- if (is.null(priors_tree())) {
        character(0)
      } else {
        names(priors_tree())
      }
      return(output)
    })
    
    avail_priors <- shiny::reactive({
      shiny::req(input$Species_Browse)
      tree <- priors_tree()
      output <- if (is.null(tree) || !input$Species_Browse %in% names(tree)) {
        character(0)
      } else {
        names(tree[[input$Species_Browse]])
      }
      return(output)
    })

    shiny::observe({
      species_choices <- .display_species_choices(
        species_values = avail_prior_species(),
        endpoint_descriptions = endpoint_descriptions
      )
      selected_sp <- if (length(species_choices) > 0) species_choices[[1]] else ""
      shiny::updateSelectInput(session, "Species_Browse",
                               choices = species_choices,
                               selected = selected_sp)
    })

    shiny::observeEvent(input$Species_Browse, {
      endpoint_choices <- .display_endpoint_choices(
        species = input$Species_Browse,
        endpoint_values = avail_priors(),
        endpoint_descriptions = endpoint_descriptions
      )
      selected_ep <- if (length(endpoint_choices) > 0) endpoint_choices[[1]] else ""
      shiny::updateSelectInput(session, "Prior_Browse",
                               choices = endpoint_choices,
                               selected = selected_ep)
    })
    
    reactive_priors_browsing <- shiny::reactive({
      shiny::req(input$Species_Browse)
      output <- priors_tree()[[input$Species_Browse]]
      return(output)
    })
    
    reac_plot <- shiny::reactive({
      shiny::req(reactive_priors_browsing())
      shiny::req(
            input$Prior_Browse %in% names(reactive_priors_browsing())
          )
      
      chain_plots <- chain_prior_plot(
        interval_start_times = c(0, 3, 6, 12, 18),
        interval_end_times = c(3, 6, 12, 18, 24),
        doses = seq(0, 3),
        prior_list = reactive_priors_browsing(),
        prior_name = input$Prior_Browse)

      display <- .display_metadata(
        endpoint_descriptions = endpoint_descriptions,
        species = input$Species_Browse,
        endpoint = input$Prior_Browse
      )
      
      arranged <- ggpubr::ggarrange(plotlist = chain_plots)
      plot_out <- ggpubr::annotate_figure(arranged,
        top = ggpubr::text_grob(
          glue::glue("{display$endpoint_display} selected for {display$species_label}"),
          vjust = 1.5,
          color = "black", face = "bold", size = 24
        )
      )

      return(plot_out)
    })
    
    shiny::observeEvent(
      list(input$Prior_Browse, input$Species_Browse), {
        output$Browsing_Priors_Plot <- shiny::renderPlot({
          reac_plot()
        })
      })
    
  })

  return(output)
}