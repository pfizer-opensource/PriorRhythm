# mod_analysis_results ----

#' @title Analysis and Results Module UI — Set Up Analysis Panel
#' @description Main panel tab for the analysis setup.
#' @param id Character. Module namespace ID.
#' @return A `shiny::tabPanel` tag.
#' @keywords internal
#' @noRd
mod_analysis_results_setup_panel_ui <- function(id = NULL) {
  ns <- shiny::NS(id)
  ui_out <- shiny::tabPanel(
    title = "Set up Analysis",
    value = "Set_up_Analysis",
    
    shiny::plotOutput(ns("analysis_priors_plot"),
                      width = "100%", height = "500px"),
    
    DT::dataTableOutput(ns("derived"))
  )
  return(ui_out)
}

#' @title Analysis and Results Module UI — Results Panel
#' @description Main panel tab for the results.
#' @param id Character. Module namespace ID.
#' @return A `shiny::tabPanel` tag.
#' @keywords internal
#' @noRd
mod_analysis_results_panel_ui <- function(id = NULL) {
  ns <- shiny::NS(id)
  ui_out <- shiny::tabPanel(
    title = "Results",
    value = "Results",
    
    shiny::plotOutput(ns("results1"),
                      height = "700px"),
    
    DT::dataTableOutput(ns("results_dat"))
  )
  return(ui_out)
}

#' @title Analysis and Results Module UI — Sidebar
#' @description Sidebar inputs for the analysis setup and results tabs.
#' @param id Character. Module namespace ID.
#' @param method_choices Named character vector mapping display labels to
#'   internal method identifiers. Defaults to the four standard historical-
#'   prior and weak-prior comparator methods. Pass a custom vector to
#'   override, or `NULL` to disable method selection entirely.
#' @return A `shiny::tagList` of two `shiny::conditionalPanel` tags.
#' @keywords internal
#' @noRd
mod_analysis_results_sidebar_ui <- function(
    id = NULL,
    method_choices = .method_choices(wrap = TRUE)) {
  
  if (is.null(method_choices)) {
    method_choices <- character(0)
  }
  
  ns <- shiny::NS(id)
  ui_out <- shiny::tagList(
    
    # UI setup analysis ----
    shiny::conditionalPanel('input.tab === "Set_up_Analysis"',
                            
                            shiny::h4("Set prior"),
                            
                            shiny::selectInput(ns("species_analysis"), "Species",
                                               choices = "",
                                               selected = ""),
                            
                            shiny::selectInput(ns("prior_analysis"), "Prior",
                                               choices = ""),
                            
                            shiny::tags$hr(),
                            shiny::h4("Set variables"),
                            
                            shiny::selectInput(ns("y_col"), "Y variable",
                                               choices = NULL),
                            
                            shiny::selectInput(ns("animal_col"), "Animal code col",
                                               choices = NULL),
                            
                            shiny::selectInput(ns("period_col"), "Period code col",
                                               choices = NULL),
                            
                            shiny::selectInput(ns("dose_col"), "Treatment code col",
                                               choices = NULL),
                            
                            shiny::selectInput(ns("time_code_col"), "Time code",
                                               choices = NULL),
                            
                            shiny::tags$hr(),
                            shiny::h4("Set Analysis"),
                            
                            shiny::numericInput(ns("y_threshold"), "Y Threshold",
                                                value = ""),
                            
                            shiny::numericInput(ns("n_exclude"),
                                                "Number of subject(s) to exclude:",
                                                value = 1),
                            
                            shinyjs::disabled(
                              shiny::actionButton(ns("btn_execute"),
                                                  label = "Execute",
                                                  class = "btn-success"))
                            
    ), # condition end
    
    # UI get Results ----
    shiny::conditionalPanel('input.tab === "Results"',
                            
                            shiny::h5(shiny::strong("Plot type:")),
                            shinyWidgets::switchInput(inputId = ns("mode"),
                                                      value = TRUE,
                                                      onLabel = "CI",
                                                      offLabel = "Prob"),
                            
                            shiny::selectInput(ns("method_inputs"), "Select method",
                                               choices  = names(method_choices),
                                               selected = names(method_choices),
                                               multiple = TRUE),
                            
                            shiny::tags$hr(),
                            shiny::h5(shiny::strong("Plot customization:")),
                            
                            shiny::numericInput(ns("title_font_size"),
                                                "Title font size",
                                                value = 16,
                                                min   = 6,
                                                max   = 40,
                                                step  = 1),
                            
                            shiny::numericInput(ns("legend_font_size"),
                                                "Legend font size",
                                                value = 16,
                                                min   = 6,
                                                max   = 40,
                                                step  = 1),
                            
                            shiny::numericInput(ns("legend_title_font_size"),
                                                "Legend title font size",
                                                value = 16,
                                                min   = 6,
                                                max   = 40,
                                                step  = 1),
                            
                            shiny::numericInput(ns("x_axis_font_size"),
                                                "X-axis label font size",
                                                value = 16,
                                                min   = 6,
                                                max   = 40,
                                                step  = 1),
                            
                            shiny::numericInput(ns("y_axis_font_size"),
                                                "Y-axis label font size",
                                                value = 16,
                                                min   = 6,
                                                max   = 40,
                                                step  = 1),
                            
                            shiny::numericInput(ns("x_axis_tick_font_size"),
                                                "X-axis tick-label font size",
                                                value = 16,
                                                min   = 6,
                                                max   = 40,
                                                step  = 1),
                            
                            shiny::numericInput(ns("y_axis_tick_font_size"),
                                                "Y-axis tick-label font size",
                                                value = 16,
                                                min   = 6,
                                                max   = 40,
                                                step  = 1),
                            
                            shiny::numericInput(ns("facet_font_size"),
                                                "Facet label font size",
                                                value = 16,
                                                min   = 6,
                                                max   = 40,
                                                step  = 1),
                            
                            shiny::numericInput(ns("aspect_ratio"),
                                                "Aspect ratio",
                                                value = 1 / 2,
                                                min   = 0.1,
                                                max   = 5,
                                                step  = 0.1)
                            
    ) # condition end
  )
  return(ui_out)
}

#' @title Analysis and Results Module Server
#' @description Server logic for the analysis setup and results tabs.
#'   Populates prior selections from support data, drives the Bayesian test
#'   on button click, and renders CI and probability plots.
#' @param id Character. Module namespace ID.
#' @param list_calculated_priors A reactive returning the normalised prior
#'   bundle (output of `resolve_prior_bundle()`), containing a `priors` list
#'   of species and their endpoints.
#' @param contents_re A reactive returning the processed upload data list
#'   from `mod_file_upload_server()`.
#' @param endpoint_descriptions Optional named list of reviewed display metadata
#'   parsed once in `app_server()` from `inst/config/endpoint_descriptions.yaml`.
#' @param method_choices Named character vector mapping display labels to
#'   internal method identifiers. Must match the vector passed to
#'   `mod_analysis_results_sidebar_ui()`. If `NULL`, no method filtering
#'   is applied.
#' @return Called for its side effect.
#' @keywords internal
#' @noRd
mod_analysis_results_server <- function(
    id = NULL,
    list_calculated_priors = NULL,
    contents_re = NULL,
    endpoint_descriptions = NULL,
    method_choices = .method_choices(wrap = TRUE)) {
  if (FALSE) {
    id <- "analysis_results"
    list_calculated_priors <- shiny::reactive(list(priors = list(dog = list(q_tc = list(pa = data.frame())))))
    contents_re <- shiny::reactive(list(
      df = data.frame(),
      col_codes = character(0),
      time_vars = c("time", "time1", "time2"),
      numeric_ids = character(0)
    ))
    endpoint_descriptions <- .read_endpoint_descriptions()
    method_choices <- .method_choices(wrap = TRUE)
  }
  
  if (is.null(method_choices)) {
    method_choices <- character(0)
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
      shiny::req(input$species_analysis)
      tree <- priors_tree()
      output <- if (is.null(tree) || !input$species_analysis %in% names(tree)) {
        character(0)
      } else {
        names(tree[[input$species_analysis]])
      }
      return(output)
    })

    shiny::observe({
      species_choices <- .display_species_choices(
        species_values = avail_prior_species(),
        endpoint_descriptions = endpoint_descriptions
      )
      selected_sp <- if (length(species_choices) > 0) species_choices[[1]] else ""
      shiny::updateSelectInput(session, "species_analysis",
                               choices = species_choices,
                               selected = selected_sp)
    })

    shiny::observeEvent(input$species_analysis, {
      endpoint_choices <- .display_endpoint_choices(
        species = input$species_analysis,
        endpoint_values = avail_priors(),
        endpoint_descriptions = endpoint_descriptions
      )
      selected_ep <- if (length(endpoint_choices) > 0) endpoint_choices[[1]] else ""
      shiny::updateSelectInput(session, "prior_analysis",
                               choices = endpoint_choices,
                               selected = selected_ep)
    })
    
    shiny::observeEvent(contents_re(), {
      shiny::updateSelectInput(session, "animal_col",
                               choices = contents_re()$col_codes,
                               selected = grep("animal",
                                               contents_re()$col_codes,
                                               value = TRUE,
                                               ignore.case = TRUE))
      
      shiny::updateSelectInput(session, "time_code_col",
                               choices = contents_re()$col_codes,
                               selected = grep("time_code",
                                               contents_re()$col_codes,
                                               value = TRUE,
                                               ignore.case = TRUE))
      
      shiny::updateSelectInput(session, "dose_col",
                               choices = contents_re()$col_codes,
                               selected = grep("treat|dose",
                                               contents_re()$col_codes,
                                               value = TRUE,
                                               ignore.case = TRUE))
      
      shiny::updateSelectInput(session, "period_col",
                               choices = contents_re()$col_codes,
                               selected = grep("period",
                                               contents_re()$col_codes,
                                               value = TRUE,
                                               ignore.case = TRUE))
      
      shiny::updateSelectInput(session, "y_col",
                               choices = contents_re()$numeric_ids,
                               selected = "")
    })
    
    shiny::observeEvent(shiny::req(
      shiny::isTruthy(input$y_col) &
        shiny::isTruthy(input$y_threshold)), {
          shinyjs::enable("btn_execute")
        })
    
    reactive_priors_analysis <- shiny::reactive({
      shiny::req(input$species_analysis)
      output <- priors_tree()[[input$species_analysis]]
      return(output)
    }) 

    selected_display <- shiny::reactive({
      display <- .display_metadata(
        endpoint_descriptions = endpoint_descriptions,
        species = input$species_analysis,
        endpoint = input$prior_analysis
      )

      return(display)
    })
    
    # server set up analysis ----
    
    reactive_prior_analysis_plot <- shiny::reactive({
      shiny::req(reactive_priors_analysis())
      
      shiny::req(input$prior_analysis)
      
      shiny::req(
        input$prior_analysis %in% names(reactive_priors_analysis())
      )
      
      chain_plots <- chain_prior_plot(
        interval_start_times = c(0, 3, 6, 12, 18),
        interval_end_times = c(3, 6, 12, 18, 24),
        doses = seq(0, 3),
        prior_list = reactive_priors_analysis(),
        prior_name = input$prior_analysis)
      
      arranged <- ggpubr::ggarrange(plotlist = chain_plots)
      display <- selected_display()
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
      list(input$prior_analysis, input$species_analysis), {
        output$analysis_priors_plot <- shiny::renderPlot({
          reactive_prior_analysis_plot()
        })
      })
    
    derived_dat <- shiny::reactive({
      shiny::req(shiny::isTruthy(input$y_col))
      shiny::req(input$animal_col)
      new_cols <- c(
        contents_re()$time_vars,
        input$animal_col,
        input$dose_col,
        input$period_col,
        input$time_code_col,
        input$y_col
      )
      contents_re()$df |>
        dplyr::select(dplyr::all_of(new_cols))
    })
    
    output$derived <- DT::renderDataTable({
      derived_dat() |>
        DT::datatable(
          rownames = FALSE,
          options = list(
            dom = "Bt",
            scrollY = 500,
            scrollX = 300,
            scrollCollapse = TRUE,
            pageLength = -1
          ))
    })
    
    # server results tab ----
    
    shiny::observeEvent(input$btn_execute, {
      shinybusy::show_modal_spinner(session = session,
                                    spin = "cube-grid",
                                    text = "Processing...")
      
      test_out1 <- bayesian_test_example(
        input_tdat = derived_dat(),
        animal_col = input$animal_col,
        treatment_col = input$dose_col,
        period_col = input$period_col,
        time1_col = "time1",
        time2_col = "time2",
        y_col = input$y_col,
        n_exclude = input$n_exclude,
        pa = reactive_priors_analysis()[[input$prior_analysis]]$pa)
      
      shinybusy::remove_modal_spinner(session = session)
      
      method_out <- method_choices[input$method_inputs]
      
      results_list_reactive <- shiny::reactive({
        display <- selected_display()
        ci_plot <- ci_plots(test_out1,
                            input_method = method_out,
                            threshold = input$y_threshold) +
          ggplot2::ggtitle(
            glue::glue("{display$endpoint_display} selected for {display$species_label}"),
            subtitle = glue::glue(
              "{input$n_exclude} animals excluded to simulate incomplete data"
            )) +
          ggplot2::theme_classic(base_size = 16) +
          ggplot2::theme(
            plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
            plot.title = ggplot2::element_text(size = input$title_font_size, hjust = 0.5),
            legend.text = ggplot2::element_text(size = input$legend_font_size),
            legend.title = ggplot2::element_text(size = input$legend_title_font_size),
            axis.title.x = ggplot2::element_text(size = input$x_axis_font_size),
            axis.title.y = ggplot2::element_text(size = input$y_axis_font_size),
            axis.text.x = ggplot2::element_text(size = input$x_axis_tick_font_size),
            axis.text.y = ggplot2::element_text(size = input$y_axis_tick_font_size),
            strip.text = ggplot2::element_text(size = input$facet_font_size),
            legend.position = "bottom",
            aspect.ratio = input$aspect_ratio) +
          ggplot2::scale_x_discrete(
            label = function(x) gsub("treatment_code", "trt ", x))
        
        prob_plot <- prob_statement_plots(test_out1,
                                          input_method = method_out,
                                          threshold = input$y_threshold) +
          ggplot2::ggtitle(
            glue::glue("{display$endpoint_display} selected for {display$species_label}")) +
          ggplot2::theme_classic(base_size = 16) +
          ggplot2::theme(
            plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
            plot.title = ggplot2::element_text(size = input$title_font_size, hjust = 0.5),
            legend.text = ggplot2::element_text(size = input$legend_font_size),
            legend.title = ggplot2::element_text(size = input$legend_title_font_size),
            axis.title.x = ggplot2::element_text(size = input$x_axis_font_size),
            axis.title.y = ggplot2::element_text(size = input$y_axis_font_size),
            axis.text.x = ggplot2::element_text(size = input$x_axis_tick_font_size),
            axis.text.y = ggplot2::element_text(size = input$y_axis_tick_font_size),
            strip.text = ggplot2::element_text(size = input$facet_font_size),
            aspect.ratio = input$aspect_ratio)
        
        return(list(CI = ci_plot, Prob = prob_plot,
                    ci_dat   = .apply_method_labels(ci_plot$data),
                    prob_dat = .apply_method_labels(prob_plot$data)))
      })
      
      results1_reactive <- shiny::reactive({
        if (input$mode == TRUE) {
          results_list_reactive()$CI
        } else if (input$mode == FALSE) {
          results_list_reactive()$Prob
        }
      })
      
      output$results1 <- shiny::renderPlot({
        results1_reactive()
      })
      
      output$results_dat <- DT::renderDataTable({
        dat <- if (isTRUE(input$mode)) {
          results_list_reactive()$ci_dat
        } else {
          results_list_reactive()$prob_dat
        }
        dat |>
          tibble::as_tibble() |>
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~signif(.x))) |>
          DT::datatable(
            extensions = "Buttons",
            rownames = FALSE,
            options = list(
              dom = "Bt",
              scrollY = 500,
              scrollX = 300,
              scrollCollapse = TRUE,
              pageLength = -1
            ))
      })
    })
    
  })

  return(output)
}