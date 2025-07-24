#!! ModName = mod_visualize
#!! ModDisplayName = Visualize Data
#!! ModDescription = Create visualizations for project data
#!! ModCitation = Price, Jeremy F. (2025). mod_visualize. [Source code].
#!! ModNotes = This module provides functionality to visualize project data.
#!! ModActive = 1
#!! FunctionArg = project_data !! Project data for visualization !! reactive

# Load required libraries
#' @importFrom dplyr ungroup filter
#' @importFrom tidyr pivot_longer

# Read color_palette from _brand.yml using yaml package
if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml")
library(yaml)
brand <- yaml::read_yaml("www/_brand.yml")
color_palette <- unname(unlist(brand$default$color_palette))

# Font setup for Google Fonts (Lato and IBM Plex Mono)
if (!requireNamespace("showtext", quietly = TRUE)) install.packages("showtext")
if (!requireNamespace("sysfonts", quietly = TRUE)) install.packages("sysfonts")
library(showtext)
library(sysfonts)
font_add_google("Lato", "lato")
font_add_google("DM Mono", "ibmplexmono")
showtext_auto()

# Configuration for visualization types
VIZ_CONFIG <- list(
  indicators = list(
    name = "Indicators",
    icon = "compass-tool",
    color = "#3B6B35",
    description = "Shows raw counts of key project elements—like partners, hours, outputs, and participants—to quickly convey project scale and activity, useful for funders and administrators."
  ),
  alignment = list(
    name = "Alignment", 
    icon = "target",
    color = "#3F5E78",
    description = "Compares how researchers and partners perceive alignment in values, goals, and roles. High alignment means strong collaboration; gaps highlight areas for improvement."
  ),
  dynamics = list(
    name = "Dynamics",
    icon = "gear",
    color = "#4E342E", 
    description = "Visualizes project dynamics across core domains, helping identify strengths and areas needing balance or integration."
  ),
  cascade = list(
    name = "Cascade Effects",
    icon = "waveform",
    color = "#990000",
    description = "Shows how project effects cascade through network layers, revealing reach, flow, and equity across the community."
  )
)

# UI Function
mod_visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # JavaScript for custom visualization messages
    tags$script(HTML(sprintf("
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('runVisualization', function(message) {
          console.log('Received visualization request for:', message);
          Shiny.setInputValue('%s-visualize_' + message, Math.random(), {priority: 'event'});
        });
      });
    ", ns("")))),
    
    bslib::accordion(
      id = ns("visualize_accordion"),
      multiple = FALSE,
      
      # Main visualization panel
      bslib::accordion_panel(
        value = "visualize_data",
        title = tagList(phosphoricons::ph("compass-tool"), HTML("&nbsp;"), "Visualize Data"),
        create_visualization_controls_ui(ns)
      ),
      
      # Results panel
      bslib::accordion_panel(
        value = "results", 
        title = tagList(ph("flag-checkered", weight = "fill"), HTML("&nbsp;"), "Results"),
        div(id = ns("results_container"),
          uiOutput(ns("visualization_results"))
        )
      )
    )
  )
}

# Helper function to create visualization controls UI
create_visualization_controls_ui <- function(ns) {
  fluidRow(
    column(
      width = 12,
      p("This module allows you to visualize your project data. Select the type of visualization you want to perform and run the visualization."),
      
      # Visualization type selector
      tags$fieldset(
        class = "custom-fieldset",
        tags$legend("Select", class = "custom-legend"),
        shinyWidgets::radioGroupButtons(
          inputId = ns("visualization_type"),
          label = "VISUALIZATION TYPE",
          choices = list(
            "All Metrics" = "full",
            "Indicators" = "indicators", 
            "Alignment" = "alignment",
            "Dynamics" = "dynamics",
            "Cascade" = "cascade"
          ),
          selected = NULL,
          direction = "horizontal",
          justified = TRUE
        )
      ),
      
      # Run visualization controls
      tags$fieldset(
        class = "custom-fieldset",
        tags$legend("Visualize", class = "custom-legend"),
        div(
          class = "d-flex flex-column align-items-center gap-3",
          actionButton(
            inputId = ns("run_visualization"),
            label = tagList("Visualize Metrics ", ph("compass-tool", weight = "bold")),
            width = "50%",
            class = "btn btn-primary btn-lg"
          ),
          div(
            id = ns("progress_container"),
            style = "width: 50%; visibility: hidden;",
            shinyWidgets::progressBar(
              id = ns("progress_bar"),
              value = 0,
              total = 100,
              status = "success", 
              title = "Ready to Visualize",
              striped = FALSE,
              display_pct = FALSE
            )
          )
        )
      )
    )
  )
}

# Server Function
mod_visualize_server <- function(id, project_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for visualization state
    viz_state <- reactiveValues(
      current_type = NULL,
      message = NULL,
      status = NULL,
      results = list()
    )
    
    # Create workflow observers
    create_workflow_observers(project_data, ns, session)
    
    # Main visualization handler
    observeEvent(input$run_visualization, {
      req(input$visualization_type)
      
      tryCatch({
        viz_type <- input$visualization_type
        viz_state$current_type <- viz_type
        
        show_progress("Starting visualization...")
        
        if (viz_type == "full") {
          run_full_visualization()
        } else {
          run_single_visualization(viz_type)
        }
        
        hide_progress()
        update_workflow_status("complete")
        
      }, error = function(e) {
        handle_visualization_error(e)
      })
    })
    
    # Helper functions for visualization execution
    run_single_visualization <- function(viz_type) {
      show_progress(paste("Preparing", VIZ_CONFIG[[viz_type]]$name, "visualization..."), 30)
      
      # Get the appropriate data and create visualization
      viz_data <- get_visualization_data(viz_type)
      if (is.null(viz_data)) {
        stop(paste("No", viz_type, "data available. Please run analysis first."))
      }
      
      show_progress("Generating plots...", 70)
      viz_result <- create_visualization_result(viz_type, viz_data)
      viz_state$results[[viz_type]] <- viz_result
      
      show_progress("Completed!", 100)
      viz_state$status <- "success"
      viz_state$message <- paste(VIZ_CONFIG[[viz_type]]$name, "visualization completed successfully!")
    }
    
    run_full_visualization <- function() {
      viz_types <- c("indicators", "alignment", "dynamics", "cascade")
      progress_steps <- seq(10, 90, length.out = length(viz_types))
      
      for (i in seq_along(viz_types)) {
        viz_type <- viz_types[i]
        show_progress(paste("Processing", VIZ_CONFIG[[viz_type]]$name, "..."), progress_steps[i])
        
        viz_data <- get_visualization_data(viz_type)
        if (!is.null(viz_data)) {
          viz_result <- create_visualization_result(viz_type, viz_data)
          viz_state$results[[viz_type]] <- viz_result
        }
        Sys.sleep(0.2) # Small delay for UI feedback
      }
      
      show_progress("Full visualization completed!", 100)
      viz_state$status <- "success"
      viz_state$message <- "Full visualization completed successfully!"
    }
    
    # Data retrieval function
    get_visualization_data <- function(viz_type) {
      switch(viz_type,
        "indicators" = project_data$cleaned_data$indicators,
        "alignment" = get_analysis_data("alignment"),
        "dynamics" = get_analysis_data("dynamics"), 
        "cascade" = get_analysis_data("cascade"),
        NULL
      )
    }
    
    get_analysis_data <- function(analysis_type) {
      analysis_data <- project_data$analysis
      if (is.null(analysis_data)) return(NULL)
      
      switch(analysis_type,
        "alignment" = if (isTRUE(analysis_data$alignment_analyzed)) analysis_data$alignment_table else NULL,
        "dynamics" = if (isTRUE(analysis_data$dynamics_analyzed)) {
          list(
            domain_scores = analysis_data$dynamics_domains,
            dimension_scores = analysis_data$dynamics_dimensions_table
          )
        } else NULL,
        "cascade" = if (isTRUE(analysis_data$cascade_analyzed)) analysis_data$cascade_data else NULL,
        NULL
      )
    }
    
    # Visualization result creation
    create_visualization_result <- function(viz_type, viz_data) {
      plots <- switch(viz_type,
        "indicators" = create_indicators_plots(viz_data, color_palette),
        "alignment" = create_alignment_plots(viz_data, color_palette),
        "dynamics" = create_dynamics_plot(
          domain_scores = viz_data$domain_scores,
          dimension_scores = viz_data$dimension_scores,
          color_palette = color_palette
        ),
        "cascade" = create_cascade_plots(viz_data, color_palette),
        stop("Unknown visualization type: ", viz_type)
      )
      
      list(
        plots = plots,
        data = viz_data,
        config = VIZ_CONFIG[[viz_type]]
      )
    }
    
    # Progress management
    show_progress <- function(message, value = NULL) {
      shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", 
                           ns("progress_container")))
      if (!is.null(value)) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = ns("progress_bar"),
          value = value,
          title = message
        )
      }
      viz_state$message <- message
    }
    
    hide_progress <- function() {
      shinyjs::delay(1000, {
        shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", 
                             ns("progress_container")))
      })
    }
    
    # Error handling
    handle_visualization_error <- function(e) {
      error_msg <- paste("Visualization error:", conditionMessage(e))
      viz_state$status <- "error"
      viz_state$message <- error_msg
      hide_progress()
      cat("ERROR:", error_msg, "\n")
    }
    
    # Workflow status update
    update_workflow_status <- function(status) {
      update_workflow_step(
        project_data,
        stage = "Visualize Findings", 
        status = status,
        session = session
      )
    }
    
    # Results UI rendering
    output$visualization_results <- renderUI({
      if (length(viz_state$results) == 0) {
        return(create_no_results_ui())
      }
      
      # Create UI for each visualization result
      result_uis <- lapply(names(viz_state$results), function(viz_type) {
        create_result_section_ui(viz_type, viz_state$results[[viz_type]], ns)
      })
      
      do.call(tagList, result_uis)
    })
    
    # Dynamic plot rendering and download handlers
    observe({
      lapply(names(viz_state$results), function(viz_type) {
        result <- viz_state$results[[viz_type]]
        
        # Render preview plot
        output[[paste0(viz_type, "_preview")]] <- renderPlot({
          result$plots$preview
        }, height = 90, width = "auto")
        
        # Render main plot for overlay
        output[[paste0(viz_type, "_main")]] <- renderPlot({
          result$plots$main
        }, height = 600, width = 600)
        
        # Download handler
        output[[paste0(viz_type, "_download")]] <- downloadHandler(
          filename = function() {
            paste0(viz_type, "_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
          },
          content = function(file) {
            ggsave(
              file,
              plot = result$plots$main,
              device = "png",
              width = 10,
              height = 8,
              units = "in",
              dpi = 300,
              bg = "white"
            )
          }
        )
        
        # Overlay handlers
        create_overlay_handlers(viz_type, ns, session, output)
      })
    })
    
    # Return visualization state for external access
    list(
      results = reactive(viz_state$results),
      status = reactive(viz_state$status),
      message = reactive(viz_state$message)
    )
  })
}

# Helper UI functions
create_no_results_ui <- function() {
  tags$div(
    class = "data-placeholder text-center p-4",
    tags$div(
      class = "d-flex align-items-center justify-content-center gap-2",
      phosphoricons::ph("warning-circle", weight = "bold", class = "warning-icon"),
      tags$div(
        tags$strong("No Visualization Results"),
        tags$br(),
        "Please select a visualization type and run the visualization to view results."
      )
    )
  )
}

create_result_section_ui <- function(viz_type, result, ns) {
  config <- result$config
  
  tags$fieldset(
    class = "custom-fieldset mb-4",
    tags$legend(class = "custom-legend", toupper(config$name)),
    
    layout_columns(
      col_widths = c(4, 8),
      gap = "1rem",
      
      # Left: Preview and controls
      div(
        class = "d-flex flex-column h-100",
        bslib::value_box(
          title = config$name,
          value = plotOutput(ns(paste0(viz_type, "_preview")), height = "120px"),
          showcase = phosphoricons::ph(config$icon, weight = "fill"),
          theme = value_box_theme(bg = config$color, fg = "#f5f1e8"),
          class = "mb-3"
        ),
        div(
          class = "btn-group w-100",
          actionButton(
            ns(paste0(viz_type, "_expand")),
            "Expand",
            icon = icon("expand"),
            class = "btn-sm btn-outline-primary"
          ),
          downloadButton(
            ns(paste0(viz_type, "_download")),
            "Download", 
            icon = icon("download"),
            class = "btn-sm btn-outline-success"
          )
        )
      ),
      
      # Right: Description and status
      bslib::card(
        bslib::card_header(
          tags$div(
            class = "d-flex align-items-center justify-content-between",
            tags$span("READY FOR REPORT GENERATION", 
                     class = "fw-medium text-success text-uppercase font-monospace"),
            phosphoricons::ph("check-circle", weight = "fill", size = "2x", 
                            style = "color: var(--bs-success);")
          )
        ),
        bslib::card_body(
          HTML(paste0("<div style='font-size: 1.1em;'>", config$description, "</div>"))
        )
      )
    ),
    
    # Overlay UI
    uiOutput(ns(paste0(viz_type, "_overlay")))
  )
}

# Overlay management
create_overlay_handlers <- function(viz_type, ns, session, output) {
  overlay_active <- reactiveVal(FALSE)
  
  observeEvent(session$input[[paste0(viz_type, "_expand")]], {
    overlay_active(TRUE)
  })
  
  observeEvent(session$input[[paste0(viz_type, "_close")]], {
    overlay_active(FALSE)
  })
  
  output[[paste0(viz_type, "_overlay")]] <- renderUI({
    if (isTRUE(overlay_active())) {
      tagList(
        div(
          class = "overlay-bg",
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", 
                          ns(paste0(viz_type, "_close")))
        ),
        div(
          class = "overlay-card",
          style = "background: #edeae2; max-width: 90vw; max-height: 90vh;",
          tags$h4(VIZ_CONFIG[[viz_type]]$name, class = "mb-3"),
          plotOutput(ns(paste0(viz_type, "_main")), height = "70vh"),
          actionButton(
            ns(paste0(viz_type, "_close")),
            "Close",
            class = "btn-secondary float-end mt-3"
          )
        )
      )
    }
  })
}
