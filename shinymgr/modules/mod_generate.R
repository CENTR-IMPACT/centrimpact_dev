#!! ModName = mod_generate
#!! ModDisplayName = Generate Report
#!! ModDescription = Generate reports and verify project readiness.
#!! ModCitation = Price, Jeremy F. (2025). mod_generate. [Source code].
#!! ModNotes = This module provides functionality to verify project readiness and generate reports.
#!! ModActive = 1
#!! FunctionArg = project_data !! Project data for report generation !! reactive
#!! FunctionReturn = report_data !! Generated report data !! reactive
#!! FunctionReturn = status !! Report generation status !! reactive

# Load required libraries
#' @importFrom phosphoricons ph ph_i
#' @importFrom shiny NS tagList HTML
#' @importFrom bslib accordion accordion_panel accordion_panel_open
#' @importFrom shinyjs disable enable
#' @importFrom utils head tail

# the ui function
mod_generate_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Initialize shinyjs
    shinyjs::useShinyjs(),
    
    bslib::accordion(
      id = ns("generate_accordion"),
      multiple = FALSE,
      open = "verify_readiness",  # Open the first panel by default
      bslib::accordion_panel(
        value = "verify_readiness",
        title = tagList(phosphoricons::ph_i("list-checks"), HTML("&nbsp;"), "Verify Readiness"),
        h4("Project Readiness Check"),
        p("Verify that all required components are ready before generating the report."),
        # Add your readiness check components here
        div(
          class = "d-grid gap-3",
          shiny::actionButton(
            inputId = ns("check_readiness"),
            label = "Check Components",
            icon = phosphoricons::ph_i("check-circle"),
            class = "btn-primary"
          ),
          shiny::uiOutput(ns("readiness_status"))
        )
      ),
      bslib::accordion_panel(
        value = "generate_report",
        title = tagList(phosphoricons::ph_i("pen-nib"), HTML("&nbsp;"), "Generate Report"),
        h4("Generate Project Report"),
        p("Generate a comprehensive report based on the project data."),
        # Add your report generation components here
        div(
          class = "d-grid gap-3",
          shiny::actionButton(
            inputId = ns("generate_report"),
            label = "Generate Report",
            icon = phosphoricons::ph_i("file-pdf"),
            class = "btn-success"
          ),
          shiny::uiOutput(ns("report_status"))
        )
      )
    )
  )
}


# Helper function to create verification result UI
verification_item_ui <- function(id, text, status = "checking") {
  icon_name <- switch(
    status,
    "checking" = "cube-focus",
    "present" = "check-circle",
    "missing_optional" = "info",
    "missing_required" = "warning-circle",
    "error" = "x-circle"
  )
  
  color_class <- switch(
    status,
    "checking" = "text-muted",
    "present" = "text-success",
    "missing_optional" = "text-info",
    "missing_required" = "text-warning",
    "error" = "text-danger"
  )
  
  tags$div(
    class = paste("d-flex align-items-center mb-2", color_class),
    phosphoricons::ph_i(icon_name, class = "me-2"),
    text
  )
}

# the server function
mod_generate_server <- function(id, project_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store report data and status
    report_data <- reactiveVal(NULL)
    status <- reactiveVal("Ready to check project components")
    
    # Define verification items and their requirements
    verification_items <- list(
      list(id = "project_title", text = "Project Title", required = TRUE),
      list(id = "report_date", text = "Report Date", required = TRUE),
      list(id = "project_description", text = "Project Description", required = FALSE),
      list(id = "report_formats", text = "Report Formats", required = TRUE),
      list(id = "project_keywords", text = "Project Keywords", required = FALSE),
      list(id = "author_name", text = "At least one author with Author Name", required = FALSE),
      list(id = "project_branding", text = "Project Branding File", required = FALSE),
      list(id = "cleaned_data", text = "Cleaned Data (any type)", required = FALSE, group = "data"),
      list(id = "cleaned_alignment", text = "Cleaned Alignment Data", required = FALSE, group = "data"),
      list(id = "cleaned_indicators", text = "Cleaned Indicators Data", required = FALSE, group = "data"),
      list(id = "cleaned_dynamics", text = "Cleaned Dynamics Data", required = FALSE, group = "data"),
      list(id = "cleaned_cascade", text = "Cleaned Cascade Effects Data", required = FALSE, group = "data"),
      list(id = "analyzed_alignment", text = "Analyzed Alignment Data", required = FALSE, group = "analysis"),
      list(id = "analyzed_dynamics", text = "Analyzed Dynamics Data", required = FALSE, group = "analysis"),
      list(id = "analyzed_cascade", text = "Analyzed Cascade Effects Data", required = FALSE, group = "analysis"),
      list(id = "visualized_indicators", text = "Visualized Indicators Metrics", required = FALSE, group = "visualization"),
      list(id = "visualized_alignment", text = "Visualized Alignment Metrics", required = FALSE, group = "visualization"),
      list(id = "visualized_dynamics", text = "Visualized Dynamics Metrics", required = FALSE, group = "visualization"),
      list(id = "visualized_cascade", text = "Visualized Cascade Effects Metrics", required = FALSE, group = "visualization")
    )
    
    # Create reactive values to track verification state
    verification_state <- reactiveValues(
      current_item = 0,
      results = list(),
      is_verifying = FALSE
    )
    
    # Initialize verification results
    lapply(seq_along(verification_items), function(i) {
      verification_state$results[[verification_items[[i]]$id]] <- "checking"
    })
    
    # Render the verification status UI
    output$readiness_status <- shiny::renderUI({
      lapply(verification_items, function(item) {
        verification_item_ui(
          id = ns(paste0("status_", item$id)),
          text = item$text,
          status = verification_state$results[[item$id]]
        )
      })
    })
    
    # Start verification process
    observeEvent(input$check_readiness, {
      if (verification_state$is_verifying) return()
      
      # Reset state
      verification_state$current_item <- 0
      verification_state$results <- list()
      verification_state$is_verifying <- TRUE
      
      # Disable the button during verification
      shinyjs::disable(ns("check_readiness"))
      status("Verifying project components...")
      
      # Start the verification process
      verification_state$current_item <- 1
    })
    
    # Process each verification item with a delay
    observe({
      if (!verification_state$is_verifying) return()
      if (verification_state$current_item > length(verification_items)) return()
      
      # Get current item
      item <- verification_items[[verification_state$current_item]]
      
      # Update status to checking
      verification_state$results[[item$id]] <- "checking"
      
      # Process the verification after a delay
      shiny::invalidateLater(300)  # Shorter delay for better UX
      
      shiny::isolate({
        # Simulate verification (replace with actual checks)
        if (item$required) {
          item_exists <- sample(c(TRUE, FALSE), 1, prob = c(0.8, 0.2))
        } else {
          item_exists <- sample(c(TRUE, FALSE), 1, prob = c(0.5, 0.5))
        }
        
        # Handle grouped items
        if (!is.null(item$group)) {
          group_items <- Filter(function(x) !is.null(x$group) && x$group == item$group, 
                              verification_items)
          group_exists <- any(sapply(group_items, function(x) {
            sample(c(TRUE, FALSE), 1, prob = c(0.7, 0.3))
          }))
          
          if (group_exists) {
            status <- "present"
          } else if (item$required) {
            status <- "missing_required"
          } else {
            status <- "missing_optional"
          }
        } else {
          status <- if (item_exists) "present" else 
                   if (item$required) "missing_required" else "missing_optional"
        }
        
        # Update the result
        verification_state$results[[item$id]] <- status
        
        # Move to next item or finish
        if (verification_state$current_item < length(verification_items)) {
          verification_state$current_item <- verification_state$current_item + 1
        } else {
          # Verification complete
          required_items <- Filter(function(x) x$required, verification_items)
          all_required_present <- all(sapply(required_items, function(x) {
            verification_state$results[[x$id]] == "present"
          }))
          
          if (all_required_present) {
            status("All required components are present. Ready to generate report.")
            bslib::accordion_panel_open("generate_accordion", "generate_report")
          } else {
            status("Some required components are missing. Please check the list above.")
          }
          
          # Reset state
          verification_state$is_verifying <- FALSE
          shinyjs::enable(ns("check_readiness"))
        }
      })
    })
    
    # Generate report
    observeEvent(input$generate_report, {
      # Update status
      status("Generating report...")
      
      # In a real implementation, you would generate the report here
      # For now, we'll simulate report generation with a delay
      shiny::invalidateLater(1500)  # 1.5 second delay
      
      shiny::isolate({
        # Update the UI with report status
        output$report_status <- shiny::renderUI({
          shiny::div(
            class = "alert alert-success mt-3",
            shiny::p(phosphoricons::ph_i("check-circle", class = "me-2"), 
                    "Report generated successfully!"),
            shiny::p("You can now download or view the generated report.")
          )
        })
        
        # In a real implementation, you would generate the actual report data here
        # For now, we'll just create a placeholder
        report_data(list(
          timestamp = Sys.time(),
          project_id = if(!is.null(project_data())) project_data()$id else "unknown",
          status = "completed"
        ))
        
        status("Report generated successfully")
      })
    })
    
    # Return reactive values
    return(
      list(
        report_data = reactive(report_data()),
        status = reactive(status())
      )
    )
  })
}
