#!! ModName = mod_generate
#!! ModDisplayName = Generate Report
#!! ModDescription = Generate reports and verify project readiness.
#!! ModCitation = Price, Jeremy F. (2025). mod_generate. [Source code].
#!! ModNotes = This module provides functionality to verify project readiness and generate reports.
#!! ModActive = 1
#!! FunctionArg = project_data !! Project data for report generation !! reactive
#!! FunctionReturn = report_data !! Generated report data !! reactive
#!! FunctionReturn = status !! Report generation status !! reactive

# Utilities are loaded in global.R

# UI function with accordion panels
mod_generate_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Add shinyjs for UI interactions
    shinyjs::useShinyjs(),
    
    # Progress bar container (initially hidden)
    div(
      id = ns("progress_container"),
      style = "width: 100%; margin: 15px 0; visibility: hidden;",
      shinyWidgets::progressBar(
        id = ns("progress_bar"),
        value = 0,
        total = 100,
        status = "info",
        title = "Ready to verify",
        striped = FALSE,
        display_pct = FALSE
      )
    ),
    
    # Main accordion
    bslib::accordion(
      id = ns("generate_accordion"),
      multiple = FALSE,  # Only one panel open at a time
      open = "verify_readiness",  # Open first panel by default
      
      # Panel 1: Verify Readiness
      bslib::accordion_panel(
        value = "verify_readiness",
        title = tagList(
          phosphoricons::ph("check-circle"),
          HTML("&nbsp;Verify Readiness")
        ),
        
        # Verify button and status
        shiny::actionButton(
          ns("check_readiness"),
          "Check Project Readiness",
          icon = phosphoricons::ph_i("arrow-clockwise"),
          class = "btn-primary"
        ),
        
        # Status message
        shiny::uiOutput(ns("verification_status")),
        
        # Verification results
        shiny::uiOutput(ns("verification_results")),
        
        # Debug output (can be removed later)
        shiny::verbatimTextOutput(ns("debug_output"))
      ),
      
      # Panel 2: Generate Report
      bslib::accordion_panel(
        value = "generate_report",
        title = tagList(
          phosphoricons::ph_i("file-pdf"),
          HTML("&nbsp;Generate Report")
        ),
        
        # Generate button (initially disabled)
        shinyjs::disabled(
          shiny::actionButton(
            ns("generate_report"),
            "Generate Report",
            icon = phosphoricons::ph_i("file-pdf"),
            class = "btn-success"
          )
        ),
        
        # Report generation status
        shiny::uiOutput(ns("generation_status")),
        
        # Generated report preview/status
        shiny::uiOutput(ns("report_preview"))
      )
    )
  )
}

# Server function with verification logic
mod_generate_server <- function(id, project_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize reactive values
    rv <- shiny::reactiveValues(
      verification_status = NULL,
      verification_results = NULL,
      report_data = NULL,
      report_status = "pending"
    )
    
    # Debug output
    output$debug_output <- shiny::renderPrint({
      list(
        verification_status = rv$verification_status,
        all_checks_passed = rv$all_checks_passed
      )
    })
    
    # Verification status UI
    output$verification_status <- shiny::renderUI({
      if (is.null(rv$verification_status)) {
        return(tags$div(class = "alert alert-info", "Click 'Check Project Readiness' to verify your project."))
      }
      
      if (rv$verification_status == "checking") {
        return(tags$div(
          class = "alert alert-info",
          phosphoricons::ph_i("hourglass"),
          " Verifying project components..."
        ))
      }
      
      if (rv$verification_status == "complete") {
        if (rv$all_checks_passed) {
          return(tags$div(
            class = "alert alert-success",
            phosphoricons::ph("check-circle"),
            " All required components are present. You can now generate your report."
          ))
        } else {
          return(tags$div(
            class = "alert alert-warning",
            phosphoricons::ph_i("warning"),
            " Some required components are missing. Please check the verification results below."
          ))
        }
      }
    })
    
    # Verification results UI
    output$verification_results <- shiny::renderUI({
      if (is.null(rv$verification_results)) return(NULL)
      
      # Create a list group for the results
      shiny::tagList(
        shiny::h4("Verification Results"),
        do.call(shiny::tags$div, c(
          class = "list-group",
          lapply(seq_along(rv$verification_results), function(i) {
            item <- rv$verification_results[[i]]
            status_class <- switch(
              item$status,
              "present" = "list-group-item-success",
              "missing_optional" = "list-group-item-warning",
              "missing_required" = "list-group-item-danger",
              "error" = "list-group-item-danger",
              "list-group-item-secondary"
            )
            
            status_icon <- switch(
              item$status,
              "present" = phosphoricons::ph_i("check-circle", fill = "green"),
              "missing_optional" = phosphoricons::ph_i("warning", fill = "orange"),
              "missing_required" = phosphoricons::ph_i("x-circle", fill = "red"),
              "error" = phosphoricons::ph_i("x-circle", fill = "red"),
              phosphoricons::ph_i("question")
            )
            
            shiny::tags$div(
              class = paste("list-group-item d-flex justify-content-between align-items-center", status_class),
              item$name,
              status_icon
            )
          })
        ))
      )
    })
    
    # Toggle generate button based on verification status
    shiny::observe({
      req(rv$verification_status == "complete")
      shinyjs::toggleState("generate_report", condition = isTRUE(rv$all_checks_passed))
    })
    
    # Helper function to update progress bar
    update_progress <- function(value, text = NULL, status = "info") {
      shinyWidgets::updateProgressBar(
        session = session,
        id = "progress_bar",
        value = value,
        status = status,
        title = if (!is.null(text)) text else NULL
      )
    }
    
    # Log the module's environment for debugging
    message("\n=== MODULE INITIALIZATION ===")
    message("Module namespace: ", ns(""))
    
    # Helper function to log object details
    log_object <- function(obj, name) {
      message("\nObject: ", name)
      message("Type: ", paste0(class(obj), collapse = ", "))
      message("Is function: ", is.function(obj))
      message("Is reactive: ", inherits(obj, "reactive"))
      message("Is reactiveVal: ", inherits(obj, "reactiveVal"))
      message("Is reactiveExpr: ", inherits(obj, "reactiveExpr"))
      message("Is reactiveValues: ", inherits(obj, "reactiveValues"))
      message("Is.null: ", is.null(obj))
      
      # Log environment details if it's a reactive
      if (inherits(obj, "reactive") || is.function(obj)) {
        message("\nReactive environment details:")
        env <- environment(obj)
        message("Environment class: ", class(env))
        message("Environment parent: ", capture.output(environmentName(parent.env(env))))
        
        # Try to get the function body
        tryCatch({
          body_text <- capture.output(print(body(obj)))
          message("Function body:")
          message(paste("  ", body_text, collapse = "\n"))
        }, error = function(e) {
          message("Could not get function body: ", e$message)
        })
      }
    }
    
    # Check readiness button handler
    shiny::observeEvent(input$check_readiness, {
      message("\n=== VERIFICATION STARTED ===")
      
      # Reset and show progress bar
      shinyjs::runjs(sprintf('document.getElementById("%s").style.visibility = "visible";', ns("progress_container")))
      update_progress(0, "Starting verification...")
      
      # Set checking status
      rv$verification_status <- "checking"
      
      # Initialize results list
      results <- list()
      
      # Safely get and verify project data
      tryCatch({
        update_progress(5, "Loading project data...")
        
        # Get the current project data from reactiveValues
        message("\n--- project_data details ---")
        proj_data <- reactiveValuesToList(project_data)
        message("Project data structure:")
        str(proj_data, max.level = 2)
        
        # Check if project_data exists
        if (!exists("project_data")) {
          stop("project_data does not exist in the current environment")
        }
        
        # Check if project_data is a function or reactive
        if (!is.function(project_data) && !shiny::is.reactive(project_data)) {
          stop("project_data is not a function or reactive. Type: ", class(project_data)[1])
        }
        
        # Log detailed information about project_data
        message("\n--- project_data details (before access) ---")
        log_object(project_data, "project_data")
        
        # Function to safely extract value from reactive
        get_reactive_value <- function(r) {
          if (shiny::is.reactive(r)) {
            # If it's a reactive, try to get its value
            tryCatch({
              # Try to get the value directly
              val <- shiny::isolate(r())
              if (!is.null(val)) return(val)
              
              # If that fails, try to access the internal value
              env <- environment(r)
              if (exists(".value", envir = env)) {
                return(env$.value)
              }
              
              # Last resort: try to evaluate the reactive in a temporary observer
              result <- NULL
              obs <- shiny::observe({
                result <<- r()
              })
              obs$destroy()
              return(result)
              
            }, error = function(e) {
              message("Error getting reactive value: ", e$message)
              return(NULL)
            })
          } else if (is.function(r)) {
            # If it's a regular function, call it
            return(tryCatch(r(), error = function(e) NULL))
          }
          # If it's not a function or reactive, return as is
          return(r)
        }
        
        # Try to get the value from the reactive
        proj_data <- NULL
        
        # First, try to get the value directly
        tryCatch({
          message("Attempting to get value from reactive...")
          proj_data <- get_reactive_value(project_data)
          
          # If we got a reactive, try to get its value
          if (shiny::is.reactive(proj_data)) {
            message("Got nested reactive, trying to get its value...")
            proj_data <- get_reactive_value(proj_data)
          }
          
          # If we still don't have data, try to access the setup_module
          if (is.null(proj_data) && exists("setup_module", inherits = TRUE)) {
            message("Trying to access setup_module$project_data...")
            if (!is.null(setup_module$project_data)) {
              proj_data <- get_reactive_value(setup_module$project_data)
            }
          }
          
        }, error = function(e) {
          message("Error in reactive evaluation: ", e$message)
        })
        
        # If we still don't have data, try to find it in the parent environment
        if (is.null(proj_data)) {
          message("Searching for project data in parent environments...")
          env <- parent.frame()
          max_depth <- 5
          current_depth <- 0
          
          while (is.null(proj_data) && current_depth < max_depth && !identical(env, emptyenv())) {
            current_depth <- current_depth + 1
            if (exists("project_data", envir = env, inherits = FALSE)) {
              pd <- get("project_data", envir = env, inherits = FALSE)
              proj_data <- get_reactive_value(pd)
            }
            env <- parent.env(env)
          }
        }
        
        # Final check
        if (is.null(proj_data)) {
          stop("Could not retrieve project data. Please ensure the project is properly loaded.")
        }
        
        # Log what we got
        message("\n--- Retrieved data ---")
        message("Type: ", paste0(class(proj_data), collapse = ", "))
        if (is.list(proj_data)) {
          message("Names: ", paste0(names(proj_data), collapse = ", "))
          message("Structure:")
          str(proj_data, max.level = 1)
        }
        
        # Validate the data with more detailed error messages
        if (is.null(proj_data)) {
          message("Project data is NULL")
          stop("Project data is NULL. Please ensure the project is properly loaded.")
        }
        
        if (!is.list(proj_data)) {
          message("Project data is not a list. Type: ", class(proj_data)[1])
          stop("Project data is not in the expected format (expected a list). Got: ", 
               class(proj_data)[1])
        }
        # Log the structure for debugging
        message("Project data structure (class: ", class(proj_data), "):")
        str(proj_data, max.level = 2)
        
        # Check required fields in project_info
        required_fields <- c(
          "title", "description", "report_date",
          "report_formats", "report_keywords"
        )
        
        # Check for missing required fields in project_info
        missing_fields <- required_fields[!sapply(required_fields, function(x) {
          !is.null(proj_data$project_info[[x]]) && nzchar(proj_data$project_info[[x]])
        })]
        
        if (length(missing_fields) > 0) {
          update_progress(100, "Missing required project information", "danger")
          shiny::showNotification(
            paste("Missing required project info fields:", paste(missing_fields, collapse = ", ")),
            type = "error"
          )
          rv$verification_status <- "incomplete"
          return()
        }
        
        # Check optional fields
        optional_fields <- c(
          "project_risks", "project_assumptions", "project_constraints",
          "project_deliverables", "project_success_criteria",
          "project_stakeholders", "project_communication_plan",
          "project_quality_plan", "project_risk_management_plan",
          "project_change_management_plan"
        )
        
        # Check each optional field
        for (i in seq_along(optional_fields)) {
          field <- optional_fields[i]
          progress_value <- 50 + (i / length(optional_fields)) * 40
          update_progress(progress_value, paste("Verifying", field, "..."))
          
          if (is.null(proj_data[[field]]) || proj_data[[field]] == "") {
            results[[field]] <- list(
              status = "missing_optional",
              message = paste("Missing optional field:", field)
            )
          } else {
            results[[field]] <- list(
              status = "present",
              message = paste("Found:", field)
            )
          }
        }
        
        # Check for required data in cleaned_data
        required_data <- c("alignment", "indicators", "dynamics", "cascade")
        
        for (data_type in required_data) {
          if (is.null(proj_data$cleaned_data[[data_type]])) {
            results[[paste0("data_", data_type)]] <- list(
              status = "missing_required",
              message = paste("Missing required data:", data_type)
            )
          } else {
            data_obj <- proj_data$cleaned_data[[data_type]]
            if (is.data.frame(data_obj)) {
              results[[paste0("data_", data_type)]] <- list(
                status = "present",
                message = paste("Found data:", data_type, 
                              paste0("(", nrow(data_obj), " rows)"))
              )
            } else if (is.list(data_obj)) {
              results[[paste0("data_", data_type)]] <- list(
                status = "present",
                message = paste("Found data:", data_type, 
                              paste0("(", length(data_obj), " elements)"))
              )
            } else {
              results[[paste0("data_", data_type)]] <- list(
                status = "present",
                message = paste("Found data:", data_type, 
                              paste0("(", class(data_obj)[1], ")"))
              )
            }
          }
        }
        
        # Check data groups (at least one required in each group)
        cleaned_data_exists <- any(c(
          !is.null(proj_data$cleaned_data$alignment),
          !is.null(proj_data$cleaned_data$indicators),
          !is.null(proj_data$cleaned_data$dynamics),
          !is.null(proj_data$cleaned_data$cascade)
        ))
        
        analyzed_data_exists <- any(c(
          !is.null(proj_data$analysis$alignment_analyzed) && proj_data$analysis$alignment_analyzed,
          !is.null(proj_data$analysis$dynamics_analyzed) && proj_data$analysis$dynamics_analyzed,
          !is.null(proj_data$analysis$cascade_analyzed) && proj_data$analysis$cascade_analyzed
        ))
        
        visualized_data_exists <- any(c(
          !is.null(proj_data$visualization$alignment),
          !is.null(proj_data$visualization$dynamics),
          !is.null(proj_data$visualization$cascade)
        ))
        
        # Add data status to results
        results$cleaned_data_status <- list(
          status = if (cleaned_data_exists) "present" else "missing_required",
          message = if (cleaned_data_exists) "Cleaned data found" else "No cleaned data available"
        )
        
        results$analyzed_data_status <- list(
          status = if (analyzed_data_exists) "present" else "missing_required",
          message = if (analyzed_data_exists) "Analysis results found" else "No analysis results available"
        )
        
        results$visualization_status <- list(
          status = if (visualized_data_exists) "present" else "missing_optional",
          message = if (visualized_data_exists) "Visualizations found" else "No visualizations available"
        )
        
        # Simulate processing time
        Sys.sleep(1.5)
        
        # Check if all required items are present
        required_checks <- vapply(results, function(x) {
          !is.null(x$status) && x$status != "missing_required"
        }, FUN.VALUE = logical(1))
        
        # Update results with error handling
        rv$verification_results <- results
        rv$verification_status <- "complete"
        rv$all_checks_passed <- all(required_checks, na.rm = TRUE)
        
        # Update UI based on verification status
        if (isTRUE(rv$all_checks_passed)) {
          update_progress(100, "Verification complete - All checks passed!", "success")
          shiny::showNotification("All required components are present. You can now generate your report.", 
                               type = "message")
        } else {
          update_progress(100, "Verification complete - Issues found", "danger")
        }
        
        # Hide progress bar after a delay
        shinyjs::delay(2000, {
          shinyjs::runjs(sprintf('document.getElementById("%s").style.visibility = "hidden";', 
                              ns("progress_container")))
        })
        
      }, error = function(e) {
        update_progress(100, paste("Error:", e$message), "danger")
        shiny::showNotification(paste("Error during verification:", e$message), 
                              type = "error")
        rv$verification_status <- "error"
        
        # Hide progress bar after a delay
        shinyjs::delay(2000, {
          shinyjs::runjs(sprintf('document.getElementById("%s").style.visibility = "hidden";', 
                              ns("progress_container")))
        })
        
        return()
      })
    })  # End of observeEvent
    
    # Generate report button handler
    shiny::observeEvent(input$generate_report, {
      shiny::showNotification("Generating report...", type = "message")
      
      # Simulate report generation
      shiny::withProgress(
        message = 'Generating report...',
        value = 0,
        {
          # Simulate processing time
          for (i in 1:10) {
            shiny::incProgress(1/10)
            Sys.sleep(0.2)
          }
          
          # Show completion message
          shiny::showNotification("Report generated successfully!", type = "message")
          
          # Update UI to show report is ready
          output$generation_status <- shiny::renderUI({
            tags$div(
              class = "alert alert-success",
              phosphoricons::ph_i("check-circle"),
              " Report generated successfully!"
            )
          })
          
          output$report_preview <- shiny::renderUI({
            tags$div(
              class = "mt-3",
              shiny::h4("Report Preview"),
              tags$div(
                class = "card",
                style = "height: 300px; background-color: #f8f9fa; display: flex; align-items: center; justify-content: center;",
                tags$p("Generated report would be displayed here")
              )
            )
          })
        }
      )
    })
    
    # Return module outputs
    list(
      report_data = shiny::reactive({
        # This would return the actual report data when implemented
        NULL
      }),
      status = shiny::reactive({
        if (is.null(rv$verification_status)) "Ready to verify"
        else if (rv$verification_status == "complete") {
          if (rv$all_checks_passed) "Ready to generate report"
          else "Verification failed - missing required components"
        } else {
          "Verifying..."
        }
      })
    )
  })
}
