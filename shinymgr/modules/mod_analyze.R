#!! ModName = mod_analyze

# Debug message to verify module is loaded
message("Analyze module is being loaded")
# !! ModDisplayName = Analyze Data
# !! ModDescription = Analyze your project data.
# !! ModCitation = Price, Jeremy F.  (2025). mod_analyze. [Source code].
# !! ModNotes = This module provides analysis functionality for the application.
# !! ModActive = 1
# !! FunctionArg = alignment_data !! Reactive containing the alignment data to analyze !! reactive
# !! FunctionReturn = analysis_results !! Analysis output containing statistical summaries !! reactive
# !! FunctionReturn = plot_data !! Data prepared for visualization !! reactive

# Load required libraries
#' @importFrom phosphoricons ph ph_i
#' @importFrom shiny NS tagList observe observeEvent reactive reactiveVal reactiveValues req debounce updateTextInput updateDateInput updateTextAreaInput renderUI showNotification
#' @importFrom shinyAce updateAceEditor
#' @importFrom DT renderDataTable datatable
#' @importFrom logger log_info log_warn log_error log_trace
#' @importFrom shinyjs useShinyjs delay html runjs
#' @importFrom bslib navset_card_tab nav_panel card card_body
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom utils create_status_card create_actions_card create_info_card

# Enable glue syntax for logger
.onLoad <- function(libname, pkgname) {
  logger::log_formatter(logger::formatter_glue)
}

# the ui function
mod_analyze_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(), # Initialize shinyjs
    tags$head(
      tags$style(HTML(paste0(
        "#", ns("analyze_status_message"), " {\n          display: block;\n          padding-left: 0.5em;\n          z-index: 10;\n          width: 100%;\n          font-family: 'Share Tech Mono' !important;\n        }",
        "#", ns("progress_analysis_container"), " {\n          visibility: hidden;\n        }"
      )))
    ),
    bslib::accordion(
      id = ns("analyze_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "overview",
        title = tagList(ph("lighthouse"), HTML("&nbsp;"), "Overview"),
        fluidRow(
          style = "padding: 1em;",
          column(
            width = 8,
            p(
              "This module allows you to analyze your project data. Select the type of analysis you want to perform and run the analysis.",
              style = "font-family: var(--font-sans) !important; padding: 10px; margin-bottom: 20px;"
            )
          ),
          column(
            width = 4,
            img(
              src = "analyze.png",
              alt = "Analyze Data Image",
              style = "max-width: 100%; height: auto; margin-top: 20px; padding-left: 10%; padding-right: 10%;"
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "analyze_data",
        title = tagList(ph("calculator"), HTML("&nbsp;"), "Analyze Data"),
        p(
          "This module allows you to analyze your project data. Select the type of analysis you want to perform and run the analysis. Use the buttons below to run each analysis."
        ),
        fluidRow(
          shinyWidgets::radioGroupButtons(
            inputId = ns("analysis_type"),
            label = "SELECT ANALYSIS TYPE",
            choices = list(
              "Analyze Alignment Data" = "alignment",
              "Analyze Dynamics Data" = "dynamics",
              "Analyze Cascade Data" = "cascade",
              "Analyze All Data" = "full"
            ),
            selected = NULL,
            direction = "horizontal",
            justified = TRUE,
          )
        ),
        fluidRow(
          class = "d-flex justify-content-center",
          div(
            id = ns("progress_analysis_container"),
            class = "progress-container",
            style = "width: 50%; visibility: hidden;",

            # Add a label above the progress bar
            tags$p("Running Analysis...", class = "progress-label", id = ns("progress_label")),

            # Use a standard shiny::div for the progress bar
            div(
              class = "progress",
              div(
                class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar",
                style = "width: 0%;",
                `aria-valuenow` = "0",
                `aria-valuemin` = "0",
                `aria-valuemax` = "100",
                id = ns("progress_bar_inner")
              )
            )
          )
        ),
        fluidRow(
          class = "d-flex justify-content-center",
          div(
            style = "width: 50%; position: relative;",
            # This is the dedicated placeholder for our new alert
            uiOutput(ns("status_alert_ui"))
          )
        ),
        fluidRow(
          class = "d-flex justify-content-center align-items-center",
          actionButton(
            inputId = ns("run_analysis"),
            label = "Run Analysis",
            class = "btn-primary",
            style = "width: 20%; margin: 0 auto; display: block;"
          )
        )
      ),
      bslib::accordion_panel(
        value = "alignment",
        title = tagList(ph("flower-lotus"), HTML("&nbsp;"), "Alignment"),
        shiny::uiOutput(ns("alignment_ui"))
      ),
      bslib::accordion_panel(
        value = "dynamics",
        title = tagList(ph("pulse"), HTML("&nbsp;"), "Dynamics"),
        shiny::uiOutput(ns("dynamics_ui"))
      ),
      bslib::accordion_panel(
        value = "cascade_effects",
        title = tagList(ph("waveform"), HTML("&nbsp;"), "Cascade Effects"),
        shiny::uiOutput(ns("cascade_ui"))
      )
    )
  )
}




# the server function with enhanced debugging
mod_analyze_server <- function(id, ns_workflow) {
  message("=== mod_analyze_server CALLED with id: ", id, " ===")



  # Debug the alignment_data parameter
  cat("\n=== MODULE INITIALIZATION ===\n")
  cat("ns_workflow type:", class(ns_workflow), "\n")
  cat("Is ns_workflow reactive?", is.reactive(ns_workflow), "\n\n")

  moduleServer(
    id = id,
    module = function(input, output, session) {
      message("=== MODULE SERVER INITIALIZED ===")

      # Reactive value to store console messages
      console_messages <- reactiveVal(character(0))

      # Render the console output
      output$console_output <- renderUI({
        # Force reactive dependency
        invalidateLater(100, session)

        # Return the messages as a list of divs
        tagList(
          lapply(console_messages(), function(msg) {
            msg
          })
        )
      })

      # Function to update console output
      update_console <- function(session, message, type = "info") {
        timestamp <- format(Sys.time(), "%H:%M:%S")

        # Create the new message with proper HTML
        new_msg <- tags$div(
          style = sprintf(
            "color: %s; margin-bottom: 5px;",
            switch(type,
              "info" = "#8ba086",
              "success" = "#4CAF50",
              "error" = "#F44336",
              "warning" = "#FFC107",
              "#8ba086"
            )
          ),
          sprintf("[%s] %s", timestamp, message)
        )

        # Update the reactive value with the new message
        current_msgs <- c(list(new_msg), console_messages())
        console_messages(current_msgs)

        # Force UI update
        session$sendCustomMessage("updateConsole", "")

        # Force reactive invalidation to ensure UI updates
        invalidateLater(0, session)
      }

      # Get the namespace for this module
      ns <- session$ns

      # Render the alignment guide table
      output$alignment_guide_table <- renderTable(
        {
          data.frame(
            `Alignment Score Range` = c("< 0.4", "0.4 - 0.59", "0.6 - 0.74", "> 0.75"),
            `Suggested Interpretation` = c(
              "Low Alignment", "Fair Alignment",
              "Good Alignment", "Excellent Alignment"
            ),
            check.names = FALSE
          )
        },
        striped = TRUE,
        hover = TRUE,
        width = "100%"
      )

      # Observer for refresh button
      observeEvent(input$refresh_debug, {
        log_info("Debug info refreshed at ", Sys.time())
        showNotification("Debug info refreshed", type = "message", duration = 2)
      })

      # Create a reactive to monitor the data
      data_status <- reactive({
        tryCatch(
          {
            if (is.null(alignment_data)) {
              log_error("alignment_data parameter is NULL")
              return("alignment_data parameter is NULL")
            }

            if (!is.reactive(alignment_data)) {
              log_error("alignment_data is not reactive, it's: {class(alignment_data)}")
              return(paste("alignment_data is not reactive, it's:", class(alignment_data)))
            }

            # Try to call the reactive
            log_debug("Attempting to access alignment_data()")
            data <- alignment_data()

            if (is.null(data)) {
              log_warn("alignment_data() returned NULL")
              return("alignment_data() returned NULL")
            }

            if (!is.data.frame(data)) {
              log_warn("alignment_data() returned non-data.frame: {class(data)}")
              return(paste("alignment_data() returned non-data.frame:", class(data)))
            }

            log_info("Data access successful: {nrow(data)} rows and {ncol(data)} columns")
            return(paste("SUCCESS: Data has", nrow(data), "rows and", ncol(data), "columns"))
          },
          error = function(e) {
            error_msg <- paste("ERROR calling alignment_data():", conditionMessage(e))
            log_error(error_msg)
            return(error_msg)
          }
        )
      })

      # Monitor data status continuously
      observe({
        status <- data_status()
        message("Data status: ", status)
      })

      # --- Observe per-metric workflow state for Alignment Clean Data completion and update status/icon ---
      observe({
        wf <- ns_workflow$workflow$alignment
        if (!is.null(wf) && wf$stage == "Clean Data" && wf$status == "complete") {
          update_alignment_status_display("cleaned", session, ns, ns_workflow)
          update_alignment_workflow_icons(ns_workflow, session)
        }
      })

      # --- NEW: Observe workflow state for Alignment Clean Data completion and update status/icon ---
      observe({
        if (!is.null(ns_workflow$stage) && !is.null(ns_workflow$status) && !is.null(ns_workflow$metric)) {
          if (
            ns_workflow$stage == "Clean Data" &&
              ns_workflow$status == "complete" &&
              tolower(ns_workflow$metric) == "alignment"
          ) {
            update_alignment_status_display("cleaned", session, ns, ns_workflow)
            update_alignment_workflow_icons(ns_workflow, session)
          }
        }
      })

      # Debug button to check data on demand
      observeEvent(input$debug_button, {
        # If we got here, the analysis was successful
        log_info("Debug button clicked - showing data status")

        # Show current data status
        data <- alignment_data()
        if (is.null(data)) {
          showNotification("No alignment data available", type = "warning")
        } else {
          showNotification(
            sprintf("Alignment data: %d rows, %d columns", nrow(data), ncol(data)),
            type = "message"
          )
        }

        log_debug("Debug information shown to user")
      })

      # Test button for simple functionality
      observeEvent(input$test_button, {
        log_info("=== TEST BUTTON CLICKED ===")
        showNotification("Test button works!", type = "message", duration = 5)
        log_debug("Test notification shown to user")
      })

      # Debug button to check data flow
      observeEvent(input$debug_data, {
        log_info("=== DEBUG DATA FLOW BUTTON CLICKED ===")

        # Check ns_workflow structure
        if (is.null(ns_workflow)) {
          log_error("ns_workflow is NULL")
          showNotification("ns_workflow is NULL", type = "error")
          return()
        }

        log_info("ns_workflow contains: {paste(names(ns_workflow), collapse = ', ')}")

        # Check each data type
        if (!is.null(ns_workflow$alignment_data)) {
          log_info("Alignment data available: {nrow(ns_workflow$alignment_data)} rows, {ncol(ns_workflow$alignment_data)} columns")
          showNotification(paste("Alignment data:", nrow(ns_workflow$alignment_data), "rows,", ncol(ns_workflow$alignment_data), "columns"), type = "message")
        } else {
          log_warn("No alignment data in ns_workflow")
          showNotification("No alignment data available", type = "warning")
        }

        if (!is.null(ns_workflow$dynamics_data)) {
          log_info("Dynamics data available: {nrow(ns_workflow$dynamics_data)} rows, {ncol(ns_workflow$dynamics_data)} columns")
          showNotification(paste("Dynamics data:", nrow(ns_workflow$dynamics_data), "rows,", ncol(ns_workflow$dynamics_data), "columns"), type = "message")
        } else {
          log_warn("No dynamics data in ns_workflow")
          showNotification("No dynamics data available", type = "warning")
        }

        if (!is.null(ns_workflow$cascade_data)) {
          if (is.list(ns_workflow$cascade_data)) {
            log_info("Cascade data available: list with elements: {paste(names(ns_workflow$cascade_data), collapse = ', ')}")
            showNotification(paste("Cascade data: list with", paste(names(ns_workflow$cascade_data), collapse = ", ")), type = "message")
          } else {
            log_info("Cascade data available: {class(ns_workflow$cascade_data)}")
            showNotification(paste("Cascade data:", class(ns_workflow$cascade_data)), type = "message")
          }
        } else {
          log_warn("No cascade data in ns_workflow")
          showNotification("No cascade data available", type = "warning")
        }

        log_debug("Debug data flow completed")
      })

      # Debug button to check dynamics results structure
      observeEvent(input$debug_dynamics_results, {
        log_info("=== DEBUG DYNAMICS RESULTS BUTTON CLICKED ===")
        
        if (is.null(rv_analysis$dynamics_results)) {
          log_warn("rv_analysis$dynamics_results is NULL")
          showNotification("No dynamics results available", type = "warning")
          return()
        }
        
        log_info("dynamics_results type: {class(rv_analysis$dynamics_results)}")
        log_info("dynamics_results columns: {paste(names(rv_analysis$dynamics_results), collapse = ', ')}")
        log_info("dynamics_results rows: {nrow(rv_analysis$dynamics_results)}")
        log_info("dynamics_results structure:")
        # Use a safer way to log the structure
        tryCatch({
          structure_output <- capture.output(str(rv_analysis$dynamics_results))
          for (line in structure_output) {
            log_info(line)
          }
        }, error = function(e) {
          log_warn("Could not log dynamics_results structure: {e$message}")
        })
        
        # Show a notification with basic info
        showNotification(
          paste("Dynamics results:", nrow(rv_analysis$dynamics_results), "rows,", 
                ncol(rv_analysis$dynamics_results), "columns"), 
          type = "message"
        )
        
        log_debug("Debug dynamics results completed")
      })

      # Reactive expressions to get data from ns_workflow
      alignment_data <- reactive({
        log_debug("Accessing alignment data reactively...")
        if (is.null(ns_workflow)) {
          log_warn("ns_workflow is NULL")
          return(NULL)
        }

        # Add more detailed debugging
        log_debug("ns_workflow contains: {paste(names(ns_workflow), collapse = ', ')}")

        if (is.null(ns_workflow$alignment_data)) {
          log_warn("No alignment_data in ns_workflow. Available elements: {paste(names(ns_workflow), collapse = ', ')}")
          return(NULL)
        }

        # Check if the data is actually a data frame
        if (!is.data.frame(ns_workflow$alignment_data)) {
          log_warn("alignment_data is not a data frame, it's: {class(ns_workflow$alignment_data)}")
          return(NULL)
        }

        log_info("Returning alignment data from reactive context: {nrow(ns_workflow$alignment_data)} rows, {ncol(ns_workflow$alignment_data)} columns")
        return(ns_workflow$alignment_data)
      })

      dynamics_data <- reactive({
        log_debug("Accessing dynamics data reactively...")
        if (is.null(ns_workflow)) {
          log_warn("ns_workflow is NULL")
          return(NULL)
        }

        # Add more detailed debugging
        log_debug("ns_workflow contains: {paste(names(ns_workflow), collapse = ', ')}")

        if (is.null(ns_workflow$dynamics_data)) {
          log_warn("No dynamics_data in ns_workflow. Available elements: {paste(names(ns_workflow), collapse = ', ')}")
          return(NULL)
        }

        # Check if the data is actually a data frame
        if (!is.data.frame(ns_workflow$dynamics_data)) {
          log_warn("dynamics_data is not a data frame, it's: {class(ns_workflow$dynamics_data)}")
          return(NULL)
        }

        log_info("Returning dynamics data from reactive context: {nrow(ns_workflow$dynamics_data)} rows, {ncol(ns_workflow$dynamics_data)} columns")
        return(ns_workflow$dynamics_data)
      })

      cascade_data <- reactive({
        log_debug("Accessing cascade data reactively...")

        # Safely log the structure of ns_workflow
        tryCatch(
          {
            log_info("ns_workflow contains:")
            if (!is.null(ns_workflow)) {
              log_info(paste("  -", names(ns_workflow), collapse = "\n"))
            } else {
              log_info("ns_workflow is NULL")
            }
          },
          error = function(e) {
            log_warn(paste("Could not log ns_workflow structure:", e$message))
          }
        )

        if (is.null(ns_workflow) || is.null(ns_workflow$cascade_data)) {
          log_warn("No cascade data available in ns_workflow")
          return(NULL)
        }

        cascade_data <- ns_workflow$cascade_data

        # Log basic info about cascade_data
        tryCatch(
          {
            log_info(paste("Type of cascade_data:", paste(class(cascade_data), collapse = ", ")))
            if (is.list(cascade_data)) {
              log_info(paste("List elements:", paste(names(cascade_data), collapse = ", ")))
            }
          },
          error = function(e) {
            log_warn(paste("Could not log cascade_data structure:", e$message))
          }
        )

        # Handle different cascade_data formats
        if (is.list(cascade_data) && all(c("cascade_df", "cascade_score") %in% names(cascade_data))) {
          log_info("Found cascade_data with cascade_df and cascade_score")
          if (is.data.frame(cascade_data$cascade_df)) {
            log_info(sprintf(
              "cascade_df: %d rows, %d columns",
              nrow(cascade_data$cascade_df),
              ncol(cascade_data$cascade_df)
            ))
            return(cascade_data$cascade_df)
          }
        }

        if (is.data.frame(cascade_data)) {
          log_info(sprintf(
            "Returning data frame: %d rows, %d columns",
            nrow(cascade_data),
            ncol(cascade_data)
          ))
          return(cascade_data)
        }

        if (is.list(cascade_data) && all(c("nodes", "edges") %in% names(cascade_data))) {
          log_info("Found network data with nodes and edges")
          if (is.data.frame(cascade_data$edges)) {
            log_info(sprintf(
              "Edges: %d rows, %d columns",
              nrow(cascade_data$edges),
              ncol(cascade_data$edges)
            ))
            return(cascade_data$edges)
          }
        }

        # If we get here, we couldn't handle the data format
        log_warn("Could not determine cascade data format")
        if (is.list(cascade_data)) {
          log_info(paste("Available elements:", paste(names(cascade_data), collapse = ", ")))
        }
        return(NULL)
      })

      # Reactive expression to check data status
      data_status <- reactive({
        # Get alignment data status
        align_data <- alignment_data()
        align_status <- if (is.null(align_data)) {
          "Alignment: Not available\n"
        } else {
          paste0("Alignment: ", nrow(align_data), " rows, ", ncol(align_data), " columns\n")
        }

        # Get dynamics data status
        dyn_data <- dynamics_data()
        dyn_status <- if (is.null(dyn_data)) {
          "Dynamics: Not available\n"
        } else {
          paste0("Dynamics: ", nrow(dyn_data), " rows, ", ncol(dyn_data), " columns\n")
        }

        # Get cascade data status
        casc_data <- cascade_data()
        casc_status <- if (is.null(casc_data)) {
          "Cascade: Not available"
        } else {
          paste0("Cascade: ", nrow(casc_data), " rows, ", ncol(casc_data), " columns")
        }

        # Return combined status
        paste0("Data Status:\n", align_status, dyn_status, casc_status)
      })

      # Initialize reactive values for analysis results
      rv_analysis <- reactiveValues(
        alignment_analyzed = FALSE,
        alignment_score = NULL,
        icc_scores = NULL,
        dynamics_analyzed = FALSE,
        dynamics_results = NULL,
        dynamics_score = NULL,
        full_results = NULL,
        cascade_analyzed = FALSE,
        cascade_results = NULL,
        cascade_score = NULL
      )

      # Centralized reactive values for the dynamic status alert
      rv_status <- reactiveValues(
        type = "info", # Can be 'info', 'success', 'warning', 'danger'
        message = "Ready to analyze. Please select an analysis type and click 'Run Analysis'."
      )

      # Observer to update status based on data availability
      observe({
        # Check if any data is available
        has_alignment <- !is.null(ns_workflow$alignment_data)
        has_dynamics <- !is.null(ns_workflow$dynamics_data)
        has_cascade <- !is.null(ns_workflow$cascade_data)

        # Log the current state
        log_debug("Data availability check - Alignment: {has_alignment}, Dynamics: {has_dynamics}, Cascade: {has_cascade}")

        if (has_alignment || has_dynamics || has_cascade) {
          available_types <- c()
          if (has_alignment) available_types <- c(available_types, "Alignment")
          if (has_dynamics) available_types <- c(available_types, "Dynamics")
          if (has_cascade) available_types <- c(available_types, "Cascade")

          rv_status$type <- "info"
          rv_status$message <- paste0("Data available for: ", paste(available_types, collapse = ", "), ". Select an analysis type and click 'Run Analysis'.")
          log_info("Data available: {paste(available_types, collapse = ', ')}")
        } else {
          rv_status$type <- "warning"
          rv_status$message <- "No data available for analysis. Please upload and clean data in the 'Upload Data' tab first."
          log_debug("No data available for analysis")
        }
      })

      # Output value for conditional panel
      output$alignment_analyzed <- reactive({
        rv_analysis$alignment_analyzed
      })
      outputOptions(output, "alignment_analyzed", suspendWhenHidden = FALSE)
      # Render dynamics icon
      output$dynamics_icon <- renderUI({
        if (isTRUE(rv_analysis$dynamics_analyzed)) {
          tags$span(style = "color: #4B7F52;", ph("check-circle", weight = "bold"))
        } else {
          tags$span(ph("circle", weight = "bold"))
        }
      })

      # Render cascade icon
      output$cascade_icon <- renderUI({
        if (isTRUE(rv_analysis$cascade_analyzed)) {
          tags$span(style = "color: #4B7F52;", ph("check-circle", weight = "bold"))
        } else {
          tags$span(ph("circle", weight = "bold"))
        }
      })

      # Render alignment icon
      output$alignment_icon <- renderUI({
        if (isTRUE(rv_analysis$alignment_analyzed)) {
          tags$span(style = "color: #4B7F52;", ph("check-circle", weight = "bold"))
        } else {
          tags$span(ph("circle", weight = "bold"))
        }
      })

      # Render ICC scores
      output$icc_scores_output <- renderPrint({
        req(rv_analysis$alignment_analyzed, rv_analysis$icc_scores)

        # Special handling for irr::icclist objects
        if (inherits(rv_analysis$icc_scores, "icclist")) {
          # Capture the output of the print method
          output_text <- capture.output(
            {
              print(rv_analysis$icc_scores)
            },
            type = "output"
          )

          # If we have output, show it with preserved formatting
          if (length(output_text) > 0) {
            cat(output_text, sep = "\n")
            return()
          }
        }

        # Fallback for non-icclist objects or if printing fails
        if (is.list(rv_analysis$icc_scores)) {
          # Try to extract key components
          icc_value <- tryCatch(
            {
              rv_analysis$icc_scores$value
            },
            error = function(e) NULL
          )

          if (!is.null(icc_value)) {
            cat("ICC Value:", icc_value, "\n\n")
            cat(
              "Confidence Interval:",
              rv_analysis$icc_scores$lbound, "to",
              rv_analysis$icc_scores$ubound, "\n"
            )
            cat("p-value:", rv_analysis$icc_scores$p.value, "\n")
            return()
          }
        }

        # Last resort: show structure
        cat("ICC Scores (raw structure):\n")
        str(rv_analysis$icc_scores)
      })



      # --- NEW RENDERUI LOGIC FOR INLINE ALERTS ---
      output$status_alert_ui <- renderUI({
        req(rv_status$type, rv_status$message)
        log_info("Rendering status alert: type={rv_status$type}, message={rv_status$message}")
        icon_name <- switch(rv_status$type,
          "success" = "check-circle",
          "danger"  = "x-circle",
          "info"    = "info",
          "warning" = "warning-circle")
        div(
          class = paste0("alert alert-", rv_status$type, " mt-4 d-flex align-items-center"),
          phosphoricons::ph(icon_name, weight = "fill", class = "alert-icon"),
          tags$span(rv_status$message, class = "alert-message-text")
        )
      })

      # Auto-hide logic for success/info/warning messages
      observe({
        req(rv_status$type, rv_status$message)

        # Automatically hide success/info/warning messages after 3 seconds.
        # Errors will persist until the next action.
        if (rv_status$type != "danger" && rv_status$type != "success") {
          shinyjs::delay(3000, {
            # Check if we have analyzed data to show appropriate message
            if (rv_analysis$alignment_analyzed || rv_analysis$dynamics_analyzed || rv_analysis$cascade_analyzed) {
              rv_status$type <- "info"
              rv_status$message <- "Analysis complete! Explore your results in the panels below."
            } else {
              rv_status$type <- "info"
              rv_status$message <- "Ready to analyze. Please select an analysis type and click 'Run Analysis'."
            }
          })
        }

        # Special handling for success messages - show for 3 seconds then show results viewing message
        if (rv_status$type == "success") {
          shinyjs::delay(3000, {
            rv_status$type <- "info"
            rv_status$message <- "Analysis complete! Explore your results in the panels below."
          })
        }
      })

      # Run analysis observer - handles the unified run_analysis button
      observeEvent(input$run_analysis, {
        log_info("=== RUN ANALYSIS BUTTON CLICKED ===")
        log_info("Button clicked, checking analysis type...")

        # Show a simple notification to confirm the button click is working
        showNotification("Run Analysis button clicked!", type = "message", duration = 2)

        if (is.null(input$analysis_type)) {
          log_warn("No analysis type selected. Please select an analysis type first.")
          rv_status$type <- "warning"
          rv_status$message <- "Please select an analysis type before running analysis."
          return()
        }

        log_info("Analysis type selected: {input$analysis_type}")

        # Debug: Check what data is available
        log_info("=== DATA AVAILABILITY CHECK ===")
        if (!is.null(ns_workflow)) {
          log_info("ns_workflow is available")
          log_info("Available elements in ns_workflow: {paste(names(ns_workflow), collapse = ', ')}")

          if (!is.null(ns_workflow$alignment_data)) {
            log_info("Alignment data available: {nrow(ns_workflow$alignment_data)} rows, {ncol(ns_workflow$alignment_data)} columns")
          } else {
            log_warn("No alignment data in ns_workflow")
          }

          if (!is.null(ns_workflow$dynamics_data)) {
            log_info("Dynamics data available: {nrow(ns_workflow$dynamics_data)} rows, {ncol(ns_workflow$dynamics_data)} columns")
          } else {
            log_warn("No dynamics data in ns_workflow")
          }

          if (!is.null(ns_workflow$cascade_data)) {
            if (is.list(ns_workflow$cascade_data)) {
              log_info("Cascade data available: list with elements: {paste(names(ns_workflow$cascade_data), collapse = ', ')}")
            } else {
              log_info("Cascade data available: {class(ns_workflow$cascade_data)}")
            }
          } else {
            log_warn("No cascade data in ns_workflow")
          }
        } else {
          log_warn("ns_workflow is NULL")
        }
        log_info("=== END DATA AVAILABILITY CHECK ===")

        # --- PROGRESS BAR LOGIC ---

        # 1. Define a helper function to update the bar
        update_progress <- function(value, text = NULL) {
          shinyjs::runjs(sprintf(
            "document.getElementById('%s').style.width = '%d%%';",
            ns("progress_bar_inner"), value
          ))
          if (!is.null(text)) {
            shinyjs::html(ns("progress_label"), text)
          }
        }

        # 2. Show the progress container
        shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_analysis_container")))

        # 3. Update progress through the analysis steps
        update_progress(10, "Initializing...")
        rv_status$type <- "info"
        rv_status$message <- "Analysis in progress..."

        # Clear any existing status messages to avoid interference
        shinyjs::delay(100, {
          rv_status$type <- "info"
          rv_status$message <- "Analysis in progress..."
        })

        # Run the appropriate analysis based on selection
        switch(input$analysis_type,
          "alignment" = {
            log_info("Running alignment analysis...")
            # Get the data reactively
            data <- alignment_data()

            # Check if data is available
            if (is.null(data)) {
              log_error("No alignment data available for analysis")
              rv_status$type <- "danger"
              rv_status$message <- "ERROR: Please load alignment data in the 'Load & Clean Data' tab first"
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              return()
            }

            # Validate the data
            if (!is.data.frame(data)) {
              msg <- glue::glue("alignment_data() returned non-data.frame: {class(data)}")
              log_error(msg)
              rv_status$type <- "danger"
              rv_status$message <- paste("ERROR:", msg)
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              return()
            }

            if (nrow(data) == 0) {
              log_warn("alignment_data() returned empty data.frame")
              rv_status$type <- "warning"
              rv_status$message <- "WARNING: No data rows available for analysis"
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              return()
            }

            # Run the analysis
            update_progress(30, "Reading alignment data...")
            update_progress(50, "Analyzing alignment...")

            tryCatch(
              {
                result <- centrimpact::analyze_alignment(data)
                rv_analysis$alignment_medians <- result$alignment_medians
                rv_analysis$alignment_score <- result$alignment_score
                rv_analysis$alignment_analyzed <- TRUE

                update_progress(100, "Finalizing...")

                # On success, update status and hide progress bar
                rv_status$type <- "success"
                rv_status$message <- "Alignment analysis complete! You can now explore the results in the panels below."
                log_info("Success status set: type={rv_status$type}, message={rv_status$message}")

                # Hide progress bar immediately
                shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
                update_progress(0) # Reset for next time
              },
              error = function(e) {
                log_error("Error during alignment analysis: {conditionMessage(e)}")

                # On error, update status and hide progress bar
                rv_status$type <- "danger"
                rv_status$message <- paste("An error occurred during alignment analysis:", conditionMessage(e))
                shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              }
            )
          },
          "dynamics" = {
            log_info("Running dynamics analysis...")
            # Get the data reactively
            data <- dynamics_data()

            # Check if data is available
            if (is.null(data)) {
              log_error("No dynamics data available for analysis")
              rv_status$type <- "danger"
              rv_status$message <- "ERROR: Please load dynamics data in the 'Load & Clean Data' tab first"
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              return()
            }

            # Validate the data
            if (!is.data.frame(data)) {
              msg <- glue::glue("dynamics_data() returned non-data.frame: {class(data)}")
              log_error(msg)
              rv_status$type <- "danger"
              rv_status$message <- paste("ERROR:", msg)
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              return()
            }

            if (nrow(data) == 0) {
              log_warn("dynamics_data() returned empty data.frame")
              rv_status$type <- "warning"
              rv_status$message <- "WARNING: No data rows available for analysis"
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              return()
            }

            # Run the analysis
            update_progress(30, "Reading dynamics data...")
            update_progress(50, "Analyzing dynamics...")

            tryCatch(
              {
                result <- centrimpact::analyze_dynamics(data)
                log_info("=== DYNAMICS ANALYSIS RESULT ===")
                log_info("Result structure: {paste(names(result), collapse = ', ')}")
                log_info("domain_df type: {class(result$domain_df)}")
                log_info("domain_df columns: {paste(names(result$domain_df), collapse = ', ')}")
                log_info("domain_df rows: {nrow(result$domain_df)}")
                log_info("domain_df structure:")
                # Use a safer way to log the structure
                tryCatch({
                  structure_output <- capture.output(str(result$domain_df))
                  for (line in structure_output) {
                    log_info(line)
                  }
                }, error = function(e) {
                  log_warn("Could not log domain_df structure: {e$message}")
                })
                
                rv_analysis$dynamics_results <- result$domain_df
                rv_analysis$dynamics_score <- result$dynamics_score
                rv_analysis$dynamics_analyzed <- TRUE
                rv_analysis$full_results <- data # Store original data for full table

                update_progress(100, "Finalizing...")

                # On success, update status and hide progress bar
                rv_status$type <- "success"
                rv_status$message <- "Dynamics analysis complete! You can now explore the results in the panels below."
                log_info("Success status set: type={rv_status$type}, message={rv_status$message}")

                # Hide progress bar immediately
                shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
                update_progress(0) # Reset for next time
              },
              error = function(e) {
                log_error("Error during dynamics analysis: {conditionMessage(e)}")

                # On error, update status and hide progress bar
                rv_status$type <- "danger"
                rv_status$message <- paste("An error occurred during dynamics analysis:", conditionMessage(e))
                shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              }
            )
          },
          "cascade" = {
            log_info("Running cascade analysis...")
            # Get the data reactively
            data <- cascade_data()

            # Check if data is available
            if (is.null(data)) {
              log_error("No cascade data available for analysis")
              rv_status$type <- "danger"
              rv_status$message <- "ERROR: Please load cascade data in the 'Load & Clean Data' tab first"
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              return()
            }

            # Validate the data
            if (!is.data.frame(data)) {
              msg <- glue::glue("cascade_data() returned non-data.frame: {class(data)}")
              log_error(msg)
              rv_status$type <- "danger"
              rv_status$message <- paste("ERROR:", msg)
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              return()
            }

            if (nrow(data) == 0) {
              log_warn("cascade_data() returned empty data.frame")
              rv_status$type <- "warning"
              rv_status$message <- "WARNING: No data rows available for analysis"
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              return()
            }

            # Run the analysis
            update_progress(30, "Reading cascade data...")
            update_progress(50, "Analyzing cascade effects...")

            tryCatch(
              {
                result <- centrimpact::analyze_cascade(data)
                rv_analysis$cascade_results <- result$cascade_df
                rv_analysis$cascade_score <- result$cascade_score
                rv_analysis$cascade_analyzed <- TRUE

                update_progress(100, "Finalizing...")

                # On success, update status and hide progress bar
                rv_status$type <- "success"
                rv_status$message <- "Cascade analysis complete! You can now explore the results in the panels below."
                log_info("Success status set: type={rv_status$type}, message={rv_status$message}")

                # Hide progress bar immediately
                shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
                update_progress(0) # Reset for next time
              },
              error = function(e) {
                log_error("Error during cascade analysis: {conditionMessage(e)}")

                # On error, update status and hide progress bar
                rv_status$type <- "danger"
                rv_status$message <- paste("An error occurred during cascade analysis:", conditionMessage(e))
                shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              }
            )
          },
          "full" = {
            log_info("Running full analysis...")
            # This will run all analyses in sequence

            tryCatch(
              {
                # Run alignment analysis
                update_progress(10, "Starting alignment analysis...")
                data <- alignment_data()
                if (is.null(data)) stop("No alignment data available")

                update_progress(20, "Running alignment analysis...")
                result <- centrimpact::analyze_alignment(data)
                rv_analysis$alignment_medians <- result$alignment_medians
                rv_analysis$alignment_score <- result$alignment_score
                rv_analysis$alignment_analyzed <- TRUE

                # Run dynamics analysis
                update_progress(40, "Starting dynamics analysis...")
                data <- dynamics_data()
                if (is.null(data)) stop("No dynamics data available")

                update_progress(50, "Running dynamics analysis...")
                result <- centrimpact::analyze_dynamics(data)
                rv_analysis$dynamics_results <- result$domain_df
                rv_analysis$dynamics_score <- result$dynamics_score
                rv_analysis$dynamics_analyzed <- TRUE
                rv_analysis$full_results <- data # Store original data for full table

                # Run cascade analysis
                update_progress(70, "Starting cascade analysis...")
                data <- cascade_data()
                if (is.null(data)) stop("No cascade data available")

                update_progress(80, "Running cascade analysis...")
                result <- centrimpact::analyze_cascade(data)
                rv_analysis$cascade_results <- result$cascade_df
                rv_analysis$cascade_score <- result$cascade_score
                rv_analysis$cascade_analyzed <- TRUE

                update_progress(100, "Finalizing...")

                # On success, update status and hide progress bar
                rv_status$type <- "success"
                rv_status$message <- "Full analysis complete! You can now explore all results in the panels below."
                log_info("Success status set: type={rv_status$type}, message={rv_status$message}")

                # Hide progress bar immediately
                shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
                update_progress(0) # Reset for next time
              },
              error = function(e) {
                log_error("Error during full analysis: {conditionMessage(e)}")

                # On error, update status and hide progress bar
                rv_status$type <- "danger"
                rv_status$message <- paste("An error occurred during full analysis:", conditionMessage(e))
                shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
              }
            )
          },
          {
            log_error("Unknown analysis type: {input$analysis_type}")
            rv_status$type <- "danger"
            rv_status$message <- "Error: Unknown analysis type selected"
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_analysis_container")))
          }
        )
      })



      get_level_info <- function(score, type = "domain") {
        if (score >= 0.8) {
          list(
            color = "#4E342E",        # Rich Espresso
            level = "Highly Developed",
            bg_color = "#ECE7E2"      # Lightened coffee cream
          )
        } else if (score >= 0.6) {
          list(
            color = "#A64B42",        # Rust Red
            level = "Well Developed",
            bg_color = "#F7EDEB"
          )
        } else if (score >= 0.4) {
          list(
            color = "#BC7A5A",        # Muted Terracotta
            level = "Developing",
            bg_color = "#FAF1ED"
          )
        } else if (score >= 0.2) {
          list(
            color = "#3F5E78",        # Denim Blue
            level = "Emerging",
            bg_color = "#EFF2F5"
          )
        } else {
          list(
            color = "#6B6459",        # Stone Grey-Brown
            level = "Initial Stage",
            bg_color = "#F3F1EE"
          )
        }
      }
      

      create_score_value_box <- function(title, score, type, icon_choice = ph_i("circle")) {
        level_info <- get_level_info(score, type)

        # Create custom theme for this specific color
        custom_theme <- bs_theme(
          bg = level_info$bg_color,
          fg = "#2A2A2A", # Your default text color
          primary = level_info$color
        )

        value_box(
          title = tags$div(title, style = "font-family: var(--bs-font-code); font-size: 1.1rem; text-transform: uppercase;"),
          value = tags$div(format(round(score, 2), nsmall = 2), style = "font-family: var(--bs-font-code); font-size: 2.5rem;"),
          showcase = icon_choice,
          theme = value_box_theme(bg = level_info$bg_color, fg = "#2A2A2A"),
          class = paste0("score-", type),
          tags$div(
            style = "font-family: var(--bs-font-code); font-style: italic; font-size: 0.8rem; color: #6B6459;",
            level_info$level
          ),
          # Custom styling that works with your color scheme
          style = paste0(
            "border-left: 4px solid ", level_info$color, "; ",
            "background-color: ", level_info$bg_color, "; ",
            "color: #2A2A2A;"
          )
        )
      }

      # Helper function to get alignment domain scores
      get_alignment_domain_score <- function(domain_name, alignment_medians) {
        # Debug logging
        log_info("=== DEBUG: get_alignment_domain_score called for domain: {domain_name} ===")
        log_info("alignment_medians type: {class(alignment_medians)}")
        log_info("alignment_medians is null: {is.null(alignment_medians)}")
        
        if (is.null(alignment_medians) || !is.data.frame(alignment_medians)) {
          log_warn("alignment_medians is null or not a data frame")
          return(0.0)  # Default fallback value
        }
        
        log_info("alignment_medians columns: {paste(names(alignment_medians), collapse = ', ')}")
        log_info("alignment_medians rows: {nrow(alignment_medians)}")
        
        # Try to find the domain score in the alignment_medians
        # Look for the domain name in character columns
        for (col in names(alignment_medians)) {
          if (is.character(alignment_medians[[col]]) && 
              any(grepl(domain_name, alignment_medians[[col]], ignore.case = TRUE))) {
            log_info("Found domain name in column '{col}'")
            # Find the corresponding numeric value
            numeric_cols <- names(alignment_medians)[sapply(alignment_medians, is.numeric)]
            log_info("Numeric columns: {paste(numeric_cols, collapse = ', ')}")
            
            if (length(numeric_cols) > 0) {
              row_idx <- which(grepl(domain_name, alignment_medians[[col]], ignore.case = TRUE))[1]
              if (!is.na(row_idx)) {
                score_val <- as.numeric(alignment_medians[row_idx, numeric_cols[1]])
                log_info("Found score in row {row_idx}, column '{numeric_cols[1]}': {score_val}")
                return(score_val)
              }
            }
          }
        }
        
        # Fallback: return 0.0 if we can't find the score
        log_warn("Could not find score for domain '{domain_name}', returning 0.0")
        return(0.0)
      }

      # Render alignment analysis UI
      output$alignment_ui <- renderUI({
        req(rv_analysis$alignment_analyzed, rv_analysis$alignment_score)

        tagList(
          # Score display
          fluidRow(
            column(width = 1),
            column(
              width = 6,
              create_score_value_box(
                title = "Alignment Analysis Score",
                score = rv_analysis$alignment_score,
                type = "alignment",
                icon_choice = ph_i("flower-lotus", weight = "bold", size = "4x")
              )
            ),
            column(
              width = 4,
              bslib::card(
                bslib::card_body(
                "This score represents the overall alignment between researchers and partners, calculated based on the analysis of various alignment domains.
                It reflects the degree of agreement and shared understanding between different stakeholder groups."
              )
              )
              ),
            column(width = 1)
          ),
          fluidRow(
            column(1),
            column(
              width = 5,
              create_score_value_box(
                "Goals and Purposes",
                get_alignment_domain_score("Purposes", rv_analysis$alignment_medians),
                "domain",
                icon_choice = ph_i("compass", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 5,
              create_score_value_box(
                "Values and Ideals",
                get_alignment_domain_score("Ideals", rv_analysis$alignment_medians),
                "domain",
                icon_choice = ph_i("lighthouse", weight = "bold", size = "3x")
              )
            ),
            column(width = 1)
          ),
          fluidRow(
            column(
              width = 3,
              create_score_value_box(
                "Roles and Responsibilities",
                get_alignment_domain_score("Responsibility", rv_analysis$alignment_medians),
                "domain",
                icon_choice = ph_i("user-switch", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 3,
              create_score_value_box(
                "Resources",
                get_alignment_domain_score("Resources", rv_analysis$alignment_medians),
                "domain",
                icon_choice = ph_i("package", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 3,
              create_score_value_box(
                "Activities and Events",
                get_alignment_domain_score("Events", rv_analysis$alignment_medians),
                "domain",
                icon_choice = ph_i("calendar-check", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 3,
              create_score_value_box(
                "Empowerment",
                get_alignment_domain_score("Empowerment", rv_analysis$alignment_medians),
                "domain",
                icon_choice = ph_i("fist", weight = "bold", size = "3x")
              )
            )
        ),
          fluidRow(
            column(width = 1),
            column(
              width = 5,
              create_score_value_box(
                "Outputs",
                get_alignment_domain_score("Outputs", rv_analysis$alignment_medians),
                "domain",
                icon_choice = ph_i("file-text", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 5,
              create_score_value_box(
                "Outcomes",
                get_alignment_domain_score("Outcomes", rv_analysis$alignment_medians),
                "domain",
                icon_choice = ph_i("trend-up", weight = "bold", size = "3x")
              )
            ),
            column(width = 2)  # Empty space for balance
          ),

          # Full alignment results table
          fluidRow(
            column(width = 1),
            column(
              width = 10,
                bslib::card(
                  bslib::card_body(
                    div(
                      DT::dataTableOutput(ns("full_alignment_table"))
                ),
                div(
                  style = "text-align: center;",
                  downloadButton(ns("download_alignment"),
                                 "Download Full Alignment Data",
                                 class = "btn-primary",
                  )
                )
              )
            )
          ),
          column(width = 1)
        )
        )
      })

      # Render dynamics analysis UI
      output$dynamics_ui <- renderUI({
        req(rv_analysis$dynamics_analyzed, rv_analysis$dynamics_results)

        tagList(
          # Score display
          fluidRow(
            column(width = 1),
            column(
              width = 6,
              create_score_value_box(
                title = "Dynamics Analysis Score",
                score = rv_analysis$dynamics_score,
                type = "dynamics",
                icon_choice = ph_i("pulse", weight = "bold", size = "4x")
              )
            ),
            column(
              width = 4,
              bslib::card(
                bslib::card_body(
                "This score represents the overall dynamics of your project, calculated based on the analysis of various domains.
                It reflects the project's health and potential for impact as well as sustainability."
              )
              )
              ),
            column(width = 1)
          ),
          fluidRow(
            column(1),
            column(
              width = 2,
              create_score_value_box(
                "Contexts",
                get_domain_score("Contexts", rv_analysis$dynamics_results),
                "domain",
                icon_choice = ph_i("polygon", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 2,
              create_score_value_box(
                "Partnership Processes",
                get_domain_score("Partnership Processes", rv_analysis$dynamics_results),
                "domain",
                icon_choice = ph_i("handshake", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 2,
              create_score_value_box(
                "Interventions and Research",
                get_domain_score("Interventions and Research", rv_analysis$dynamics_results),
                "domain",
                icon_choice = ph_i("hand-heart", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 2,
              create_score_value_box(
                "Engaged Learning",
                get_domain_score("Engaged Learning", rv_analysis$dynamics_results),
                "domain",
                icon_choice = ph_i("book-open-user", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 2,
              create_score_value_box(
                "Outcomes",
                get_domain_score("Outcomes", rv_analysis$dynamics_results),
                "domain",
                icon_choice = ph_i("chart-line-up", weight = "bold", size = "3x")
              )
            ),
            column(1)
          ),

          # Full dynamics results table
          fluidRow(
            column(width = 1),
            column(
              width = 10,
                bslib::card(
                  bslib::card_body(
                    div(
                      DT::dataTableOutput(ns("full_dynamics_table"))
                ),
                div(
                  style = "text-align: center;",
                  downloadButton(ns("download_dynamics"),
                                 "Download Full Dynamics Data",
                                 class = "btn-primary",
                  )
                )
              )
            )
          ),
          column(width = 1)
        )
        )
      })

      # Render cascade analysis UI
      output$cascade_ui <- renderUI({
        req(rv_analysis$cascade_analyzed, rv_analysis$cascade_results)

        # Helper to calculate means safely
        safe_mean <- function(df, col) {
          if (!is.data.frame(df) || !(col %in% names(df))) return(NA_real_)
          vals <- suppressWarnings(as.numeric(df[[col]]))
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) return(NA_real_)
          mean(vals)
        }

        # Calculate means for the five metrics
        cascade_df <- rv_analysis$cascade_results
        mean_layer_score     <- safe_mean(cascade_df, "layer_score")
        mean_layer_knitting  <- safe_mean(cascade_df, "layer_knitting")
        mean_layer_bridging  <- safe_mean(cascade_df, "layer_bridging")
        mean_layer_channeling<- safe_mean(cascade_df, "layer_channeling")
        mean_layer_reaching  <- safe_mean(cascade_df, "layer_reaching")

        tagList(
          # Score display (main value box)
          fluidRow(
            column(width = 1),
            column(
              width = 6,
              create_score_value_box(
                title = "Cascade Effects Score",
                score = rv_analysis$cascade_score,
                type = "cascade",
                icon_choice = ph_i("waveform", weight = "bold", size = "4x")
              )
            ),
            column(
              width = 4,
              bslib::card(
                bslib::card_body(
                  "This score represents the overall cascade connectivity in your project, calculated from the network structure and layer metrics. Higher scores indicate more robust and interconnected project layers."
                )
              )
            ),
            column(width = 1)
          ),
          # Five value boxes for layer metrics
          fluidRow(
            column(1),
            column(
              width = 2,
              create_score_value_box(
                "Mean Layer Score",
                mean_layer_score,
                "domain",
                icon_choice = ph_i("stack", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 2,
              create_score_value_box(
                "Mean Knitting",
                mean_layer_knitting,
                "domain",
                icon_choice = ph_i("link", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 2,
              create_score_value_box(
                "Mean Bridging",
                mean_layer_bridging,
                "domain",
                icon_choice = ph_i("bridge", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 2,
              create_score_value_box(
                "Mean Channeling",
                mean_layer_channeling,
                "domain",
                icon_choice = ph_i("arrow-bend-double-up-right", weight = "bold", size = "3x")
              )
            ),
            column(
              width = 2,
              create_score_value_box(
                "Mean Reaching",
                mean_layer_reaching,
                "domain",
                icon_choice = ph_i("arrows-out", weight = "bold", size = "3x")
              )
            ),
            column(1)
          ),
          # Cascade results table and summary
          fluidRow(
            column(width = 1),
            column(
              width = 10,
              bslib::card(
                bslib::card_body(
                  div(
                    DT::dataTableOutput(ns("cascade_results_table"))
                  ),
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    downloadButton(ns("download_cascade_results"), "Download Cascade Results", class = "btn-primary")
                  )
                )
              )
            ),
            column(width = 1)
          )
        )
      })



      # Render cascade score box
      output$cascade_score_box <- renderUI({
        req(rv_analysis$cascade_analyzed, rv_analysis$cascade_score)

        score <- tryCatch(
          as.numeric(rv_analysis$cascade_score),
          error = function(e) NA_real_
        )

        if (is.na(score)) {
          return(div(
            class = "alert alert-warning",
            "Could not calculate cascade score"
          ))
        }

        # Get score style based on 5-point scale
        score_style <- get_score_style(score, scale = 5)
        score_color <- score_style$color

        # Define connectivity level text
        connectivity_text <- if (score >= 0.8) {
          "Very highly connected"
        } else if (score >= 0.6) {
          "Highly connected"
        } else if (score >= 0.4) {
          "Moderately connected"
        } else if (score >= 0.2) {
          "Low connectivity"
        } else {
          "Very low connectivity"
        }

        bslib::card(
          style = "flex: 1; display: flex; flex-direction: column; margin: 0;",
          bslib::card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                div(
                  style = "display: flex; align-items: center; gap: 0.5em;",
                  tags$i(phosphoricons::ph("waveform", weight = "light"),
                    style = "font-size: 1em; color: inherit;"
                  ),
                  "Cascade Effects Score"
                )
              )
            )
          ),
          bslib::card_body(
            style = "flex: 1; display: flex; align-items: center; justify-content: center;",
            div(
              class = "d-flex flex-column align-items-center gap-2",
              div(
                class = "d-flex align-items-center gap-3",
                style = paste0(
                  "font-family: 'Jersey 20';
                font-size: 4rem;
                padding: 0.5rem 1rem;
                border-radius: 0.5rem;
                color: white;
                background-color: ", score_color, ";"
                ),
                tags$i(phosphoricons::ph("waveform", weight = "light")),
                format(round(score, 2), nsmall = 2)
              ),
              span(
                style = paste0(
                  "font-size: 1.25rem;
                font-family: 'Jersey 20';
                text-transform: uppercase;
                color: ", score_color, ";"
                ),
                connectivity_text
              )
            )
          )
        )
      })

      # Render cascade results table as a DataTable with scores by layer
      output$cascade_results_table <- DT::renderDataTable({
        req(rv_analysis$cascade_analyzed, rv_analysis$cascade_results)

        if (is.data.frame(rv_analysis$cascade_results)) {
          # Show the main cascade results table (scores by layer)
          DT::datatable(
            rv_analysis$cascade_results,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = "frtip"
            ),
            rownames = FALSE
          )
        } else {
          DT::datatable(
            data.frame(Message = "No cascade results available"),
            rownames = FALSE
          )
        }
      })

      # Render cascade summary output
      output$cascade_summary_output <- renderPrint({
        req(rv_analysis$cascade_analyzed, rv_analysis$cascade_results)

        cat("Cascade Effects Analysis Summary\n")
        cat("==============================\n\n")

        if (is.data.frame(rv_analysis$cascade_results)) {
          # Get summary of cascade results
          cascade_results <- rv_analysis$cascade_results

          # Calculate basic statistics for numeric columns
          numeric_cols <- sapply(cascade_results, is.numeric)
          if (any(numeric_cols)) {
            cat("Numeric Columns Summary:\n")
            print(summary(cascade_results[, numeric_cols, drop = FALSE]))
          }

          # Add overall score
          if (!is.null(rv_analysis$cascade_score)) {
            cat(
              "\nOverall Cascade Score: ",
              round(as.numeric(rv_analysis$cascade_score), 2), "\n"
            )
          }
        } else {
          cat("No cascade data available for summary")
        }
      })

      # Download handler for cascade results
      output$download_cascade_results <- downloadHandler(
        filename = function() {
          paste0("cascade_results_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
          write.csv(rv_analysis$cascade_results, file, row.names = FALSE)
        }
      )

      # Output value for dynamics_analyzed
      output$dynamics_analyzed <- reactive({
        isTRUE(rv_analysis$dynamics_analyzed)
      })
      outputOptions(output, "dynamics_analyzed", suspendWhenHidden = FALSE)

      # Output value for cascade_analyzed
      output$cascade_analyzed <- reactive({
        isTRUE(rv_analysis$cascade_analyzed)
      })
      outputOptions(output, "cascade_analyzed", suspendWhenHidden = FALSE)

      # Render dynamics score box
      output$dynamics_score_box <- renderUI({
        req(rv_analysis$dynamics_analyzed, rv_analysis$dynamics_score)

        score <- tryCatch(
          as.numeric(rv_analysis$dynamics_score),
          error = function(e) NA_real_
        )

        if (is.na(score)) {
          return(div(
            class = "alert alert-warning",
            "Could not calculate dynamics score"
          ))
        }

        # Get score style based on 4-point scale
        score_style <- get_score_style(score, scale = 4)
        score_level <- score_style$level
        score_color <- score_style$color

        # Define balance level text
        balance_text <- if (score >= 0.75) {
          "Very High Balance"
        } else if (score >= 0.5) {
          "High Balance"
        } else if (score >= 0.25) {
          "Low Balance"
        } else {
          "Very Low Balance"
        }

        bslib::card(
          style = "flex: 1; display: flex; flex-direction: column; margin: 0;",
          bslib::card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                div(
                  style = "display: flex; align-items: center; gap: 0.5em;",
                  tags$i(phosphoricons::ph("pulse", weight = "light"),
                    style = "font-size: 1em; color: inherit;"
                  ),
                  "Dynamics Score"
                ),
                bslib::popover(
                  tags$span(
                    tags$i(
                      phosphoricons::ph("info", weight = "light"),
                      style = "font-size: 1em; color: #6c757d; cursor: pointer;"
                    )
                  ),
                  title = "Interpreting Dynamics Scores",
                  div(
                    style = "max-width: 400px;",
                    p("The dynamics score indicates the balance and alignment of project dynamics across different domains."),
                    p("Scores range from 0 to 1, with higher scores indicating better balance.")
                  ),
                  placement = "right"
                )
              )
            )
          ),
          bslib::card_body(
            style = "flex: 1; display: flex; align-items: center; justify-content: center;",
            div(
              class = "d-flex flex-column align-items-center gap-2",
              div(
                class = "d-flex align-items-center gap-3",
                style = paste0(
                  "font-family: 'Jersey 20';
                font-size: 4rem;
                padding: 0.5rem 1rem;
                border-radius: 0.5rem;
                color: white;
                background-color: ", score_color, ";"
                ),
                tags$i(phosphoricons::ph("pulse", weight = "light")),
                format(round(score, 2), nsmall = 2)
              ),
              span(
                style = paste0(
                  "font-size: 1.25rem;
                font-family: 'Jersey 20';
                text-transform: uppercase;
                color: ", score_color, ";"
                ),
                balance_text
              )
            )
          )
        )
      })

      # Render domain scores table
      output$domain_scores_table <- renderTable(
        {
          req(rv_analysis$dynamics_results)

          if (is.data.frame(rv_analysis$dynamics_results)) {
            # Format the domain scores nicely
            domain_scores <- rv_analysis$dynamics_results

            # Round numeric columns to 2 decimal places
            numeric_cols <- sapply(domain_scores, is.numeric)
            if (any(numeric_cols)) {
              domain_scores[numeric_cols] <- round(domain_scores[numeric_cols], 2)
            }

            domain_scores
          } else {
            data.frame(Message = "No domain scores available")
          }
        },
        rownames = FALSE,
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE
      )

      # Render dynamics summary output
      output$dynamics_summary_output <- renderPrint({
        req(rv_analysis$dynamics_results)

        cat("Dynamics Analysis Summary\n")
        cat("========================\n\n")

        if (is.data.frame(rv_analysis$dynamics_results)) {
          # Get summary of domain scores
          domain_scores <- rv_analysis$dynamics_results

          # Calculate basic statistics
          cat("Domain Score Statistics:\n")
          print(summary(domain_scores[, sapply(domain_scores, is.numeric), drop = FALSE]))

          # Add overall score
          cat(
            "\nOverall Dynamics Score: ",
            round(as.numeric(rv_analysis$dynamics_score), 2), "\n"
          )
        } else {
          cat("No dynamics data available for summary")
        }
      })

      # Render full dynamics results table
      output$full_dynamics_table <- DT::renderDataTable({
        req(rv_analysis$dynamics_analyzed)

        if (is.data.frame(rv_analysis$full_results)) {
          DT::datatable(
            rv_analysis$full_results,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = "frtip"
            ),
            rownames = FALSE
          )
        } else {
          # Fallback to dynamics_results if full_results is not available
          if (is.data.frame(rv_analysis$dynamics_results)) {
            DT::datatable(
              rv_analysis$dynamics_results,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = "frtip"
              ),
              rownames = FALSE
            )
          } else {
            # Show empty table with message
            DT::datatable(
              data.frame(Message = "No dynamics data available"),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = "frtip"
              ),
              rownames = FALSE
            )
          }
        }
      })

      # Download handler for full dynamics data
      output$download_dynamics <- downloadHandler(
        filename = function() {
          paste0("dynamics_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
          req(rv_analysis$dynamics_analyzed)
          
          # Use full_results if available, otherwise fall back to dynamics_results
          data_to_download <- if (is.data.frame(rv_analysis$full_results)) {
            rv_analysis$full_results
          } else if (is.data.frame(rv_analysis$dynamics_results)) {
            rv_analysis$dynamics_results
          } else {
            data.frame(Message = "No dynamics data available")
          }
          
          write.csv(data_to_download, file, row.names = FALSE)
        },
        contentType = "text/csv"
      )

      # Download handler for domain scores
      output$download_domain_scores <- downloadHandler(
        filename = function() {
          paste0("domain_scores_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
          write.csv(rv_analysis$dynamics_results, file, row.names = FALSE)
        }
      )

      # Helper function to extract domain scores from dynamics_results
      get_domain_score <- function(domain_name, dynamics_results) {
        # Debug logging
        log_info("=== DEBUG: get_domain_score called for domain: {domain_name} ===")
        log_info("dynamics_results type: {class(dynamics_results)}")
        log_info("dynamics_results is null: {is.null(dynamics_results)}")
        
        if (is.null(dynamics_results) || !is.data.frame(dynamics_results)) {
          log_warn("dynamics_results is null or not a data frame")
          return(0.0)  # Default fallback value
        }
        
        log_info("dynamics_results columns: {paste(names(dynamics_results), collapse = ', ')}")
        log_info("dynamics_results rows: {nrow(dynamics_results)}")
        log_info("dynamics_results structure:")
        # Use a safer way to log the structure
        tryCatch({
          structure_output <- capture.output(str(dynamics_results))
          for (line in structure_output) {
            log_info(line)
          }
        }, error = function(e) {
          log_warn("Could not log dynamics_results structure: {e$message}")
        })
        
        # Try to find the domain score in the dynamics_results
        # The structure might vary, so we'll try different approaches
        if ("domain" %in% names(dynamics_results)) {
          log_info("Found 'domain' column, filtering by domain name")
          # If there's a domain column, filter by domain name
          domain_data <- dynamics_results[dynamics_results$domain == domain_name, ]
          log_info("Filtered domain_data rows: {nrow(domain_data)}")
          
          if (nrow(domain_data) > 0) {
            # Look for a score column (could be named differently)
            score_cols <- c("domain_score", "score", "value", "median", "mean", "weight")
            for (col in score_cols) {
              if (col %in% names(domain_data) && is.numeric(domain_data[[col]])) {
                score_val <- as.numeric(domain_data[[col]][1])
                log_info("Found score in column '{col}': {score_val}")
                return(score_val)
              }
            }
            log_warn("No score columns found in domain_data")
          } else {
            log_warn("No rows found for domain '{domain_name}'")
          }
        } else {
          log_info("No 'domain' column found")
        }
        
        # If no domain column, try to find by row names or first column
        if (nrow(dynamics_results) > 0) {
          log_info("Trying to find domain name in character columns")
          # Check if the domain name appears in any column
          for (col in names(dynamics_results)) {
            if (is.character(dynamics_results[[col]]) && 
                any(grepl(domain_name, dynamics_results[[col]], ignore.case = TRUE))) {
              log_info("Found domain name in column '{col}'")
              # Find the corresponding numeric value
              numeric_cols <- names(dynamics_results)[sapply(dynamics_results, is.numeric)]
              log_info("Numeric columns: {paste(numeric_cols, collapse = ', ')}")
              
              if (length(numeric_cols) > 0) {
                row_idx <- which(grepl(domain_name, dynamics_results[[col]], ignore.case = TRUE))[1]
                if (!is.na(row_idx)) {
                  score_val <- as.numeric(dynamics_results[row_idx, numeric_cols[1]])
                  log_info("Found score in row {row_idx}, column '{numeric_cols[1]}': {score_val}")
                  return(score_val)
                }
              }
            }
          }
          log_warn("Domain name '{domain_name}' not found in any character column")
        }
        
        # Fallback: return 0.0 if we can't find the score
        log_warn("Could not find score for domain '{domain_name}', returning 0.0")
        return(0.0)
      }

      # Helper function to get score box colors and levels
      get_score_style <- function(score, scale = 5) {
        if (scale == 5) {
          # 5-point scale colors and levels
          if (is.na(score)) {
            return(list(color = "#6c757d", level = "N/A"))
          }

          if (score >= 0.8) {
            list(color = "#7A6248", level = "Very High")
          } else if (score >= 0.6) {
            list(color = "#96795F", level = "High")
          } else if (score >= 0.4) {
            list(color = "#B29076", level = "Medium")
          } else if (score >= 0.2) {
            list(color = "#CEA78D", level = "Low")
          } else {
            list(color = "#EABEA4", level = "Very Low")
          }
        } else {
          # 4-point scale colors and levels
          if (is.na(score)) {
            return(list(color = "#6c757d", level = "N/A"))
          }

          if (score >= 0.75) {
            list(color = "#7A6248", level = "Very High")
          } else if (score >= 0.5) {
            list(color = "#96795F", level = "High")
          } else if (score >= 0.25) {
            list(color = "#B29076", level = "Low")
          } else {
            list(color = "#EABEA4", level = "Very Low")
          }
        }
      }

      # Render alignment score box
      output$alignment_score_box <- renderUI({
        req(rv_analysis$alignment_analyzed, rv_analysis$alignment_score)

        # Debug: Print the reactive values structure
        message("\n=== DEBUG: Alignment Score ===")
        message("Type of rv_analysis$alignment_score: ", typeof(rv_analysis$alignment_score))
        message("Class of rv_analysis$alignment_score: ", class(rv_analysis$alignment_score))
        message("Value of rv_analysis$alignment_score: ", rv_analysis$alignment_score)

        score <- tryCatch(
          {
            s <- as.numeric(rv_analysis$alignment_score)
            message("Successfully converted to numeric: ", s)
            s
          },
          error = function(e) {
            message("Error in conversion: ", e$message)
            NA_real_
          }
        )

        message("Final score value: ", score)
        message("Is score NA? ", is.na(score))
        message("Is score numeric? ", is.numeric(score))
        message("============================\n")

        if (is.na(score)) {
          return(div(
            class = "alert alert-warning",
            paste(
              "Could not calculate alignment score. Value received:",
              rv_analysis$alignment_score
            )
          ))
        }

        # Get score style based on 5-point scale
        score_style <- get_score_style(score, scale = 5)
        score_level <- score_style$level
        score_color <- score_style$color

        # Define alignment level text
        alignment_text <- if (score >= 0.8) {
          "Very High Alignment"
        } else if (score >= 0.6) {
          "High Alignment"
        } else if (score >= 0.4) {
          "Moderate Alignment"
        } else if (score >= 0.2) {
          "Low Alignment"
        } else {
          "Very Low Alignment"
        }

        bslib::card(
          style = "flex: 1; display: flex; flex-direction: column; margin: 0;",
          bslib::card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                div(
                  style = "display: flex; align-items: center; gap: 0.5em;",
                  tags$i(phosphoricons::ph("flower-lotus", weight = "light"),
                    style = "font-size: 1em; color: inherit;"
                  ),
                  "Alignment Score"
                )
              )
            )
          ),
          bslib::card_body(
            style = "flex: 1; display: flex; align-items: center; justify-content: center;",
            div(
              class = "d-flex flex-column align-items-center gap-2",
              div(
                class = "d-flex align-items-center gap-3",
                style = paste0(
                  "font-family: 'Jersey 20';
                font-size: 4rem;
                padding: 0.5rem 1rem;
                border-radius: 0.5rem;
                color: white;
                background-color: ", score_color, ";"
                ),
                tags$i(phosphoricons::ph("flower-lotus", weight = "light")),
                format(round(score, 2), nsmall = 2)
              ),
              span(
                style = paste0(
                  "font-size: 1.25rem;
                font-family: 'Jersey 20';
                text-transform: uppercase;
                color: ", score_color, ";"
                ),
                alignment_text
              )
            )
          )
        )
      })

      # Render alignment medians table
      output$alignment_medians_table <- renderTable(
        {
          req(rv_analysis$alignment_analyzed, rv_analysis$alignment_medians)

          rv_analysis$alignment_medians
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        width = "100%",
        align = NULL,
        rownames = FALSE,
        colnames = TRUE,
        digits = 2,
        na = ""
      )

      # Render alignment medians table
      output$alignment_medians_table <- renderTable(
        {
          req(rv_analysis$alignment_analyzed, rv_analysis$alignment_medians)

          rv_analysis$alignment_medians
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        width = "100%",
        align = NULL,
        rownames = FALSE,
        colnames = TRUE,
        digits = 2,
        na = ""
      )

      # Render full alignment table
      output$full_alignment_table <- DT::renderDataTable({
        req(rv_analysis$alignment_analyzed, rv_analysis$alignment_medians)

        if (is.data.frame(rv_analysis$alignment_medians)) {
          # Format the alignment medians nicely
          alignment_data <- rv_analysis$alignment_medians

          # Round numeric columns to 2 decimal places
          numeric_cols <- sapply(alignment_data, is.numeric)
          if (any(numeric_cols)) {
            alignment_data[numeric_cols] <- round(alignment_data[numeric_cols], 2)
          }

          DT::datatable(
            alignment_data,
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              dom = 'ftip'
            ),
            rownames = FALSE
          )
        } else {
          DT::datatable(
            data.frame(Message = "No alignment data available"),
            rownames = FALSE
          )
        }
      })

      # Render dynamics medians table
      output$dynamics_medians_table <- renderTable(
        {
          req(rv_analysis$dynamics_analyzed, rv_analysis$dynamics_results)

          rv_analysis$dynamics_results
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        width = "100%",
        align = NULL,
        rownames = FALSE,
        colnames = TRUE,
        digits = 2,
        na = ""
      )

      # Download handler for alignment results
      output$download_alignment <- downloadHandler(
        filename = function() {
          paste0(
            "alignment_results_", format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv"
          )
        },
        content = function(file) {
          write.csv(rv_analysis$alignment_medians, file, row.names = FALSE)
        }
      )

      # Download handler for results (legacy)
      output$download_results <- downloadHandler(
        filename = function() {
          paste0(
            "alignment_medians_", format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv"
          )
        },
        content = function(file) {
          write.csv(rv_analysis$alignment_medians, file, row.names = FALSE)
        }
      )

      # Generate summary statistics output - show ICC scores
      output$summary_statistics_output <- renderPrint({
        req(rv_analysis$alignment_analyzed, rv_analysis$icc_scores)

        cat("ICC Scores Summary\n")
        cat("=================\n\n")

        # Special handling for irr::icclist objects
        if (inherits(rv_analysis$icc_scores, "icclist")) {
          print(rv_analysis$icc_scores)
          return()
        }

        # Fallback for non-icclist objects or if printing fails
        if (is.list(rv_analysis$icc_scores)) {
          # Try to extract common ICC values
          icc_value <- tryCatch(
            {
              rv_analysis$icc_scores$value
            },
            error = function(e) NULL
          )

          if (!is.null(icc_value)) {
            cat("ICC Value:", icc_value, "\n\n")
            if (!is.null(rv_analysis$icc_scores$lbound) && !is.null(rv_analysis$icc_scores$ubound)) {
              cat(
                "95% CI:", rv_analysis$icc_scores$lbound, "to",
                rv_analysis$icc_scores$ubound, "\n"
              )
            }
            if (!is.null(rv_analysis$icc_scores$p.value)) {
              cat("p-value:", rv_analysis$icc_scores$p.value, "\n")
            }
            return()
          }
        }

        # If we get here, show the raw structure
        cat("ICC Scores (raw structure):\n")
        str(rv_analysis$icc_scores)
      })

      # Create workflow observers using centralized utility functions
      create_workflow_observers(ns_workflow, ns, session)

      # Initialize workflow icons on module load with current state
      observe({
        # Workflow icons are now updated centrally by the main server observer
        logger::log_info("Workflow icons initialization skipped - handled centrally")
      })

      # Return analysis results
      return(
        reactiveValues(
          # Alignment results
          alignment_analyzed = reactive(rv_analysis$alignment_analyzed),
          alignment_score = reactive(rv_analysis$alignment_score),
          alignment_medians = reactive(rv_analysis$alignment_medians),
          icc_scores = reactive(rv_analysis$icc_scores),

          # Dynamics results
          dynamics_analyzed = reactive(rv_analysis$dynamics_analyzed),
          dynamics_score = reactive(rv_analysis$dynamics_score),
          dynamics_results = reactive(rv_analysis$dynamics_results),
          full_dynamics_results = reactive(rv_analysis$full_results),

          # Cascade results
          cascade_analyzed = reactive(rv_analysis$cascade_analyzed),
          cascade_score = reactive(rv_analysis$cascade_score),
          cascade_results = reactive(rv_analysis$cascade_results)
        ) # Closes reactiveValues()
      ) # Closes return()
    } # Closes the 'module = function(...) {' block
  ) # Closes the 'moduleServer(...)' call
} # Closes the 'mod_analyze_server <- function(...) {' block
