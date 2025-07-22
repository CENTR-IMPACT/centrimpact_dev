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
#' @importFrom shinyWidgets radioGroupButtons progressBar
#' @importFrom utils create_status_card create_actions_card create_info_card

# Enable glue syntax for logger
.onLoad <- function(libname, pkgname) {
  logger::log_formatter(logger::formatter_glue)
}

# Modular overlay UI function
overlay_card_ui <- function(id, title, content_ui) {
  ns <- NS(id)
  uiOutput(ns("overlay_ui"))
}

# the ui function
mod_analyze_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        .overlay-card {
          position: fixed;
          top: 5vh;
          left: 50%;
          transform: translate(-50%, 0);
          z-index: 9999;
          width: 80vw;
          max-width: 1200px;
          box-shadow: 0 4px 24px rgba(0,0,0,0.3);
          background: #edeae2;
          padding: 1.5em;
          border-radius: 1em;
        }
        .overlay-bg {
          position: fixed;
          top: 0; left: 0; right: 0; bottom: 0;
          background: rgba(0,0,0,0.5);
          z-index: 9998;
        }
      "))
    ),
    bslib::accordion(
      id = ns("analyze_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "analyze_data",
        title = tagList(ph("calculator"), HTML("&nbsp;"), "Analyze Data"),
        fluidRow(
          column(
            width = 11,
            fluidRow(
              p(
                "This module allows you to analyze your project data. Select the
              type of analysis you want to perform and run the analysis."
              )
            ),
            fluidRow(
              tags$fieldset(
                class = "custom-fieldset",
                tags$legend(
                  "Select",
                  class = "custom-legend"
                ),
                shinyWidgets::radioGroupButtons(
                  inputId = ns("analysis_type"),
                  label = "ANALYSIS TYPE",
                  choices = list(
                    "All Data" = "full",
                    "Alignment Data" = "alignment",
                    "Dynamics Data" = "dynamics",
                    "Cascade Data" = "cascade"
                  ),
                  selected = NULL,
                  direction = "horizontal",
                  justified = TRUE,
                )
              )
            ),
            tags$fieldset(
              class = "custom-fieldset",
              tags$legend(
                "Analyze",
                class = "custom-legend"
              ),
              fluidRow(
                class = "d-flex justify-content-center align-items-center",
                actionButton(
                  inputId = ns("run_analysis"),
                  label = tagList(
                    "Analyze Data ",
                    ph("calculator", weight = "bold")
                  ),
                  width = "50%",
                  class = "btn btn-primary btn-lg"
                )
              ),
              fluidRow(
                class = "d-flex justify-content-center",
                div(
                  id = ns("progress_container"),
                  style = "width: 50%; visibility: hidden;",
                  shinyWidgets::progressBar(
                    id = ns("progress_bar"),
                    value = 0,
                    total = 100,
                    status = "success",
                    title = "Ready to Analyze",
                    striped = FALSE,
                    display_pct = FALSE
                  )
                )
              ),
            )
          ),
          column(
            width = 1,
            img(
              src = "analyze.png",
              alt = "Analyze Data Image",
              style = "width: auto; max-height: 7em; margin-top: 0.5em; padding-left: auto; padding-right: 1em;"
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "results",
        title = tagList(ph("flag-checkered", weight = "fill"), HTML("&nbsp;"), "Results"),
        shiny::uiOutput(ns("results_ui"))
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
    ),
    # Overlay UIs must be rendered last to ensure they overlay the entire app
    overlay_card_ui(ns("alignment_overlay"), "Analyzed Alignment Data", DT::dataTableOutput(ns("full_alignment_table_overlay"))),
    overlay_card_ui(ns("dynamics_overlay"), "Analyzed Dynamics Data", DT::dataTableOutput(ns("full_dynamics_table_overlay"))),
    overlay_card_ui(ns("cascade_overlay"), "Analyzed Cascade Data", DT::dataTableOutput(ns("cascade_results_table_overlay")))
  )
}




# Helper to safely round numeric values, fallback to 0 if NA or not numeric
safe_round <- function(x, digits = 2) {
  val <- suppressWarnings(as.numeric(x))
  if (is.na(val)) val <- 0
  round(val, digits)
}

# the server function with enhanced debugging
mod_analyze_server <- function(id, ns_project, rv_analysis) {
  message("=== mod_analyze_server CALLED with id: ", id, " ===")

  # Debug the alignment_data parameter
  cat("\n=== MODULE INITIALIZATION ===\n")
  cat("ns_project type:", class(ns_project), "\n")
  cat("Is ns_project reactive?", is.reactive(ns_project), "\n\n")

  moduleServer(
    id = id,
    module = function(input, output, session) {
      message("=== MODULE SERVER INITIALIZED ===")

      # Get the namespace for this module
      ns <- session$ns

      # Helper function for creating score value boxes
      create_score_value_box <- function(title, score, type, bgcolor = NULL, tooltip_text = NULL, icon_choice = ph_i("circle")) {
        # Ensure score is numeric, default to 0 if not
        if (is.null(score) || !is.numeric(score) || is.na(score)) {
          score <- 0
        }
        score_display <- round(score, 2)

        bslib::value_box(
          style = "
            padding: 0.25em !important;
            border-radius: 10px 0 0 10px !important;
            border: 1px 0 1px 1px !important;
            border-style: solid !important;
            border-color: #d4c5b9 !important;
            box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06) !important;",
          title = bslib::tooltip(
            tags$div(
              title,
              class = "text-uppercase fw-semibold",
              style = "font-size: 0.9rem;",
              tags$span(
                style = "cursor: help; margin-left: 0.15em;",
                ph("info", weight = "bold", style = "font-size:1.12em; color:#f5f1e8; vertical-align:middle;")
              )
            ),
            tooltip_text
          ),
          value = tags$div(
            score_display,
            class = "font-monospace",
            style = "
              font-size: 5rem;
              font-weight: 600;
              line-height: 1;
              color: #f5f1e8;"
          ),
          showcase = icon_choice,
          showcase_layout = "top right",
          theme = value_box_theme(bg = bgcolor, fg = "#f5f1e8")
        ) # closes bslib::value_box
      } # closes create_score_value_box

      #' Score Section UI
      #'
      #' Generates the two-column layout for a metric (value box/placeholder, details card, overlay button).
      #' @param data The data frame to check (e.g., rv$indicators)
      #' @param value_box_title Title for the value box
      #' @param score The score to display in the value box (usually nrow(data))
      #' @param type The type for the value box (for styling)
      #' @param bgcolor Background color for the value box
      #' @param icon_choice Icon for the value box
      #' @param input_id Input ID for the overlay button
      #' @param extra_info Extra info line in the details card (e.g., "Qualtrics Metadata Removed")
      #' @param ns Namespace function
      #' @param section_name Section name for the legend/title
      #' @param placeholder_title Title for the placeholder (if no data)
      #' @param placeholder_text Text for the placeholder (if no data)
      #' @param placeholder_icon Icon for the placeholder (if no data)
      #' @param qualtrics_metadata_removed Logical indicating if qualtrics metadata was removed.
      #' @return The inner layout (layout_columns(...)) for the metric section
      score_section_ui <- function(data, value_box_title, score, type, bgcolor, tooltip_text, icon_choice, input_id, extra_info = NULL, ns, section_name, right_content = NULL, placeholder_title, placeholder_text, placeholder_icon, qualtrics_metadata_removed = NULL) {
        # If qualtrics_metadata_removed is TRUE, override extra_info (except for Alignment)
        if (!is.null(qualtrics_metadata_removed) && isTRUE(qualtrics_metadata_removed)) {
          extra_info <- "Qualtrics Metadata Removed"
        }
        cols <- list(
          # LEFT SIDE: Value Box or Placeholder
          if (is.null(data) || nrow(data) == 0) {
            tags$div(
              class = "data-placeholder",
              style = "\n        background: #f5f0e8;\n        border: 2px dashed #d4c5b9;\n        border-radius: 8px 0 0 8px;\n        padding: 1.1rem 0.5rem;\n        text-align: center;\n        min-height: 75px;\n        display: flex;\n        align-items: center;\n        justify-content: center;",
              tags$div(
                placeholder_icon,
                tags$div(
                  style = "color: #6c5a47;",
                  tags$strong(placeholder_title,
                    style = "font-size:0.98rem; display:block; margin-bottom: 0.1rem;"
                  ),
                  tags$div(
                    style = "font-size:0.85rem; line-height:1.2;",
                    placeholder_text
                  )
                )
              )
            )
          } else {
            create_score_value_box(
              title = value_box_title,
              score = score,
              type = type,
              bgcolor = bgcolor,
              tooltip_text = tooltip_text,
              icon_choice = icon_choice
            )
          },
          # RIGHT SIDE: Details Card (if data)
          if (!is.null(data) && nrow(data) > 0) {
            tagList(
              bslib::card(
                style = "\n        padding:0 !important;\n        border-radius:0 8px 8px 0 !important;\n        border: 1px 1px 1px 0; border-style: solid; border-color: #d4c5b9 !important;\n        box-shadow: 0 2px 4px rgba(0,0,0,0.08) !important;\n        background: #f9f5f0;",
                bslib::card_header(
                  style = "background-color: #f9f5f0; border-bottom: 1px solid #e8ddd4; padding: 0.5rem 1rem;",
                  tags$div(
                    class = "d-flex align-items-center justify-content-between mb-0",
                    tags$div("Status",
                      class = "fw-medium",
                      style = "font-size:0.75rem;color: var(--bs-success);text-transform:uppercase;"
                    ),
                    tags$div(
                      "Ready for Visualization",
                      class = "fw-bold",
                      style = "font-size:1.1rem; text-transform: uppercase; color: var(--bs-success);font-family:var(--bs-font-monospace);"
                    ),
                    ph("check-circle", weight = "fill", size = "3x", style = "color: var(--bs-success);")
                  )
                ),
                bslib::card_body(
                  style = "padding:0.22rem .5rem; color: var(--bs-primary); background: var(--bs-body-bg); overflow-x: hidden;",
                  right_content
                ),
                bslib::card_footer(
                  style = "\n          background: var(--bs-body-bg);\n          border-top:1px solid #e8ddd4;\n          padding:0.5rem 1rem; text-align:center;",
                  actionButton(
                    inputId = ns(input_id),
                    label = tagList(
                      ph("table", weight = "fill", style = "margin-right:0.45rem;"),
                      "VIEW ANALYZED DATA"
                    ),
                    class = "text-uppercase fw-semibold",
                    style = paste0("background-color: #8A7A8F; color: var(--bs-body-bg); border: none;\n                  padding:0.44rem 1.1rem;font-size:0.7rem;letter-spacing:0.4px;border-radius:6px;")
                  )
                )
              )
            )
          }
        )
        do.call(layout_columns, c(list(col_widths = c(5, 7), gap = 0, style = "align-items: flex-start;"), cols))
      }

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
        wf <- ns_project$workflow$alignment
        if (!is.null(wf) && wf$stage == "Clean Data" && wf$status == "complete") {
          update_alignment_status_display("cleaned", session, ns, ns_project)
          update_alignment_workflow_icons(ns_project, session)
        }
      })

      # --- NEW: Observe workflow state for Alignment Clean Data completion and update status/icon ---
      observe({
        if (!is.null(ns_project$stage) && !is.null(ns_project$status) && !is.null(ns_project$metric)) {
          if (
            ns_project$stage == "Clean Data" &&
              ns_project$status == "complete" &&
              tolower(ns_project$metric) == "alignment"
          ) {
            update_alignment_status_display("cleaned", session, ns, ns_project)
            update_alignment_workflow_icons(ns_project, session)
          }
        }
      })


      # Reactive expressions to get data from ns_project
      alignment_data <- reactive({
        log_debug("Accessing alignment data reactively...")
        if (is.null(ns_project)) {
          log_warn("ns_project is NULL")
          return(NULL)
        }

        # Add more detailed debugging
        log_debug("ns_project contains: {paste(names(ns_project), collapse = ', ')}")

        if (is.null(ns_project$alignment_data)) {
          log_warn("No alignment_data in ns_project. Available elements: {paste(names(ns_project), collapse = ', ')}")
          return(NULL)
        }

        # Check if the data is actually a data frame
        if (!is.data.frame(ns_project$alignment_data)) {
          log_warn("alignment_data is not a data frame, it's: {class(ns_project$alignment_data)}")
          return(NULL)
        }

        log_info("Returning alignment data from reactive context: {nrow(ns_project$alignment_data)} rows, {ncol(ns_project$alignment_data)} columns")
        return(ns_project$alignment_data)
      })

      dynamics_data <- reactive({
        log_debug("Accessing dynamics data reactively...")
        if (is.null(ns_project)) {
          log_warn("ns_project is NULL")
          return(NULL)
        }

        # Add more detailed debugging
        log_debug("ns_project contains: {paste(names(ns_project), collapse = ', ')}")

        if (is.null(ns_project$dynamics_data)) {
          log_warn("No dynamics_data in ns_project. Available elements: {paste(names(ns_project), collapse = ', ')}")
          return(NULL)
        }

        # Check if the data is actually a data frame
        if (!is.data.frame(ns_project$dynamics_data)) {
          log_warn("dynamics_data is not a data frame, it's: {class(ns_project$dynamics_data)}")
          return(NULL)
        }

        log_info("Returning dynamics data from reactive context: {nrow(ns_project$dynamics_data)} rows, {ncol(ns_project$dynamics_data)} columns")
        return(ns_project$dynamics_data)
      })

      cascade_data <- reactive({
        log_debug("Accessing cascade data reactively...")

        # Safely log the structure of ns_project
        tryCatch(
          {
            log_info("ns_project contains:")
            if (!is.null(ns_project)) {
              log_info(paste("  -", names(ns_project), collapse = "\n"))
            } else {
              log_info("ns_project is NULL")
            }
          },
          error = function(e) {
            log_warn(paste("Could not log ns_project structure:", e$message))
          }
        )

        if (is.null(ns_project) || is.null(ns_project$cascade_data)) {
          log_warn("No cascade data available in ns_project")
          return(NULL)
        }

        cascade_data <- ns_project$cascade_data

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

      # Initialize reactive values for analysis results with default values
      # rv_analysis <- reactiveValues(
      #   ... fields ...
      # )
      # Log the initialization of reactive values
      log_info("Initialized rv_analysis with keys: ", 
               paste(names(reactiveValuesToList(rv_analysis)), collapse = ", "))

      # Run analysis observer - handles the unified run_analysis button
      observeEvent(input$run_analysis, {
        log_info("=== RUN ANALYSIS BUTTON CLICKED ===")
        # Show the progress container
        shinyjs::runjs(sprintf('document.getElementById("%s").style.visibility = "visible";', ns("progress_container")))
        # Ensure the animation is active for the new run
        shinyjs::removeClass(selector = paste0("#", ns("progress_bar")), class = "progress-bar-animated")
        shinyjs::addClass(selector = paste0("#", ns("progress_bar")), class = "progress-bar-striped")
        log_info("Button clicked, checking analysis type...")

        if (is.null(input$analysis_type)) {
          log_warn("No analysis type selected. Please select an analysis type first.")
          return()
        }

        log_info("Analysis type selected: {input$analysis_type}")

        # Debug: Check what data is available
        log_info("=== DATA AVAILABILITY CHECK ===")
        if (!is.null(ns_project)) {
          log_info("ns_project is available")
          log_info("Available elements in ns_project: {paste(names(ns_project), collapse = ', ')}")

          if (!is.null(ns_project$alignment_data)) {
            log_info("Alignment data available: {nrow(ns_project$alignment_data)} rows, {ncol(ns_project$alignment_data)} columns")
          } else {
            log_warn("No alignment data in ns_project")
          }

          if (!is.null(ns_project$dynamics_data)) {
            log_info("Dynamics data available: {nrow(ns_project$dynamics_data)} rows, {ncol(ns_project$dynamics_data)} columns")
          } else {
            log_warn("No dynamics data in ns_project")
          }

          if (!is.null(ns_project$cascade_data)) {
            if (is.list(ns_project$cascade_data)) {
              log_info("Cascade data available: list with elements: {paste(names(ns_project$cascade_data), collapse = ', ')}")
            } else {
              log_info("Cascade data available: {class(ns_project$cascade_data)}")
            }
          } else {
            log_warn("No cascade data in ns_project")
          }
        } else {
          log_warn("ns_project is NULL")
        }
        log_info("=== END DATA AVAILABILITY CHECK ===")

        # --- PROGRESS BAR LOGIC ---

        # 1. Define a helper function to update the bar
        update_progress <- function(value, text = NULL, status = "success") {
          shinyWidgets::updateProgressBar(
            session = session,
            id = ns("progress_bar"),
            value = value,
            status = status,
            title = if (!is.null(text)) text else NULL
          )
        }

        # Reset progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "progress_bar",
          value = 100,
          status = "success"
        )

        # Hide the progress container after a short delay
        shinyjs::delay(1000, {
          shinyjs::runjs(sprintf('document.getElementById("%s").style.visibility = "hidden";', ns("progress_container")))
        })

        # Helper function to run analysis with common error handling and progress updates
        run_analysis <- function(analysis_type, data_func, analysis_func, rv_list, progress_stages, extra_processing = NULL) {
          log_info(paste("Running", analysis_type, "analysis..."))

          # Get the data reactively
          data <- data_func()

          # Check if data is available
          if (is.null(data)) {
            log_error(paste("No", analysis_type, "data available for analysis"))
            return(FALSE)
          }

          # Validate the data
          if (!is.data.frame(data)) {
            msg <- glue::glue(paste(analysis_type, "_data() returned non-data.frame: {class(data)}"))
            log_error(msg)
            return(FALSE)
          }

          if (nrow(data) == 0) {
            log_warn(paste(analysis_type, "_data() returned empty data.frame"))
            return(FALSE)
          }

          # Update progress
          update_progress(progress_stages$start, progress_stages$start_msg)

          tryCatch(
            {
              # Run the analysis
              result <- analysis_func(data)

              # Apply any extra processing specific to the analysis type
              if (is.function(extra_processing)) {
                extra_processing(result, data)
              }

              # Store results in reactive values with detailed logging
              log_info(paste("Storing results for", analysis_type, "analysis"))
              log_info(paste("Result keys available:", paste(names(result), collapse = ", ")))
              
              # First, log the current state of the reactive values
              log_info("Current rv_analysis state:")
              log_info(paste("- alignment_analyzed:", rv_analysis$alignment_analyzed))
              log_info(paste("- dynamics_analyzed:", rv_analysis$dynamics_analyzed))
              log_info(paste("- cascade_analyzed:", rv_analysis$cascade_analyzed))
              
              # Use isolate to batch reactive updates
              isolate({
                # Store each result with the analysis_type prefix
                for (name in names(result)) {
                  key <- paste0(analysis_type, "_", name)
                  log_info(paste("Setting", key, "to:", toString(result[[name]])))
                  rv_list[[key]] <- result[[name]]
                }
                # --- NEW: Also set generic keys for UI compatibility ---
                if (analysis_type == "alignment") {
                  if ("alignment_score" %in% names(result)) rv_list$alignment_score <- result$alignment_score
                  if ("alignment_medians" %in% names(result)) rv_list$alignment_medians <- result$alignment_medians
                  if ("icc_scores" %in% names(result)) rv_list$icc_scores <- result$icc_scores
                }
                if (analysis_type == "dynamics") {
                  if ("dynamics_score" %in% names(result)) rv_list$dynamics_score <- result$dynamics_score
                  if ("domain_df" %in% names(result)) rv_list$dynamics_results <- result$domain_df
                  if ("dynamics_df" %in% names(result)) rv_list$full_results <- result$dynamics_df

                  # --- BEGIN: Dimension Scores Calculation (using ns_project$dynamics_data) ---
                  if (is.data.frame(ns_project$dynamics_data) &&
                      all(c("weight", "salience") %in% names(ns_project$dynamics_data))) {
                    library(dplyr)
                    dimension_scores <- ns_project$dynamics_data %>%
                      mutate(descriptor_score = weight * salience) %>%
                      group_by(domain, dimension) %>%
                      mutate(full_weight = descriptor_score / sum(descriptor_score, na.rm = TRUE)) %>%
                      mutate(dimension_score = sum(descriptor_score * full_weight, na.rm = TRUE) / sum(full_weight, na.rm = TRUE)) %>%
                      ungroup() %>%
                      select(domain, dimension, dimension_score) %>%
                      distinct(domain, dimension, .keep_all = TRUE)
                    rv_list$dimension_scores <- dimension_scores
                    # Debug print/log
                    message("==== [DEBUG] dimension_scores just set in mod_analyze ====")
                    print(str(dimension_scores))
                    print(head(dimension_scores))
                    if (nrow(dimension_scores) == 0) message("[DEBUG] dimension_scores is EMPTY after calculation!")
                  } else {
                    rv_list$dimension_scores <- NULL
                    message("[DEBUG] dimension_scores set to NULL (missing required columns or not a data.frame)")
                  }
                  # --- END: Dimension Scores Calculation ---
                }
                if (analysis_type == "cascade") {
                  if ("cascade_score" %in% names(result)) rv_list$cascade_score <- result$cascade_score
                  if ("cascade_df" %in% names(result)) rv_list$cascade_results <- result$cascade_df
                }
                
                # Set the analyzed flag
                analyzed_key <- paste0(analysis_type, "_analyzed")
                log_info(paste("Setting", analyzed_key, "to TRUE"))
                rv_list[[analyzed_key]] <- TRUE
                
                # Special handling for dynamics to store full results
                if (analysis_type == "dynamics") {
                  log_info("Storing full dynamics results")
                  rv_list$full_results <- data
                }
                
                # Update debug info
                rv_list$last_updated <- Sys.time()
                rv_list$last_analysis_type <- analysis_type
                
                # Log the final state of the reactive values
                log_info("Updated rv_analysis state:")
                log_info(paste("- alignment_analyzed:", rv_analysis$alignment_analyzed))
                log_info(paste("- dynamics_analyzed:", rv_analysis$dynamics_analyzed))
                log_info(paste("- cascade_analyzed:", rv_analysis$cascade_analyzed))
                log_info("All rv_analysis keys:", paste(names(reactiveValuesToList(rv_analysis)), collapse = ", "))
              })

              update_progress(progress_stages$complete, "Analysis Complete")
              shinyjs::delay(1000, {
                update_progress(0, "Ready to Analyze")
              })

              return(TRUE)
            },
            error = function(e) {
              log_error(paste("Error during", analysis_type, "analysis:", conditionMessage(e)))
              update_progress(100, "Error: Analysis Failed", status = "danger")
              return(FALSE)
            }
          )
        }

        # Special processing for dynamics analysis
        process_dynamics_result <- function(result, data) {
          # Use skip_formatter for messages that might contain special characters
          log_info(skip_formatter("=== DYNAMICS ANALYSIS RESULT ==="))
          log_info(skip_formatter(paste("Result structure:", paste(names(result), collapse = ", "))))

          # Safely log class information (can be multiple classes)
          domain_classes <- paste(class(result$domain_df), collapse = ", ")
          log_info(skip_formatter(paste("domain_df type:", domain_classes)))

          log_info(skip_formatter(paste(
            "domain_df columns:",
            paste(names(result$domain_df), collapse = ", ")
          )))
          log_info(skip_formatter(paste("domain_df rows:", nrow(result$domain_df))))
          log_info(skip_formatter("domain_df structure:"))

          tryCatch(
            {
              # Limit the structure output to avoid flooding logs
              structure_output <- capture.output(utils::str(result$domain_df, max.level = 2, list.len = 10))
              for (line in structure_output) {
                log_info(skip_formatter(line))
              }
            },
            error = function(e) {
              log_warn(paste("Could not log domain_df structure:", e$message))
            }
          )
        }

        # Run the appropriate analysis based on selection
        switch(input$analysis_type,
          "alignment" = {
            run_analysis(
              analysis_type = "alignment",
              data_func = alignment_data,
              analysis_func = centrimpact::analyze_alignment,
              rv_list = rv_analysis,
              progress_stages = list(
                start = 30,
                start_msg = "Reading alignment data...",
                analyze = 50,
                analyze_msg = "Analyzing alignment...",
                complete = 100
              )
            )
          },
          "dynamics" = {
            run_analysis(
              analysis_type = "dynamics",
              data_func = dynamics_data,
              analysis_func = centrimpact::analyze_dynamics,
              rv_list = rv_analysis,
              progress_stages = list(
                start = 30,
                start_msg = "Reading dynamics data...",
                analyze = 50,
                analyze_msg = "Analyzing dynamics...",
                complete = 100
              ),
              extra_processing = process_dynamics_result
            )
          },
          "cascade" = {
            run_analysis(
              analysis_type = "cascade",
              data_func = cascade_data,
              analysis_func = centrimpact::analyze_cascade,
              rv_list = rv_analysis,
              progress_stages = list(
                start = 30,
                start_msg = "Constructing social network model...",
                analyze = 50,
                analyze_msg = "Analyzing cascade effects...",
                complete = 100
              )
            )
          },
          "full" = {
            log_info("Running full analysis...")
            tryCatch(
              {
                # Run alignment analysis
                update_progress(10, "Starting alignment analysis...")

                # Run alignment analysis
                align_success <- run_analysis(
                  analysis_type = "alignment",
                  data_func = alignment_data,
                  analysis_func = centrimpact::analyze_alignment,
                  rv_list = rv_analysis,
                  progress_stages = list(
                    start = 20,
                    start_msg = "Running alignment analysis...",
                    analyze = 30,
                    analyze_msg = "Analyzing alignment...",
                    complete = 30
                  )
                )

                if (!align_success) stop("Alignment analysis failed")

                # Run dynamics analysis
                update_progress(40, "Starting dynamics analysis...")

                dynamics_success <- run_analysis(
                  analysis_type = "dynamics",
                  data_func = dynamics_data,
                  analysis_func = centrimpact::analyze_dynamics,
                  rv_list = rv_analysis,
                  progress_stages = list(
                    start = 50,
                    start_msg = "Running dynamics analysis...",
                    analyze = 60,
                    analyze_msg = "Analyzing dynamics...",
                    complete = 60
                  ),
                  extra_processing = process_dynamics_result
                )

                if (!dynamics_success) stop("Dynamics analysis failed")

                # Run cascade analysis
                update_progress(65, "Starting cascade analysis...")

                cascade_success <- run_analysis(
                  analysis_type = "cascade",
                  data_func = cascade_data,
                  analysis_func = centrimpact::analyze_cascade,
                  rv_list = rv_analysis,
                  progress_stages = list(
                    start = 70,
                    start_msg = "Running cascade analysis...",
                    analyze = 85,
                    analyze_msg = "Constructing social network model...",
                    complete = 90
                  )
                )

                if (!cascade_success) stop("Cascade analysis failed")

                update_progress(95, "Finalizing...")
                shinyjs::delay(250, {
                  update_progress(100, "Analysis Complete")
                  shinyjs::delay(1000, {
                    update_progress(0, "Ready to Analyze")
                  })
                })
              },
              error = function(e) {
                log_error("Error during full analysis: {conditionMessage(e)}")
                update_progress(100, "Error: Analysis Failed", status = "danger")
              }
            )
          }
        )
      })

      get_level_info <- function(score, type = "domain") {
        if (score >= 0.8) {
          list(
            color = "#4E342E", # Rich Espresso
            level = "Highly Developed",
            bg_color = "#ECE7E2" # Lightened coffee cream
          )
        } else if (score >= 0.6) {
          list(
            color = "#A64B42", # Rust Red
            level = "Well Developed",
            bg_color = "#F7EDEB"
          )
        } else if (score >= 0.4) {
          list(
            color = "#BC7A5A", # Muted Terracotta
            level = "Developing",
            bg_color = "#FAF1ED"
          )
        } else if (score >= 0.2) {
          list(
            color = "#3F5E78", # Denim Blue
            level = "Emerging",
            bg_color = "#EFF2F5"
          )
        } else {
          list(
            color = "#6B6459", # Stone Grey-Brown
            level = "Initial Stage",
            bg_color = "#F3F1EE"
          )
        }
      }


      # Helper function to get alignment domain scores
      get_alignment_domain_score <- function(domain_name, alignment_medians) {
        # Debug logging
        log_info("=== DEBUG: get_alignment_domain_score called for domain: {domain_name} ===")
        log_info("alignment_medians type: {class(alignment_medians)}")
        log_info("alignment_medians is null: {is.null(alignment_medians)}")

        if (is.null(alignment_medians) || !is.data.frame(alignment_medians)) {
          log_warn("alignment_medians is null or not a data frame")
          return(0.0) # Default fallback value
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
                score_val <- safe_round(score_val, 2)
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

      output$results_ui <- renderUI({
        # Debug: Log the current state of rv_analysis
        log_info("\n=== RENDERING RESULTS UI ===")
        log_info(paste("Last analysis type:", rv_analysis$last_analysis_type))
        log_info(paste("Last updated:", rv_analysis$last_updated))
        
        # Log detailed state of each analysis type
        log_info("\n--- Analysis Status ---")
        log_info(paste("Alignment analyzed:", rv_analysis$alignment_analyzed, "(score:", rv_analysis$alignment_score, ")"))
        log_info(paste("Dynamics analyzed: ", rv_analysis$dynamics_analyzed, "(score:", rv_analysis$dynamics_score, ")"))
        log_info(paste("Cascade analyzed:  ", rv_analysis$cascade_analyzed, "(score:", rv_analysis$cascade_score, ")"))
        
        # Log all available keys for debugging
        all_keys <- names(reactiveValuesToList(rv_analysis))
        log_info("\n--- All rv_analysis keys ---\n", paste("-", all_keys, collapse = "\n"))
        
        # Check if any analysis has been run
        if (!isTRUE(rv_analysis$alignment_analyzed) &&
          !isTRUE(rv_analysis$dynamics_analyzed) &&
          !isTRUE(rv_analysis$cascade_analyzed)) {
          log_info("No analysis results found, showing placeholder")
          return(
            tags$div(
              class = "data-placeholder",
              tags$div(
                class = "d-flex align-items-center justify-content-center gap-2",
                ph("warning-circle", weight = "bold", class = "warning-icon"),
                tags$div(
                  tags$strong("No Analysis Results"),
                  tags$br(),
                  "Please run an analysis first to view results."
                )
              )
            )
          )
        }

        # Create a list to hold all sections
        sections <- list()

        # Initialize sections list if it doesn't exist
        if (!exists("sections")) {
          sections <- list()
        }
        
        # Add Alignment section if analyzed
        if (isTRUE(rv_analysis$alignment_analyzed)) {
          log_info("Rendering alignment results")
          
          # Safely get alignment score with fallback
          alignment_score <- if (is.null(rv_analysis$alignment_score) || is.na(rv_analysis$alignment_score)) {
            log_warn("Alignment score is missing or invalid, using 0 as fallback")
            0
          } else {
            rv_analysis$alignment_score
          }
          
          log_info(paste("Alignment score:", alignment_score))
          
          alignment_section <- tags$fieldset(
            class = "custom-fieldset",
            style = "margin-bottom: 2rem;",
            tags$legend(class = "custom-legend", "Project Alignment"),
            score_section_ui(
              data = data.frame(score = alignment_score),
              value_box_title = "Alignment Score",
              score = alignment_score,
              type = "alignment",
              bgcolor = "#A08E6F",
              tooltip_text = "This score measures project alignment across eight collaboration areas. Higher scores indicate stronger shared vision and better coordination between partners.",
              icon_choice = ph_i("flower-lotus", weight = "bold", size = "4x"),
              input_id = "show_alignment_overlay",
              ns = ns,
              section_name = "Project Alignment Score",
              right_content = tagList(
                tags$fieldset(
                  class = "mini-fieldset",
                  tags$legend(class = "mini-legend", "Foundations"),
                  layout_columns(
                    style = "margin-bottom: 0 !important; gap: 0 !important;",
                    col_widths = c(6, 6),
                    # First column - Goals/Purposes
                    tags$div(
                      class = "d-flex align-items-start justify-items-start",
                      ph("compass",
                        weight = "light", size = "1.5x",
                        style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Goals/Purposes", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          get_alignment_domain_score("Purposes", rv_analysis$alignment_medians)
                        )
                      )
                    ),
                    # Second column - Values/Ideals
                    tags$div(
                      class = "d-flex align-items-start justify-items-end",
                      ph("lighthouse",
                        weight = "light", size = "1.5x",
                        style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Values/Ideals", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          get_alignment_domain_score("Ideals", rv_analysis$alignment_medians)
                        )
                      )
                    )
                  )
                ),
                tags$fieldset(
                  class = "mini-fieldset",
                  tags$legend(class = "mini-legend", "Implementation"),
                  layout_columns(
                    style = "margin-bottom: 0 !important; gap: 0 !important;",
                    col_widths = c(6,6),
                    # First column - Roles/Responsibilities
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("user-switch",
                        weight = "light", size = "1.5x",
                        style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Roles/Responsibilities", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    ),
                    # Second column - Resources/Assets
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("package",
                        weight = "light", size = "1.5x",
                        style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Resources", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    )
                  ),
                  layout_columns(
                    style = "margin-bottom: 0 !important; gap: 0 !important;",
                    col_widths = c(6, 6),
                    # First column - Activities/Events
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("calendar-heart",
                        weight = "light", size = "1.5x",
                        style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Activities/Events", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    ),
                    # Second column - Empowerment
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("hand-fist",
                        weight = "light", size = "1.5x",
                        style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Empowerment", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    )
                  )
                ),
                tags$fieldset(
                  class = "mini-fieldset",
                  tags$legend(class = "mini-legend", "Results"),
                  layout_columns(
                    style = "margin-bottom: 0 !important; gap: 0 !important;",
                    col_widths = c(6,6),
                    # First column - Outputs
                    tags$div(
                      class = "d-flex align-items-start g-0",
                      ph("files",
                         weight = "light", size = "1.5x",
                         style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem; gap: 0 !important; "
                      ),
                      tags$div(
                        tags$strong("Outputs", style = "color: var(--bs-primary); gap: 0 !important; text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); gap: 0 !important; text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    ),
                    # Second column - Outcomes
                    tags$div(
                      class = "d-flex align-items-start g-0",
                      ph("trend-up",
                         weight = "light", size = "1.5x",
                         style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem; gap: 0 !important; "
                      ),
                      tags$div(
                        tags$strong("Outcomes", style = "color: var(--bs-primary); gap: 0 !important; text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); gap: 0 !important; text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    )
                  )
                )
              ),
              placeholder_title = "No Alignment Analysis Results",
              placeholder_text = "Please run the alignment analysis first to view results.",
              placeholder_icon = ph("warning-circle", weight = "bold", class = "warning-icon")
            )
          )
          
          # Add to sections list
          sections <- c(sections, list(alignment_section))
        }

        # Add Dynamics section if analyzed
        if (isTRUE(rv_analysis$dynamics_analyzed)) {
          log_info("Rendering dynamics results")
          
          # Safely get dynamics score with fallback
          dynamics_score <- if (is.null(rv_analysis$dynamics_score) || is.na(rv_analysis$dynamics_score)) {
            log_warn("Dynamics score is missing or invalid, using 0 as fallback")
            0
          } else {
            rv_analysis$dynamics_score
          }
          
          log_info(paste("Dynamics score:", dynamics_score))
          
          dynamics_section <- tags$fieldset(
            class = "custom-fieldset",
            style = "margin-bottom: 2rem;",
            tags$legend(class = "custom-legend", "Project Dynamics"),
            score_section_ui(
              data = data.frame(score = dynamics_score),
              value_box_title = "Dynamics Score",
              score = dynamics_score,
              type = "dynamics",
              bgcolor = "#88707E",
              tooltip_text = "This score evaluates the internal project dynamics across five key domains. Higher scores indicate more balanced development and better integration of project components.",
              icon_choice = ph_i("pulse", weight = "bold", size = "4x"),
              input_id = "show_dynamics_overlay",
              ns = ns,
              section_name = "Project Dynamics",
              right_content = tagList(
                tags$fieldset(
                  class = "mini-fieldset",
                  tags$legend(class = "mini-legend", "Domain Scores"),
                  layout_columns(
                    style = "margin-bottom: 0 !important; gap: 0 !important;",
                    col_widths = c(6,6),
                    # First column - Contexts
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("polygon",
                         weight = "light", size = "1.5x",
                         style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Contexts", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    ),
                    # Second column - Partnership Processes
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("handshake",
                         weight = "light", size = "1.5x",
                         style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Partnership Processes", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    )
                  ),
                  layout_columns(
                    style = "margin-bottom: 0 !important; gap: 0 !important;",
                    class = "d-flex justify-items-center",
                    col_widths = c(12),
                    # First column - Interventions/Research
                    tags$div(
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("hand-heart",
                         weight = "light", size = "1.5x",
                         style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Interventions/ Research", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    )
                    )
                  ),
                  layout_columns(
                    style = "margin-bottom: 0 !important; gap: 0 !important;",
                    col_widths = c(6, 6),
                    # First column - Engaged Learning
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("book-open-user",
                         weight = "light", size = "1.5x",
                         style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("Engaged Learning", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    ),
                    # Second column - Outcomes
                    tags$div(
                    class = "d-flex align-items-start",
                    ph("chart-line-up",
                       weight = "light", size = "1.5x",
                       style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                    ),
                    tags$div(
                      tags$strong("Outcomes", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                      tags$br(),
                      tags$span(
                        style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                        safe_round(alignment_score, 2)
                      )
                    )
                  )
                  )
                ),
                tags$p("Dynamics analysis shows the internal project dynamics across key domains.")
              ),
              placeholder_title = "No Dynamics Analysis Results",
              placeholder_text = "Please run the dynamics analysis first to view results.",
              placeholder_icon = ph("warning-circle", weight = "bold", class = "warning-icon")
            )
          )
          
          # Add to sections list
          sections <- c(sections, list(dynamics_section))
        }

        # Add Cascade section if analyzed
        if (isTRUE(rv_analysis$cascade_analyzed)) {
          log_info("Rendering cascade results")
          
          # Safely get cascade score with fallback
          cascade_score <- if (is.null(rv_analysis$cascade_score) || is.na(rv_analysis$cascade_score)) {
            log_warn("Cascade score is missing or invalid, using 0 as fallback")
            0
          } else {
            rv_analysis$cascade_score
          }
          
          log_info(paste("Cascade score:", cascade_score))
          
          cascade_section <- tags$fieldset(
            class = "custom-fieldset",
            style = "margin-bottom: 2rem;",
            tags$legend(class = "custom-legend", "Cascade Effects"),
            score_section_ui(
              data = data.frame(score = cascade_score),
              value_box_title = "Cascade Score",
              score = cascade_score,
              type = "cascade",
              bgcolor = "#8A7A8F",
              tooltip_text = "This score measures the cascade effects across different domains. Higher scores indicate stronger cascading impacts.",
              icon_choice = ph_i("waveform", weight = "bold", size = "4x"),
              input_id = "show_cascade_overlay",
              ns = ns,
              section_name = "Cascade Effects",
              right_content = tagList(
                tags$fieldset(
                  class = "mini-fieldset",
                  tags$legend(class = "mini-legend", "Degree Scores"),
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    style = "margin-bottom: 0 !important; gap: 0 !important;",
                    # First column - 1st Degree
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("user-sound",
                         weight = "light", size = "1.5x",
                         style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("1st Degree", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    ),
                    # Second column - 2nd Degree
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("users",
                         weight = "light", size = "1.5x",
                         style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("2nd Degree", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    ),
                    # Third column - 3rd Degree
                    tags$div(
                      class = "d-flex align-items-start",
                      ph("users-three",
                         weight = "light", size = "1.5x",
                         style = "color: var(--bs-primary); margin-right:0.5rem; margin-top:0.1rem;"
                      ),
                      tags$div(
                        tags$strong("3rd Degree", style = "color: var(--bs-primary); text-transform: uppercase; font-size:0.8rem;"),
                        tags$br(),
                        tags$span(
                          style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace); font-size:0.9rem;",
                          safe_round(alignment_score, 2)
                        )
                      )
                    )
                  )
                )
              ),
              placeholder_title = "No Cascade Analysis Results",
              placeholder_text = "Please run the cascade analysis first to view results.",
              placeholder_icon = ph("warning-circle", weight = "bold", class = "warning-icon")
            )
          )
          
          # Add to sections list
          sections <- c(sections, list(cascade_section))
        }

        # Return all sections as a tagList
        do.call(tagList, sections)
      }) # End of renderUI

      # Render alignment analysis UI
      output$alignment_ui <- renderUI({
        if (!isTRUE(rv_analysis$alignment_analyzed)) {
          return(
            tags$div(
              class = "data-placeholder",
              tags$div(
                class = "d-flex align-items-center justify-content-center gap-2",
                ph("warning-circle", weight = "bold", class = "warning-icon"),
                tags$div(
                  tags$strong("No Alignment Analysis Results"),
                  tags$br(),
                  "Please run the alignment analysis first to view results."
                )
              )
            )
          )
        }
        req(rv_analysis$alignment_analyzed, rv_analysis$alignment_medians)
        alignment_data <- rv_analysis$alignment_medians
        numeric_cols <- sapply(alignment_data, is.numeric)
        if (any(numeric_cols)) alignment_data[numeric_cols] <- round(alignment_data[numeric_cols], 2)
        tagList(
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
                    style = "text-align: center; margin-top: 0.5em;",
                    actionButton(ns("show_alignment_overlay"), "Expand Table", class = "btn-primary"),
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
        if (!isTRUE(rv_analysis$dynamics_analyzed)) {
          return(
            tags$div(
              class = "data-placeholder",
              tags$div(
                class = "d-flex align-items-center justify-content-center gap-2",
                ph("warning-circle", weight = "bold", class = "warning-icon"),
                tags$div(
                  tags$strong("No Dynamics Analysis Results"),
                  tags$br(),
                  "Please run the dynamics analysis first to view results."
                )
              )
            )
          )
        }
        req(rv_analysis$dynamics_analyzed, rv_analysis$dimension_scores)
        tagList(
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
                    style = "text-align: center; margin-top: 0.5em;",
                    actionButton(ns("show_dynamics_overlay"), "Expand Table", class = "btn-primary"),
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
        if (!isTRUE(rv_analysis$cascade_analyzed)) {
          return(
            tags$div(
              class = "data-placeholder",
              tags$div(
                class = "d-flex align-items-center justify-content-center gap-2",
                ph("warning-circle", weight = "bold", class = "warning-icon"),
                tags$div(
                  tags$strong("No Cascade Analysis Results"),
                  tags$br(),
                  "Please run the cascade analysis first to view results."
                )
              )
            )
          )
        }
        req(rv_analysis$cascade_analyzed, rv_analysis$cascade_results)
        tagList(
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
                    style = "text-align: center; margin-top: 0.5em;",
                    actionButton(ns("show_cascade_overlay"), "Expand Table", class = "btn-primary"),
                    downloadButton(ns("download_cascade"),
                      "Download Full Cascade Data",
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

        if (is.data.frame(rv_analysis$dimension_scores)) {
          DT::datatable(
            rv_analysis$dimension_scores,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = "frtip"
            ),
            rownames = FALSE
          )
        } else {
          DT::datatable(
            data.frame(Message = "No dynamics dimension scores available"),
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = "frtip"
            ),
            rownames = FALSE
          )
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
          return(0.0) # Default fallback value
        }

        log_info("dynamics_results columns: {paste(names(dynamics_results), collapse = ', ')}")
        log_info("dynamics_results rows: {nrow(dynamics_results)}")
        log_info("dynamics_results structure:")
        # Use a safer way to log the structure
        tryCatch(
          {
            structure_output <- capture.output(str(dynamics_results))
            for (line in structure_output) {
              log_info(line)
            }
          },
          error = function(e) {
            log_warn("Could not log dynamics_results structure: {e$message}")
          }
        )

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
              dom = "ftip"
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
      create_workflow_observers(ns_project, ns, session)

      # Initialize workflow icons on module load with current state
      observe({
        # Workflow icons are now updated centrally by the main server observer
        logger::log_info("Workflow icons initialization skipped - handled centrally")
      })

      # Modular overlay server logic
      overlay_card_server <- function(id, show, close, title, content_ui) {
        moduleServer(id, function(input, output, session) {
          ns <- session$ns
          overlay_active <- reactiveVal(FALSE)
          observeEvent(show(), overlay_active(TRUE))
          observeEvent(input$close_overlay, overlay_active(FALSE))
          observeEvent(close(), overlay_active(FALSE))
          output$overlay_ui <- renderUI({
            if (isTRUE(overlay_active())) {
              tagList(
                div(class = "overlay-bg", onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("close_overlay"))),
                div(
                  class = "overlay-card", style = "background: #edeae2;",
                  tags$h4(title),
                  content_ui,
                  actionButton(ns("close_overlay"), "Close", class = "btn-secondary float-end")
                )
              )
            }
          })
        })
      }

      # Alignment overlay
      overlay_card_server("alignment_overlay",
        show = reactive(input$show_alignment_overlay),
        close = reactive(NULL),
        title = "Analyzed Alignment Data",
        content_ui = DT::dataTableOutput(ns("full_alignment_table_overlay"))
      )
      output$full_alignment_table_overlay <- DT::renderDataTable({
        req(rv_analysis$alignment_analyzed, rv_analysis$alignment_medians)
        alignment_data <- rv_analysis$alignment_medians
        numeric_cols <- sapply(alignment_data, is.numeric)
        if (any(numeric_cols)) alignment_data[numeric_cols] <- round(alignment_data[numeric_cols], 2)
        DT::datatable(alignment_data, options = list(scrollX = TRUE, pageLength = 10, dom = "ftip"), rownames = FALSE)
      })

      # Dynamics overlay
      overlay_card_server("dynamics_overlay",
        show = reactive(input$show_dynamics_overlay),
        close = reactive(NULL),
        title = "Analyzed Dynamics Data",
        content_ui = DT::dataTableOutput(ns("full_dynamics_table_overlay"))
      )
      output$full_dynamics_table_overlay <- DT::renderDataTable({
        req(rv_analysis$dynamics_analyzed)
        data_to_show <- if (is.data.frame(rv_analysis$full_results)) {
          rv_analysis$full_results
        } else if (is.data.frame(rv_analysis$dynamics_results)) {
          rv_analysis$dynamics_results
        } else {
          data.frame(Message = "No dynamics data available")
        }
        DT::datatable(data_to_show, options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"), rownames = FALSE)
      })

      # Cascade overlay
      overlay_card_server("cascade_overlay",
        show = reactive(input$show_cascade_overlay),
        close = reactive(NULL),
        title = "Analyzed Cascade Data",
        content_ui = DT::dataTableOutput(ns("cascade_results_table_overlay"))
      )
      output$cascade_results_table_overlay <- DT::renderDataTable({
        req(rv_analysis$cascade_analyzed, rv_analysis$cascade_results)
        DT::datatable(rv_analysis$cascade_results, options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"), rownames = FALSE)
      })

      # Return analysis results
      return(rv_analysis)
    } # Closes the 'module = function(...) {' block
  ) # Closes the 'moduleServer(...)' call
} # Closes the 'mod_analyze_server <- function(...) {' block
