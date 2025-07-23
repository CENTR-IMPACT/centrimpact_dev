#!! ModName = mod_analyze
# !! ModDisplayName = Analyze Data
# !! ModDescription = Analyze project data including alignment, dynamics, and cascade metrics
# !! ModCitation = Price, Jeremy F. (2025). mod_analyze. [Source code].
# !! ModNotes = This module provides functionality to analyze project data across multiple dimensions.
# !! ModActive = 1
# !! FunctionArg = project_data !! Project data for analysis !! reactive

# Utilities are loaded in global.R

# Minimal logger:: logging for module load
logger::log_info("[@mod_analyze.R] Analyze module loaded")

# Define null-coalescing operator for cleaner code
`%||%` <- function(x, y) if (is.null(x)) y else x
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
  if (is.null(val) || length(val) == 0 || is.na(val)) {
    return(0)
  }
  round(val, digits)
}

# the server function with enhanced debugging
mod_analyze_server <- function(id, project_data) {
  logger::log_info(paste0("[@mod_analyze.R] mod_analyze_server called with id: ", id))

  # Initialize analysis results in project_data if not already present
  if (is.null(project_data$analysis)) {
    project_data$analysis <- reactiveValues(
      alignment_analyzed = FALSE,
      dynamics_analyzed = FALSE,
      cascade_analyzed = FALSE,
      last_analysis_type = NULL,
      last_updated = NULL,
      alignment = NULL,
      dynamics = NULL,
      cascade = NULL
    )
  }

  # No need for local reference, we'll use project_data$analysis directly

  moduleServer(
    id = id,
    module = function(input, output, session) {
      logger::log_info("[@mod_analyze.R] Module server initialized")

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

      # Overlay server logic (update to use new structure)
      observe({
        # Alignment overlay table
        output$full_alignment_table_overlay <- DT::renderDataTable({
          req(project_data$analysis$alignment_analyzed)
          req(!is.null(project_data$analysis$alignment))
          req(!is.null(project_data$analysis$alignment$alignment_medians))
          project_data$analysis$alignment$alignment_medians
        })

        # Dynamics overlay table
        output$full_dynamics_table_overlay <- DT::renderDataTable({
          req(project_data$analysis$dynamics_analyzed)
          req(!is.null(project_data$analysis$dynamics))
          # If the package returns a domain summary, use it; otherwise, fallback to the main result
          if (!is.null(project_data$analysis$dynamics$domain_df)) {
            project_data$analysis$dynamics$domain_df
          } else if (!is.null(project_data$analysis$dynamics$dynamics_df)) {
            project_data$analysis$dynamics$dynamics_df
          } else {
            NULL
          }
        })

        # Cascade overlay table
        output$cascade_results_table_overlay <- DT::renderDataTable({
          req(project_data$analysis$cascade_analyzed)
          req(!is.null(project_data$analysis$cascade))
          # If the package returns a cascade_results df, use it; otherwise, fallback to the main result
          if (!is.null(project_data$analysis$cascade$cascade_results)) {
            project_data$analysis$cascade$cascade_results
          } else if (!is.null(project_data$analysis$cascade$cascade_df)) {
            project_data$analysis$cascade$cascade_df
          } else {
            NULL
          }
        })
      })
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

      # Overlay server logic (update to use new structure)
      observe({
        # Alignment overlay table
        output$full_alignment_table_overlay <- DT::renderDataTable({
          req(project_data$analysis$alignment_analyzed)
          req(!is.null(project_data$analysis$alignment))
          req(!is.null(project_data$analysis$alignment$alignment_medians))
          project_data$analysis$alignment$alignment_medians
        })

        # Dynamics overlay table
        output$full_dynamics_table_overlay <- DT::renderDataTable({
          req(project_data$analysis$dynamics_analyzed)
          req(!is.null(project_data$analysis$dynamics))
          # If the package returns a domain summary, use it; otherwise, fallback to the main result
          if (!is.null(project_data$analysis$dynamics$domain_df)) {
            project_data$analysis$dynamics$domain_df
          } else if (!is.null(project_data$analysis$dynamics$dynamics_df)) {
            project_data$analysis$dynamics$dynamics_df
          } else {
            NULL
          }
        })

        # Cascade overlay table
        output$cascade_results_table_overlay <- DT::renderDataTable({
          req(project_data$analysis$cascade_analyzed)
          req(!is.null(project_data$analysis$cascade))
          # If the package returns a cascade_results df, use it; otherwise, fallback to the main result
          if (!is.null(project_data$analysis$cascade$cascade_results)) {
            project_data$analysis$cascade$cascade_results
          } else if (!is.null(project_data$analysis$cascade$cascade_df)) {
            project_data$analysis$cascade$cascade_df
          } else {
            NULL
          }
        })
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
        logger::log_info("[@mod_analyze.R] Data status checked")
      })

      # --- Observe per-metric workflow state for Alignment Clean Data completion and update status/icon ---
      # Commented out ns_project references as they're not defined
      # observe({
      #   wf <- ns_project$workflow$alignment
      #   if (!is.null(wf) && wf$stage == "Clean Data" && wf$status == "complete") {
      #     update_alignment_status_display("cleaned", session, ns, ns_project)
      #     update_alignment_workflow_icons(ns_project, session)
      # --- Observe workflow state for Alignment Clean Data completion and update status/icon ---
      observe({
        # Check if we have alignment data
        if (!is.null(project_data$cleaned_data$alignment)) {
          # For now, just log that we have alignment data
          log_info("Alignment data is available")
        }
      })

      # Reactive expressions to get data from project_data
      alignment_data <- reactive({
        log_debug("Accessing alignment data reactively...")
        if (is.null(project_data$cleaned_data$alignment)) {
          log_warn("project_data$cleaned_data$alignment is NULL")
          return(NULL)
        }

        # Check if the data is actually a data frame
        if (!is.data.frame(project_data$cleaned_data$alignment)) {
          log_warn("alignment_data is not a data frame, it's: {class(project_data$cleaned_data$alignment)}")
          return(NULL)
        }

        log_info("Returning alignment data from reactive context: {nrow(project_data$cleaned_data$alignment)} rows, {ncol(project_data$cleaned_data$alignment)} columns")
        return(project_data$cleaned_data$alignment)
      })

      dynamics_data <- reactive({
        log_debug("Accessing dynamics data reactively...")
        if (is.null(project_data$cleaned_data$dynamics)) {
          log_warn("project_data$cleaned_data$dynamics is NULL")
          return(NULL)
        }

        # Check if the data is actually a data frame
        if (!is.data.frame(project_data$cleaned_data$dynamics)) {
          log_warn("dynamics_data is not a data frame, it's: {class(project_data$cleaned_data$dynamics)}")
          return(NULL)
        }

        log_info("Returning dynamics data from reactive context: {nrow(project_data$cleaned_data$dynamics)} rows, {ncol(project_data$cleaned_data$dynamics)} columns")
        return(project_data$cleaned_data$dynamics)
      })

      cascade_data <- reactive({
        log_debug("Accessing cascade data reactively...")

        if (is.null(project_data$cleaned_data$cascade)) {
          log_warn("project_data$cleaned_data$cascade is NULL")
          return(NULL)
        }

        # Check if the data is actually a data frame
        if (!is.data.frame(project_data$cleaned_data$cascade)) {
          log_warn("cascade_data is not a data frame, it's: {class(project_data$cleaned_data$cascade)}")
          return(NULL)
        }

        log_info("Returning cascade data from reactive context: {nrow(project_data$cleaned_data$cascade)} rows, {ncol(project_data$cleaned_data$cascade)} columns")
        return(project_data$cleaned_data$cascade)
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

      # Log the initialization of reactive values
      observe({
        log_info(
          "Initialized project_data$analysis with keys: ",
          paste(names(project_data$analysis), collapse = ", ")
        )
      })

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
        log_info("Checking project data availability...")

        # Check alignment data
        if (!is.null(project_data$cleaned_data$alignment)) {
          log_info("Alignment data available: {nrow(project_data$cleaned_data$alignment)} rows, {ncol(project_data$cleaned_data$alignment)} columns")
        } else {
          log_warn("No alignment data available")
        }

        # Check dynamics data
        if (!is.null(project_data$cleaned_data$dynamics)) {
          log_info("Dynamics data available: {nrow(project_data$cleaned_data$dynamics)} rows, {ncol(project_data$cleaned_data$dynamics)} columns")
        } else {
          log_warn("No dynamics data available")
        }

        # Check cascade data
        if (!is.null(project_data$cleaned_data$cascade)) {
          if (is.list(project_data$cleaned_data$cascade)) {
            log_info("Cascade data available: list with elements: {paste(names(project_data$cleaned_data$cascade), collapse = ', ')}")
          } else {
            log_info("Cascade data available: {class(project_data$cleaned_data$cascade)}")
          }
        } else {
          log_warn("No cascade data available")
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
          id = ns("progress_bar"),
          value = 0,
          status = "info"
        )

        # Show the progress container
        shinyjs::runjs(sprintf('document.getElementById("%s").style.visibility = "visible";', ns("progress_container")))

        # Helper function to run analysis with common error handling and progress updates
        run_analysis <- function(analysis_type, data_func, analysis_func, progress_stages, extra_processing = NULL) {
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

              # Store results in project_data$analysis directly
              log_info(paste("Storing results for", analysis_type, "analysis"))
              log_info(paste("Result keys available:", paste(names(result), collapse = ", ")))

              # Prepare all values to set
              values_to_set <- list()

              # Store each result with the analysis_type prefix
              for (name in names(result)) {
                key <- paste0(analysis_type, "_", name)
                log_info(paste("Will set", key, "to:", toString(utils::head(result[[name]]))))
                values_to_set[[key]] <- result[[name]]
              }

              # Add the analyzed flag
              analyzed_key <- paste0(analysis_type, "_analyzed")
              values_to_set[[analyzed_key]] <- TRUE

              # Add last updated timestamp
              values_to_set$last_updated <- Sys.time()
              values_to_set$last_analysis_type <- analysis_type

              # Update all reactive values in a single isolate block
              isolate({
                for (name in names(values_to_set)) {
                  project_data$analysis[[name]] <- values_to_set[[name]]
                }
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
        # DRY helper functions for analysis
        #' Run alignment analysis
        #' 
        #' @param alignment_data Data frame containing alignment data with required columns: role, alignment, rating
        #' @return A list containing analysis results with elements: score, table, icc, and analyzed flag
        #' @export
        run_alignment_analysis <- function(alignment_data) {
          logger::log_info("[@mod_analyze.R] Running alignment analysis")
          
          # Input validation
          if (is.null(alignment_data)) {
            stop("No alignment data available")
          }
          
          if (!is.data.frame(alignment_data)) {
            stop(paste("Alignment data must be a data frame, got:", class(alignment_data)))
          }
          
          if (nrow(alignment_data) == 0) {
            stop("Alignment data is empty")
          }
          
          # Check for required columns
          required_cols <- c("role", "alignment", "rating")
          missing_cols <- setdiff(required_cols, names(alignment_data))
          if (length(missing_cols) > 0) {
            stop(paste("Alignment data is missing required columns: ", 
                      paste(missing_cols, collapse = ", ")))
          }
          
          # Check for data quality
          if (all(is.na(alignment_data$rating))) {
            stop("All rating values are NA - cannot perform analysis")
          }
          
          tryCatch({
            # Run the analysis
            analysis_result <- centrimpact::analyze_alignment(alignment_df = alignment_data)
            
            # Validate the result
            if (is.null(analysis_result)) {
              stop("analyze_alignment returned NULL")
            }
            
            # Log the result structure for debugging
            logger::log_info("Analysis result type: {class(analysis_result)}")
            logger::log_info("Analysis result length: {length(analysis_result)}")
            
            # Initialize result structure
            result <- list(
              score = NULL,
              table = data.frame(),
              icc = NA_real_,
              analyzed = TRUE,
              timestamp = Sys.time()
            )
            
            # Handle different result structures
            if (is.atomic(analysis_result)) {
              # If it's an atomic vector, treat it as the alignment score
              result$score <- as.numeric(analysis_result[1])
            } else if (is.list(analysis_result)) {
              # Extract components from list result
              result$score <- if (!is.null(analysis_result$alignment_score)) {
                as.numeric(analysis_result$alignment_score[1])
              } else if (length(analysis_result) > 0) {
                as.numeric(analysis_result[[1]])
              }
              
              result$table <- if (!is.null(analysis_result$alignment_medians)) {
                as.data.frame(analysis_result$alignment_medians)
              } else {
                data.frame()
              }
              
              result$icc <- if (!is.null(analysis_result$icc_score)) {
                as.numeric(analysis_result$icc_score[1])
              } else {
                NA_real_
              }
            }
            
            # Validate we have a score
            if (is.null(result$score) || is.na(result$score)) {
              stop("Could not determine alignment score from analysis result")
            }
            
            logger::log_info("Alignment analysis completed successfully")
            logger::log_info("Alignment score: {result$score}")
            if (!is.na(result$icc)) {
              logger::log_info("ICC score: {result$icc}")
            }
            
            return(result)
            
          }, error = function(e) {
            logger::log_error("[@mod_analyze.R] Error in alignment analysis: {conditionMessage(e)}")
            stop(e) # Re-throw the error to be handled by the caller
          })
        }

        #' Run dynamics analysis
        #' 
        #' @param dynamics_data Data frame containing dynamics data with required columns: role, domain, dimension, rating
        #' @return A list containing analysis results with elements: score, domains, dynamics, and analyzed flag
        #' @export
        run_dynamics_analysis <- function(dynamics_data) {
          logger::log_info("[@mod_analyze.R] Running dynamics analysis")
          
          # Input validation
          if (is.null(dynamics_data)) {
            stop("No dynamics data available")
          }
          
          if (!is.data.frame(dynamics_data)) {
            stop(paste("Dynamics data must be a data frame, got:", class(dynamics_data)))
          }
          
          if (nrow(dynamics_data) == 0) {
            stop("Dynamics data is empty")
          }
          
          # Check for required columns
          required_cols <- c("role", "domain", "dimension", "rating")
          missing_cols <- setdiff(required_cols, names(dynamics_data))
          if (length(missing_cols) > 0) {
            stop(paste("Dynamics data is missing required columns: ", 
                      paste(missing_cols, collapse = ", ")))
          }
          
          # Check for data quality
          if (all(is.na(dynamics_data$rating))) {
            stop("All rating values are NA - cannot perform analysis")
          }
          
          tryCatch({
            # Run the analysis
            analysis_result <- centrimpact::analyze_dynamics(dynamics_df = dynamics_data)
            
            # Validate the result
            if (is.null(analysis_result)) {
              stop("analyze_dynamics returned NULL")
            }
            
            # Log the result structure for debugging
            logger::log_info("Dynamics result type: {class(analysis_result)}")
            logger::log_info("Dynamics result length: {length(analysis_result)}")
            
            # Initialize result structure
            result <- list(
              score = NULL,
              domains = data.frame(),
              dynamics = data.frame(),
              analyzed = TRUE,
              timestamp = Sys.time()
            )
            
            # Handle different result structures
            if (is.atomic(analysis_result)) {
              # If it's an atomic vector, treat it as the dynamics score
              result$score <- as.numeric(analysis_result[1])
            } else if (is.list(analysis_result)) {
              # Extract components from list result
              result$score <- if (!is.null(analysis_result$dynamics_score)) {
                as.numeric(analysis_result$dynamics_score[1])
              } else if (length(analysis_result) > 0) {
                as.numeric(analysis_result[[1]])
              }
              
              result$domains <- if (!is.null(analysis_result$domain_df)) {
                as.data.frame(analysis_result$domain_df)
              } else {
                data.frame()
              }
              
              result$dynamics <- if (!is.null(analysis_result$dynamics_df)) {
                as.data.frame(analysis_result$dynamics_df)
              } else {
                data.frame()
              }
            }
            
            # Validate we have a score
            if (is.null(result$score) || is.na(result$score)) {
              stop("Could not determine dynamics score from analysis result")
            }
            
            logger::log_info("Dynamics analysis completed successfully")
            logger::log_info("Dynamics score: {result$score}")
            
            return(result)
            
          }, error = function(e) {
            logger::log_error("[@mod_analyze.R] Error in dynamics analysis: {conditionMessage(e)}")
            stop(e) # Re-throw the error to be handled by the caller
          })
        }  
        

        # Helper function to validate and prepare cascade input
        prepare_cascade_input <- function(cascade_raw) {
          cascade_input <- NULL
          
          if (is.list(cascade_raw) && !is.null(cascade_raw$edges)) {
            # If it's a list with edges, extract the edgelist
            cascade_input <- cascade_raw$edges
            logger::log_info("Extracted cascade edgelist from input data")
          } else if (is.data.frame(cascade_raw)) {
            # If it's already a data frame, use it directly
            cascade_input <- cascade_raw
            logger::log_info("Using cascade input as is (data frame)")
          }
          
          # Validate the input
          if (is.null(cascade_input)) {
            stop("Cascade data must be a data frame (edgelist) or list containing 'edges' element")
          }
          
          # Check for required columns if it's a data frame
          if (is.data.frame(cascade_input)) {
            required_cols <- c("from", "to", "weight")
            missing_cols <- setdiff(required_cols, names(cascade_input))
            if (length(missing_cols) > 0) {
              stop("Cascade edgelist is missing required columns: ", 
                   paste(missing_cols, collapse = ", "))
            }
          }
          
          return(cascade_input)
        }
        
        #' Run cascade analysis
        #' 
        #' @param cascade_data Data frame containing cascade data with required columns: from, to, weight
        #' @return A list containing analysis results with elements: score, data, and analyzed flag
        #' @export
        run_cascade_analysis <- function(cascade_data) {
          logger::log_info("[@mod_analyze.R] Running cascade analysis")
          
          # Validate input
          if (is.null(cascade_data)) {
            stop("No cascade data provided")
          }
          
          # Prepare and validate cascade input
          cascade_input <- tryCatch(
            {
              prepare_cascade_input(cascade_data)
            },
            error = function(e) {
              logger::log_error("Error preparing cascade input: {e$message}")
              stop(e)
            }
          )
          
          # Run the analysis with error handling
          tryCatch({
            logger::log_info("Running centrimpact::analyze_cascade...")
            analysis_result <- centrimpact::analyze_cascade(cascade_input)
            
            # Validate the result
            if (is.null(analysis_result)) {
              stop("analyze_cascade returned NULL")
            }
            
            # Log the result structure for debugging
            logger::log_info("Cascade result type: {class(analysis_result)}")
            logger::log_info("Cascade result length: {length(analysis_result)}")
            
            # Initialize result structure
            result <- list(
              score = NULL,
              data = data.frame(),
              analyzed = TRUE,
              timestamp = Sys.time()
            )
            
            # Handle different result structures
            if (is.atomic(analysis_result)) {
              # If it's an atomic vector, treat it as the cascade score
              result$score <- as.numeric(analysis_result[1])
            } else if (is.list(analysis_result)) {
              # Extract components from list result
              result$score <- if (!is.null(analysis_result$cascade_score)) {
                as.numeric(analysis_result$cascade_score[1])
              } else if (length(analysis_result) > 0) {
                as.numeric(analysis_result[[1]])
              }
              
              result$data <- if (!is.null(analysis_result$cascade_df)) {
                as.data.frame(analysis_result$cascade_df)
              } else {
                data.frame()
              }
            }
            
            # Validate we have a score
            if (is.null(result$score) || is.na(result$score)) {
              stop("Could not determine cascade score from analysis result")
            }
            
            logger::log_info("Cascade analysis completed successfully")
            logger::log_info("Cascade score: {result$score}")
            
            return(result)
            
          }, error = function(e) {
            logger::log_error("[@mod_analyze.R] Error in cascade analysis: {conditionMessage(e)}")
            stop(e) # Re-throw the error to be handled by the caller
          })
        }

        # Run the appropriate analysis based on type
        analysis_result <- tryCatch({
          switch(input$analysis_type,
            "alignment" = {
              logger::log_info("Starting alignment analysis")
              result <- run_alignment_analysis(alignment_data())
              
              # Store results in reactive values
              isolate({
                project_data$analysis$alignment_score <- result$score
                project_data$analysis$alignment_table <- result$table
                project_data$analysis$alignment_icc <- result$icc
                project_data$analysis$alignment_analyzed <- TRUE
                project_data$analysis$last_analysis_type <- "alignment"
                project_data$analysis$last_updated <- Sys.time()
              })
              
              logger::log_info("Alignment analysis completed successfully")
              return(TRUE)
            },
            "dynamics" = {
              logger::log_info("Starting dynamics analysis")
              result <- run_dynamics_analysis(dynamics_data())
              
              # Store results in reactive values
              isolate({
                project_data$analysis$dynamics_score <- result$score
                project_data$analysis$dynamics_domains <- result$domains
                project_data$analysis$dynamics_dynamics <- result$dynamics
                project_data$analysis$dynamics_analyzed <- TRUE
                project_data$analysis$last_analysis_type <- "dynamics"
                project_data$analysis$last_updated <- Sys.time()
              })
              
              logger::log_info("Dynamics analysis completed successfully")
              return(TRUE)
            },
            "cascade" = {
              logger::log_info("Starting cascade analysis")
              result <- run_cascade_analysis(cascade_data())
              
              # Store results in reactive values
              isolate({
                project_data$analysis$cascade_score <- result$score
                project_data$analysis$cascade_data <- result$data
                project_data$analysis$cascade_analyzed <- TRUE
                project_data$analysis$last_analysis_type <- "cascade"
                project_data$analysis$last_updated <- Sys.time()
              })
              
              logger::log_info("Cascade analysis completed successfully")
              return(TRUE)
            },
            "full" = {
              logger::log_info("Starting full analysis")
              shinyjs::runjs(sprintf('document.getElementById("%s").classList.add("loading");', ns("run_analysis")))
            tryCatch({
              result <- tryCatch(
                {
                  # Get the cascade data
                  cascade_raw <- cascade_data()
                  if (is.null(cascade_raw)) {
                    stop("No cascade data available. Please load data first.")
                  }
                  
                  # Log data structure for debugging
                  logger::log_info("Cascade data structure: {class(cascade_raw)}")
                  if (is.list(cascade_raw)) {
                    logger::log_info("Cascade data keys: {paste(names(cascade_raw), collapse = ', ')}")
                  }
                  
                  # Run the analysis with the full cascade data
                  # The run_cascade_analysis function will handle extracting the edgelist
                  out <- run_cascade_analysis(cascade_raw, project_data$analysis)
                  
                  # The cascade_analyzed flag is set inside run_cascade_analysis
                  out
                },
                error = function(e) {
                  logger::log_error("[@mod_analyze.R] Error in cascade analysis")
                  return(FALSE)
                }
              )
              if (!result) {
                showNotification("Error in cascade analysis. Check logs for details.", type = "error")
                return()
              }
              showNotification("Cascade analysis completed successfully!", type = "message")
            }, error = function(e) {
              logger::log_error("[@mod_analyze.R] Error in cascade analysis")
              showNotification(paste("Error:", conditionMessage(e)), type = "error")
            }, finally = {
              shinyjs::runjs(sprintf('document.getElementById("%s").classList.remove("loading");', ns("run_analysis")))
            })
          },
          "full" = {
            log_info("Running full analysis...")
            shinyjs::runjs(sprintf('document.getElementById("%s").classList.add("loading");', ns("run_analysis")))
            
            # Initialize progress tracking
            total_steps <- 3  # Total number of analysis steps
            current_step <- 0
            
            tryCatch({
              all_success <- TRUE
              
              # Run alignment analysis if data is available
              if (!is.null(alignment_data())) {
                current_step <- current_step + 1
                update_progress(
                  value = (current_step - 1) / total_steps * 100,
                  text = sprintf("Running alignment analysis (%d/%d)...", current_step, total_steps),
                  detail = "Analyzing alignment between project components..."
                )
                
                tryCatch({
                  result <- run_alignment_analysis(alignment_data())
                  
                  # Store results in reactive values
                  isolate({
                    project_data$analysis$alignment_score <- result$score
                    project_data$analysis$alignment_table <- result$table
                    project_data$analysis$alignment_icc <- result$icc
                    project_data$analysis$alignment_analyzed <- TRUE
                    project_data$analysis$last_analysis_type <- "alignment"
                    project_data$analysis$last_updated <- Sys.time()
                  })
                  
                  showNotification(
                    "Alignment analysis completed successfully!",
                    type = "message"
                  )
                }, error = function(e) {
                  all_success <- FALSE
                  logger::log_error("Error in alignment analysis: {e$message}")
                  showNotification(
                    "Alignment analysis failed. Check logs for details.",
                    type = "error"
                  )
                })
              } else {
                log_warn("Skipping alignment analysis: no data available")
                update_progress(
                  value = (1 / total_steps) * 100,
                  text = "Skipped alignment analysis (no data)",
                  status = "warning"
                )
              }
              
              # Run dynamics analysis if data is available
              if (!is.null(dynamics_data())) {
                current_step <- current_step + 1
                update_progress(
                  value = (current_step - 1) / total_steps * 100,
                  text = sprintf("Running dynamics analysis (%d/%d)...", current_step, total_steps),
                  detail = "Analyzing project dynamics..."
                )
                
                tryCatch({
                  result <- run_dynamics_analysis(dynamics_data())
                  
                  # Store results in reactive values
                  isolate({
                    project_data$analysis$dynamics_score <- result$score
                    project_data$analysis$dynamics_domains <- result$domains
                    project_data$analysis$dynamics_dynamics <- result$dynamics
                    project_data$analysis$dynamics_analyzed <- TRUE
                    project_data$analysis$last_analysis_type <- "dynamics"
                    project_data$analysis$last_updated <- Sys.time()
                  })
                  
                  showNotification(
                    "Dynamics analysis completed successfully!",
                    type = "message"
                  )
                }, error = function(e) {
                  all_success <- FALSE
                  logger::log_error("Error in dynamics analysis: {e$message}")
                  showNotification(
                    "Dynamics analysis failed. Check logs for details.",
                    type = "error"
                  )
                })
              } else {
                log_warn("Skipping dynamics analysis: no data available")
                update_progress(
                  value = (2 / total_steps) * 100,
                  text = "Skipped dynamics analysis (no data)",
                  status = "warning"
                )
              }
              
              # Run cascade analysis if data is available
              if (!is.null(cascade_data())) {
                current_step <- current_step + 1
                update_progress(
                  value = (current_step - 1) / total_steps * 100,
                  text = sprintf("Running cascade analysis (%d/%d)...", current_step, total_steps),
                  detail = "Analyzing cascade effects..."
                )
                
                tryCatch({
                  result <- run_cascade_analysis(cascade_data())
                  
                  # Store results in reactive values
                  isolate({
                    project_data$analysis$cascade_score <- result$score
                    project_data$analysis$cascade_data <- result$data
                    project_data$analysis$cascade_analyzed <- TRUE
                    project_data$analysis$last_analysis_type <- "cascade"
                    project_data$analysis$last_updated <- Sys.time()
                  })
                  
                  showNotification(
                    "Cascade analysis completed successfully!",
                    type = "message"
                  )
                }, error = function(e) {
                  all_success <- FALSE
                  logger::log_error("Error in cascade analysis: {e$message}")
                  showNotification(
                    "Cascade analysis failed. Check logs for details.",
                    type = "error"
                  )
                })
              } else {
                log_warn("Skipping cascade analysis: no data available")
                update_progress(
                  value = 100,
                  text = "Skipped cascade analysis (no data)",
                  status = "warning"
                )
              }

              # Final update
              if (all_success) {
                update_progress(
                  value = 100,
                  text = "All analyses completed successfully!",
                  status = "success"
                )
                showNotification("All analyses completed successfully!", type = "message")
              } else {
                log_warn("Some analyses in the full analysis failed")
                update_progress(
                  value = 100,
                  text = "Some analyses completed with errors. Check logs for details.",
                  status = "warning"
                )
                showNotification(
                  "Some analyses in the full analysis failed. Check logs for details.",
                  type = "warning"
                )
              }
            }, error = function(e) {
              log_error(paste("Error in full analysis:", conditionMessage(e)))
              showNotification(
                paste("Error in full analysis:", conditionMessage(e)),
                type = "error"
              )
            }, finally = {
              shinyjs::runjs(sprintf('document.getElementById("%s").classList.remove("loading");', ns("run_analysis")))
            })
          }
        )
      })

      # Helper function to get score styling with validation and fallbacks
      get_score_style <- function(score, scale = 5) {
        # Handle NA/NaN/Inf values
        if (is.null(score) || is.na(score) || is.nan(score) || is.infinite(score)) {
          return(list(color = "#6c757d", level = "N/A"))
        }
        
        # Ensure score is numeric
        score <- as.numeric(score)
        
        # Handle invalid scores
        if (is.na(score) || is.nan(score) || is.infinite(score)) {
          return(list(color = "#6c757d", level = "N/A"))
        }
        
        # 5-point scale colors and levels (default)
        if (scale == 5) {
          if (score >= 0.8) return(list(color = "#28a745", level = "High"))
          if (score >= 0.6) return(list(color = "#5cb85c", level = "Good"))
          if (score >= 0.4) return(list(color = "#ffc107", level = "Moderate"))
          if (score >= 0.2) return(list(color = "#fd7e14", level = "Low"))
          return(list(color = "#dc3545", level = "Very Low"))
        }
        
        # 10-point scale colors and levels
        if (scale == 10) {
          if (score >= 9) return(list(color = "#28a745", level = "Excellent"))
          if (score >= 7) return(list(color = "#5cb85c", level = "Very Good"))
          if (score >= 5) return(list(color = "#ffc107", level = "Good"))
          if (score >= 3) return(list(color = "#fd7e14", level = "Fair"))
          return(list(color = "#dc3545", level = "Poor"))
        }
        
        # Default to 5-point scale for unknown scales
        if (score >= 0.8) return(list(color = "#28a745", level = "High"))
        if (score >= 0.6) return(list(color = "#5cb85c", level = "Good"))
        if (score >= 0.4) return(list(color = "#ffc107", level = "Moderate"))
        if (score >= 0.2) return(list(color = "#fd7e14", level = "Low"))
        return(list(color = "#dc3545", level = "Very Low"))
      }
      get_score_style <- function(score, scale = 5) {
        if (scale == 5) {
          # 5-point scale colors and levels
          if (is.na(score)) {
            return(list(color = "#6c757d", level = "N/A"))
          } else if (score >= 4.5) {
            return(list(color = "#4E342E", level = "High")) # Rich Espresso
          } else if (score >= 3.5) {
            return(list(color = "#A64B42", level = "Medium-High")) # Rust Red
          } else if (score >= 2.5) {
            return(list(color = "#BC7A5A", level = "Medium")) # Muted Terracotta
          } else if (score >= 1.5) {
            return(list(color = "#3F5E78", level = "Medium-Low")) # Denim Blue
          } else {
            return(list(color = "#6B6459", level = "Low")) # Stone Grey-Brown
          }
        } else if (scale == 4) {
          # 4-point scale colors and levels
          if (is.na(score)) {
            return(list(color = "#6c757d", level = "N/A", bg_color = "#f8f9fa"))
          } else if (score >= 3.5) {
            return(list(color = "#4E342E", level = "High", bg_color = "#f5e8e4")) # Rich Espresso
          } else if (score >= 2.5) {
            return(list(color = "#A64B42", level = "Medium-High", bg_color = "#f8e9e7")) # Rust Red
          } else if (score >= 1.5) {
            return(list(color = "#BC7A5A", level = "Medium", bg_color = "#faf1ed")) # Muted Terracotta
          } else {
            return(list(color = "#3F5E78", level = "Low", bg_color = "#f0f4f8")) # Denim Blue
          }
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

      # Helper function to safely get score from analysis results with detailed logging
      safe_get_score <- function(analysis_type, score_key) {
        log_info("=== DEBUG: safe_get_score called ===")
        log_info(paste("Analysis type:", analysis_type))
        log_info(paste("Score key:", score_key))
        
        # Safely get the analysis result
        analysis_result <- NULL
        if (exists("project_data") && !is.null(project_data$analysis)) {
          analysis_result <- project_data$analysis[[analysis_type]]
        }
        
        # Debug: Log the type and structure of the analysis result
        if (!is.null(analysis_result)) {
          log_info(paste("Analysis result type:", class(analysis_result)))
          if (is.list(analysis_result)) {
            log_info(paste("Analysis result keys:", paste(names(analysis_result), collapse = ", ")))
          } else if (is.atomic(analysis_result)) {
            log_info(paste("Atomic value:", paste(analysis_result, collapse = ", ")))
          }
        } else {
          log_info(paste("No analysis result found for type:", analysis_type))
        }
        
        # Function to safely extract numeric value from an object
        safe_extract_numeric <- function(x) {
          if (is.null(x)) return(NA)
          if (is.numeric(x)) return(as.numeric(x[1]))
          if (is.character(x)) {
            num_val <- suppressWarnings(as.numeric(x))
            if (!is.na(num_val)) return(num_val)
          }
          if (is.list(x) && "value" %in% names(x)) {
            return(safe_extract_numeric(x$value))
          }
          return(NA)
        }
        
        # 1. First try to get the score from the flat structure (backward compatibility)
        flat_key <- paste0(analysis_type, "_", score_key)
        if (exists("project_data") && !is.null(project_data$analysis[[flat_key]])) {
          score <- safe_extract_numeric(project_data$analysis[[flat_key]])
          if (!is.na(score)) {
            log_info(paste("Found score in flat structure:", score))
            return(score)
          }
        }
        
        # 2. Try direct access in the analysis result
        if (!is.null(analysis_result)) {
          # Handle atomic vectors (single values)
          if (is.atomic(analysis_result) && length(analysis_result) == 1) {
            score <- safe_extract_numeric(analysis_result)
            if (!is.na(score)) {
              log_info(paste("Found atomic score in analysis result:", score))
              return(score)
            }
          }
          
          # Handle list results
          if (is.list(analysis_result)) {
            # Try direct match first
            if (!is.null(analysis_result[[score_key]])) {
              score <- safe_extract_numeric(analysis_result[[score_key]])
              if (!is.na(score)) {
                log_info(paste("Found score in analysis result:", score))
                return(score)
              }
            }
            
            # Try common score field names
            common_fields <- c("score", "value", "mean", "average", "estimate", "statistic")
            for (field in common_fields) {
              if (field %in% names(analysis_result)) {
                score <- safe_extract_numeric(analysis_result[[field]])
                if (!is.na(score)) {
                  log_info(paste("Found score in common field", field, ":", score))
                  return(score)
                }
              }
            }
          }
        }
        
        # 3. For alignment analysis, try to calculate from medians if score not found directly
        if (analysis_type == "alignment" && score_key == "alignment_score") {
          medians <- NULL
          if (!is.null(analysis_result) && "alignment_medians" %in% names(analysis_result)) {
            medians <- analysis_result$alignment_medians
          } else if (exists("project_data") && !is.null(project_data$analysis$alignment_medians)) {
            medians <- project_data$analysis$alignment_medians
          }
          
          if (!is.null(medians) && is.data.frame(medians)) {
            if ("overall" %in% names(medians)) {
              score <- mean(medians$overall, na.rm = TRUE)
              if (!is.na(score)) {
                log_info(paste("Calculated alignment score from medians:", score))
                return(score)
              }
            } 
            
            # If no 'overall' column, take mean of all numeric columns
            numeric_cols <- sapply(medians, is.numeric)
            if (any(numeric_cols)) {
              score <- mean(as.matrix(medians[, numeric_cols, drop = FALSE]), na.rm = TRUE)
              if (!is.na(score)) {
                log_info(paste("Calculated alignment score from numeric columns:", score))
                return(score)
              }
            }
          }
        }
        
        # 4. If we still don't have a score, try to get it from the analysis store
        if (exists("project_data") && !is.null(project_data$analysis)) {
          # Try direct access to the score in the analysis store
          if (score_key %in% names(project_data$analysis)) {
            score <- safe_extract_numeric(project_data$analysis[[score_key]])
            if (!is.na(score)) {
              log_info(paste("Found score in analysis store:", score))
              return(score)
            }
          }
        }
        
        # 5. Last resort: look for any numeric value in the analysis result
        if (!is.null(analysis_result) && is.list(analysis_result)) {
          for (field in names(analysis_result)) {
            val <- analysis_result[[field]]
            if (is.numeric(val) && length(val) == 1 && !is.na(val)) {
              log_info(paste("Found numeric value in field", field, ":", val))
              return(as.numeric(val))
            }
          }
        }
        
        # If we get here, we couldn't find a valid score
        log_warn(paste("Could not find score for", analysis_type, score_key, "- returning NA"))
        return(NA)
      }
      
      # Initialize a reactive value to store the UI state
      output$results_ui <- renderUI({
        # Debug: Log the current state of project_data$analysis
        log_info("\n=== RENDERING RESULTS UI ===")
        log_info(paste("Last analysis type:", project_data$analysis$last_analysis_type))
        log_info(paste("Last updated:", project_data$analysis$last_updated))
        
        # Debug: Log the structure of the analysis results
        log_info("\n=== ANALYSIS RESULTS STRUCTURE ===")
        log_info("Alignment analyzed:", project_data$analysis$alignment_analyzed)
        log_info("Dynamics analyzed:", project_data$analysis$dynamics_analyzed)
        log_info("Cascade analyzed:", project_data$analysis$cascade_analyzed)
        
        # Safely log the structure without causing errors
        tryCatch({
          log_info("\nAlignment structure:")
          log_info(capture.output(str(project_data$analysis$alignment, max.level = 2)))
          if (!is.null(project_data$analysis$alignment_score)) {
            log_info(paste("Alignment score (flat):", project_data$analysis$alignment_score))
          }
          
          log_info("\nDynamics structure:")
          log_info(capture.output(str(project_data$analysis$dynamics, max.level = 2)))
          if (!is.null(project_data$analysis$dynamics_score)) {
            log_info(paste("Dynamics score (flat):", project_data$analysis$dynamics_score))
          }
          
          log_info("\nCascade structure:")
          log_info(capture.output(str(project_data$analysis$cascade, max.level = 2)))
        }, error = function(e) {
          log_warn(paste("Error logging analysis structure:", conditionMessage(e)))
        })
        
        # Initialize sections list to hold UI components
        sections <- list()
        
        # Helper to create a placeholder UI
        placeholder_ui <- function(title, message, icon = "warning-circle") {
          tags$div(
            class = "alert alert-info",
            ph(icon, weight = "bold"),
            tags$strong(title),
            tags$p(message)
          )
        }
        
        # Debug: Print cascade structure
        print(str(project_data$analysis$cascade))
        
        # Initialize UI components list
        ui_components <- list()
        
        # Add Alignment section if analyzed
        if (isTRUE(project_data$analysis$alignment_analyzed)) {
          log_info("Rendering alignment results")
          
          # Safely get alignment score with fallback
          alignment_score <- safe_get_score("alignment", "alignment_score")
          if (is.na(alignment_score) || is.null(alignment_score)) {
            alignment_score <- 0
            log_warn("Alignment score is NA or NULL, defaulting to 0")
          }
          
          # Ensure alignment_score is a single numeric value
          if (length(alignment_score) > 1) {
            alignment_score <- alignment_score[1]
            log_warn("Alignment score had multiple values, using first value:", alignment_score)
          }
          
          # Get ICC data if available
          icc_data <- project_data$analysis$alignment_icc
          has_icc <- !is.null(icc_data) && is.list(icc_data) && !is.null(icc_data$icc_value)
          
          # Create alignment section UI
          alignment_ui <- tags$fieldset(
            class = "custom-fieldset",
            style = "margin-bottom: 2rem; padding: 1rem; border: 1px solid #dee2e6; border-radius: 0.5rem;",
            tags$legend(
              class = "custom-legend", 
              style = "width: auto; padding: 0 0.5rem; font-size: 1.25rem; font-weight: 600; color: #495057;",
              "Alignment Analysis"
            ),
            
            # Score and ICC row
            fluidRow(
              # Alignment Score
              column(
                width = if (has_icc) 6 else 12,
                bslib::card(
                  bslib::card_header(
                    "Alignment Score",
                    bslib::tooltip(
                      bsicons::bs_icon("info-circle"),
                      "Overall alignment between project components",
                      placement = "right"
                    )
                  ),
                  bslib::card_body(
                    class = "text-center",
                    h1(
                      style = "font-size: 3.5rem; font-weight: bold; margin: 0; color: #0d6efd;",
                      format(round(alignment_score, 2), nsmall = 2)
                    ),
                    p(
                      style = "margin: 0.5rem 0 0 0; font-size: 1rem; color: #6c757d;",
                      "Overall alignment across all dimensions"
                    )
                  )
                )
              ),
              
              # ICC score if available
              if (has_icc) {
                column(
                  width = 6,
                  bslib::card(
                    bslib::card_header(
                      "Inter-Rater Reliability",
                      bslib::tooltip(
                        bsicons::bs_icon("info-circle"),
                        "Measures consistency between different raters",
                        placement = "right"
                      )
                    ),
                    bslib::card_body(
                      class = "text-center",
                      h3(
                        style = "font-size: 2.5rem; font-weight: bold; margin: 0; color: #0d6efd;",
                        format(round(icc_data$icc_value, 3), nsmall = 3)
                      ),
                      p(
                        style = "margin: 0.5rem 0 0 0; font-style: italic; color: #6c757d;",
                        icc_data$interpretation
                      )
                    )
                  )
                )
              }
            ),
            
            # Data table section
            if (!is.null(project_data$analysis$alignment_table) && 
                is.data.frame(project_data$analysis$alignment_table)) {
              fluidRow(
                column(
                  width = 12,
                  bslib::card(
                    bslib::card_header("Alignment by Role and Dimension"),
                    bslib::card_body(
                      div(
                        style = "overflow-x: auto;",
                        DT::dataTableOutput(ns("alignment_table"), width = "100%")
                      ),
                      div(
                        style = "text-align: center; margin-top: 1rem;",
                        actionButton(
                          ns("show_alignment_overlay"), 
                          "Expand Table", 
                          class = "btn-primary",
                          style = "margin-right: 0.5rem;"
                        ),
                        downloadButton(
                          ns("download_alignment"), 
                          "Download Data",
                          class = "btn-outline-primary"
                        )
                      )
                    )
                  )
                )
              )
            }
          )
          
          # Add to UI components
          ui_components <- c(ui_components, list(alignment_ui))
          
          # Add table rendering and download handlers
          output$alignment_table <- DT::renderDataTable({
            req(project_data$analysis$alignment_table)
            DT::datatable(
              project_data$analysis$alignment_table,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              class = 'cell-border stripe hover',
              selection = 'none'
            )
          })
          
          # Download handler for alignment data
          output$download_alignment <- downloadHandler(
            filename = function() {
              paste0("alignment_data_", Sys.Date(), ".csv")
            },
            content = function(file) {
              write.csv(project_data$analysis$alignment_table, file, row.names = FALSE)
            }
          )
        }
        
        # Return all UI components
        if (length(ui_components) == 0) {
          # No analysis results to show
          tagList(
            tags$div(
              class = "alert alert-info",
              ph("info", weight = "bold", class = "me-2"),
              "No analysis results available. Please run an analysis first."
            )
          )
        } else {
          # Return all UI components
          do.call(tagList, ui_components)
        }
      })
      
          # Add Dynamics section if analyzed
        if (isTRUE(project_data$analysis$dynamics_analyzed)) {
          log_info("Rendering dynamics results")
          
          # Safely get dynamics score with fallback
          dynamics_score <- safe_get_score("dynamics", "dynamics_score")
          if (is.na(dynamics_score) || is.null(dynamics_score)) {
            dynamics_score <- 0
            log_warn("Dynamics score is NA or NULL, defaulting to 0")
          }
          
          # Ensure dynamics_score is a single numeric value
          if (length(dynamics_score) > 1) {
            dynamics_score <- dynamics_score[1]
            log_warn("Dynamics score had multiple values, using first value:", dynamics_score)
          }
          
          log_info(paste("Dynamics score:", dynamics_score))
          
          # Get dimension scores with fallbacks
          dim_scores <- list(
            Contexts = 0,
            Partnership_Processes = 0,
            Engaged_Learning = 0,
            Outcomes = 0,
            Interventions_Research = 0
          )
          
          if (!is.null(project_data$analysis$dimension_scores)) {
            for (dim in names(dim_scores)) {
              if (dim %in% names(project_data$analysis$dimension_scores)) {
                dim_scores[[dim]] <- safe_round(project_data$analysis$dimension_scores[[dim]], 2)
              }
            }
          }

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
                    style = "margin-bottom: 0 !important; gap: 1rem !important;",
                    col_widths = c(6, 6),
                    # First column - Contexts
                    tags$div(
                      class = "d-flex align-items-start p-2",
                      style = "background-color: #f8f9fa; border-radius: 0.5rem;",
                      ph("polygon",
                        weight = "light", size = "1.5x",
                        style = "color: #6f42c1; margin-right: 0.75rem; margin-top: 0.2rem;"
                      ),
                      tags$div(
                        style = "width: 100%;",
                        tags$div(
                          class = "d-flex justify-content-between align-items-center",
                          tags$strong("Contexts", style = "color: #6f42c1; font-size: 0.9rem;"),
                          tags$span(
                            style = "background-color: #e9ecef; padding: 0.15rem 0.5rem; border-radius: 1rem; font-weight: 600; font-size: 0.85rem;",
                            dim_scores$Contexts
                          )
                        ),
                        div(
                          style = "height: 0.5rem; background-color: #e9ecef; border-radius: 0.25rem; margin-top: 0.5rem; overflow: hidden;",
                          div(
                            style = sprintf(
                              "width: %s%%; height: 100%%; background: linear-gradient(90deg, #6f42c1, #9b59b6); border-radius: 0.25rem; transition: width 0.5s ease-in-out;",
                              min(100, max(0, as.numeric(dim_scores$Contexts) * 20))
                            )
                          )
                        )
                      )
                    ),
                    # Second column - Partnership Processes
                    tags$div(
                      class = "d-flex align-items-start p-2",
                      style = "background-color: #f8f9fa; border-radius: 0.5rem;",
                      ph("handshake",
                        weight = "light", size = "1.5x",
                        style = "color: #6f42c1; margin-right: 0.75rem; margin-top: 0.2rem;"
                      ),
                      tags$div(
                        style = "width: 100%;",
                        tags$div(
                          class = "d-flex justify-content-between align-items-center",
                          tags$strong("Partnership", style = "color: #6f42c1; font-size: 0.9rem;"),
                          tags$span(
                            style = "background-color: #e9ecef; padding: 0.15rem 0.5rem; border-radius: 1rem; font-weight: 600; font-size: 0.85rem;",
                            dim_scores$Partnership_Processes
                          )
                        ),
                        div(
                          style = "height: 0.5rem; background-color: #e9ecef; border-radius: 0.25rem; margin-top: 0.5rem; overflow: hidden;",
                          div(
                            style = sprintf(
                              "width: %s%%; height: 100%%; background: linear-gradient(90deg, #6f42c1, #9b59b6); border-radius: 0.25rem; transition: width 0.5s ease-in-out;",
                              min(100, max(0, as.numeric(dim_scores$Partnership_Processes) * 20))
                            )
                          )
                        )
                      )
                    )
                  ),
                  layout_columns(
                    style = "margin: 1rem 0 !important; gap: 1rem !important;",
                    col_widths = c(12),
                    # Interventions/Research
                    tags$div(
                      class = "d-flex align-items-start p-2",
                      style = "background-color: #f8f9fa; border-radius: 0.5rem;",
                      ph("hand-heart",
                        weight = "light", size = "1.5x",
                        style = "color: #6f42c1; margin-right: 0.75rem; margin-top: 0.2rem;"
                      ),
                      tags$div(
                        style = "width: 100%;",
                        tags$div(
                          class = "d-flex justify-content-between align-items-center",
                          tags$strong("Interventions/Research", style = "color: #6f42c1; font-size: 0.9rem;"),
                          tags$span(
                            style = "background-color: #e9ecef; padding: 0.15rem 0.5rem; border-radius: 1rem; font-weight: 600; font-size: 0.85rem;",
                            dim_scores$Interventions_Research
                          )
                        ),
                        div(
                          style = "height: 0.5rem; background-color: #e9ecef; border-radius: 0.25rem; margin-top: 0.5rem; overflow: hidden;",
                          div(
                            style = sprintf(
                              "width: %s%%; height: 100%%; background: linear-gradient(90deg, #6f42c1, #9b59b6); border-radius: 0.25rem; transition: width 0.5s ease-in-out;",
                              min(100, max(0, as.numeric(dim_scores$Interventions_Research) * 20))
                            )
                          )
                        )
                      )
                    )
                  ),
                  layout_columns(
                    style = "margin-bottom: 0 !important; gap: 1rem !important;",
                    col_widths = c(6, 6),
                    # First column - Engaged Learning
                    tags$div(
                      class = "d-flex align-items-start p-2",
                      style = "background-color: #f8f9fa; border-radius: 0.5rem;",
                      ph("book-open-user",
                        weight = "light", size = "1.5x",
                        style = "color: #6f42c1; margin-right: 0.75rem; margin-top: 0.2rem;"
                      ),
                      tags$div(
                        style = "width: 100%;",
                        tags$div(
                          class = "d-flex justify-content-between align-items-center",
                          tags$strong("Engaged Learning", style = "color: #6f42c1; font-size: 0.9rem;"),
                          tags$span(
                            style = "background-color: #e9ecef; padding: 0.15rem 0.5rem; border-radius: 1rem; font-weight: 600; font-size: 0.85rem;",
                            dim_scores$Engaged_Learning
                          )
                        ),
                        div(
                          style = "height: 0.5rem; background-color: #e9ecef; border-radius: 0.25rem; margin-top: 0.5rem; overflow: hidden;",
                          div(
                            style = sprintf(
                              "width: %s%%; height: 100%%; background: linear-gradient(90deg, #6f42c1, #9b59b6); border-radius: 0.25rem; transition: width 0.5s ease-in-out;",
                              min(100, max(0, as.numeric(dim_scores$Engaged_Learning) * 20))
                            )
                          )
                        )
                      )
                    ),
                    # Second column - Outcomes
                    tags$div(
                      class = "d-flex align-items-start p-2",
                      style = "background-color: #f8f9fa; border-radius: 0.5rem;",
                      ph("chart-line-up",
                        weight = "light", size = "1.5x",
                        style = "color: #6f42c1; margin-right: 0.75rem; margin-top: 0.2rem;"
                      ),
                      tags$div(
                        style = "width: 100%;",
                        tags$div(
                          class = "d-flex justify-content-between align-items-center",
                          tags$strong("Outcomes", style = "color: #6f42c1; font-size: 0.9rem;"),
                          tags$span(
                            style = "background-color: #e9ecef; padding: 0.15rem 0.5rem; border-radius: 1rem; font-weight: 600; font-size: 0.85rem;",
                            dim_scores$Outcomes
                          )
                        ),
                        div(
                          style = "height: 0.5rem; background-color: #e9ecef; border-radius: 0.25rem; margin-top: 0.5rem; overflow: hidden;",
                          div(
                            style = sprintf(
                              "width: %s%%; height: 100%%; background: linear-gradient(90deg, #6f42c1, #9b59b6); border-radius: 0.25rem; transition: width 0.5s ease-in-out;",
                              min(100, max(0, as.numeric(dim_scores$Outcomes) * 20))
                            )
                          )
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
        if (isTRUE(project_data$analysis$cascade_analyzed)) {
          log_info("Rendering cascade results")

          # Safely get cascade score with fallback (from new structure)
          cascade_score <- safe_get_score("cascade", "cascade_score")
          # Try alternative keys if cascade_score is missing
          if (!is.null(project_data$analysis$cascade)) {
            if (!is.null(project_data$analysis$cascade$cascade_score) &&
              !is.na(project_data$analysis$cascade$cascade_score) &&
              length(project_data$analysis$cascade$cascade_score) > 0) {
              cascade_score <- as.numeric(project_data$analysis$cascade$cascade_score)
            } else if (!is.null(project_data$analysis$cascade$score) &&
              !is.na(project_data$analysis$cascade$score) &&
              length(project_data$analysis$cascade$score) > 0) {
              cascade_score <- as.numeric(project_data$analysis$cascade$score)
            } else {
              log_warn("No cascade score found, using NA")
            }
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

      # Render cascade score box
      output$cascade_score_box <- renderUI({
        req(project_data$analysis$cascade_analyzed, project_data$analysis$cascade_score)

        score <- tryCatch(
          as.numeric(project_data$analysis$cascade_score),
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
        req(project_data$analysis$cascade_analyzed, project_data$analysis$cascade_results)

        if (is.data.frame(project_data$analysis$cascade_results)) {
          # Show the main cascade results table (scores by layer)
          DT::datatable(
            project_data$analysis$cascade_results,
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
        req(project_data$analysis$cascade_analyzed, project_data$analysis$cascade_results)

        logger::log_info("[@mod_analyze.R] Cascade summary output rendered")

        if (is.data.frame(project_data$analysis$cascade_results)) {
          # Get summary of cascade results
          cascade_results <- project_data$analysis$cascade_results

          # Calculate basic statistics for numeric columns
          numeric_cols <- sapply(cascade_results, is.numeric)
          if (any(numeric_cols)) {
            logger::log_info("[@mod_analyze.R] Numeric columns summary rendered")
            print(summary(cascade_results[, numeric_cols, drop = FALSE]))
          }

          # Add overall score
          if (!is.null(project_data$analysis$cascade_score)) {
            logger::log_info("[@mod_analyze.R] Overall cascade score rendered")
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
          write.csv(project_data$analysis$cascade_results, file, row.names = FALSE)
        }
      )

      # Output value for dynamics_analyzed
      output$dynamics_analyzed <- reactive({
        isTRUE(project_data$analysis$dynamics_analyzed)
      })
      outputOptions(output, "dynamics_analyzed", suspendWhenHidden = FALSE)

      # Output value for cascade_analyzed
      output$cascade_analyzed <- reactive({
        isTRUE(project_data$analysis$cascade_analyzed)
      })
      outputOptions(output, "cascade_analyzed", suspendWhenHidden = FALSE)

      # Render dynamics score box
      output$dynamics_score_box <- renderUI({
        req(project_data$analysis$dynamics_analyzed, project_data$analysis$dynamics_score)

        score <- tryCatch(
          as.numeric(project_data$analysis$dynamics_score),
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
          req(project_data$analysis$dynamics_results)

          if (is.data.frame(project_data$analysis$dynamics_results)) {
            # Format the domain scores nicely
            domain_scores <- project_data$analysis$dynamics_results

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
        req(project_data$analysis$dynamics_results)

        cat("Dynamics Analysis Summary\n")
        cat("========================\n\n")

        if (is.data.frame(project_data$analysis$dynamics_results)) {
          # Get summary of domain scores
          domain_scores <- project_data$analysis$dynamics_results

          # Calculate basic statistics
          cat("Domain Score Statistics:\n")
          print(summary(domain_scores[, sapply(domain_scores, is.numeric), drop = FALSE]))

          # Add overall score
          cat(
            "\nOverall Dynamics Score: ",
            round(as.numeric(project_data$analysis$dynamics_score), 2), "\n"
          )
        } else {
          cat("No dynamics data available for summary")
        }
      })

      # Render full dynamics results table
      output$full_dynamics_table <- DT::renderDataTable({
        req(project_data$analysis$dynamics_analyzed)

        if (is.data.frame(project_data$analysis$dimension_scores)) {
          DT::datatable(
            project_data$analysis$dimension_scores,
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
          req(project_data$analysis$dynamics_analyzed)

          # Use full_results if available, otherwise fall back to dynamics_results
          data_to_download <- if (is.data.frame(project_data$analysis$full_results)) {
            project_data$analysis$alignment_results
          } else if (is.data.frame(project_data$analysis$dynamics_results)) {
            project_data$analysis$dynamics_results
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
          write.csv(project_data$analysis$dynamics_results, file, row.names = FALSE)
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
        req(project_data$analysis$alignment_analyzed, project_data$analysis$alignment_score)

        # Debug: Print the reactive values structure
        message("\n=== DEBUG: Alignment Score ===")
        message("Type of project_data$analysis$alignment_score: ", typeof(project_data$analysis$alignment_score))
        message("Class of project_data$analysis$alignment_score: ", class(project_data$analysis$alignment_score))
        message("Value of project_data$analysis$alignment_score: ", project_data$analysis$alignment_score)

        score <- tryCatch(
          {
            s <- as.numeric(project_data$analysis$alignment_score)
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
              project_data$analysis$alignment_score
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
          req(project_data$analysis$alignment_analyzed, project_data$analysis$alignment_medians)

          project_data$analysis$alignment_medians
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

      # Render alignment medians table (duplicate removed)
      output$alignment_medians_table <- renderTable(
        {
          req(project_data$analysis$alignment_analyzed, project_data$analysis$alignment_medians)

          project_data$analysis$alignment_medians
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
        req(project_data$analysis$alignment_analyzed, project_data$analysis$alignment_medians)

        if (is.data.frame(project_data$analysis$alignment_medians)) {
          # Format the alignment medians nicely
          alignment_data <- project_data$analysis$alignment_medians

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
          req(project_data$analysis$dynamics_analyzed, project_data$analysis$dynamics_results)

          project_data$analysis$dynamics_results
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
          write.csv(project_data$analysis$alignment_medians, file, row.names = FALSE)
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
          write.csv(project_data$analysis$alignment_medians, file, row.names = FALSE)
        }
      )

      # Generate summary statistics output - show ICC scores
      output$summary_statistics_output <- renderPrint({
        req(project_data$analysis$alignment_analyzed, project_data$analysis$icc_scores)

        cat("ICC Scores Summary\n")
        cat("=================\n\n")

        # Special handling for irr::icclist objects
        if (inherits(project_data$analysis$icc_scores, "icclist")) {
          print(project_data$analysis$icc_scores)
          return()
        }

        # Fallback for non-icclist objects or if printing fails
        if (is.list(project_data$analysis$icc_scores)) {
          # Try to extract common ICC values
          icc_value <- tryCatch(
            {
              project_data$analysis$icc_scores$value
            },
            error = function(e) NULL
          )

          if (!is.null(icc_value)) {
            cat("ICC Value:", icc_value, "\n\n")
            if (!is.null(project_data$analysis$icc_scores$lbound) && !is.null(project_data$analysis$icc_scores$ubound)) {
              cat(
                "95% CI:", project_data$analysis$icc_scores$lbound, "to",
                project_data$analysis$icc_scores$ubound, "\n"
              )
            }
            if (!is.null(project_data$analysis$icc_scores$p.value)) {
              cat("p-value:", project_data$analysis$icc_scores$p.value, "\n")
            }
            return()
          }
        }

        # If we get here, show the raw structure
        cat("ICC Scores (raw structure):\n")
        str(project_data$analysis$icc_scores)
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
        req(project_data$analysis$alignment_analyzed, project_data$analysis$alignment_medians)
        alignment_data <- project_data$analysis$alignment_medians
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
        req(project_data$analysis$dynamics_analyzed)
        data_to_show <- if (is.data.frame(project_data$analysis$full_results)) {
          project_data$analysis$alignment_results
        } else if (is.data.frame(project_data$analysis$dynamics_results)) {
          project_data$analysis$dynamics_results
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
        req(project_data$analysis$cascade_analyzed, project_data$analysis$cascade_results)
        DT::datatable(project_data$analysis$cascade_results, options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"), rownames = FALSE)
      })

      # Return analysis results
      return(project_data$analysis)
    } # Closes the 'module = function(...) {' block
  ) # Closes the 'moduleServer(...)' call
} # Closes the 'mod_analyze_server <- function(...) {' block
