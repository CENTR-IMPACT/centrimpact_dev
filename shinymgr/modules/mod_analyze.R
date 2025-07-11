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
#' @importFrom phosphoricons ph
#' @importFrom shiny NS tagList observe observeEvent reactive reactiveVal reactiveValues req debounce updateTextInput updateDateInput updateTextAreaInput renderUI
#' @importFrom shinyAce updateAceEditor
#' @importFrom logger log_info log_warn log_error log_trace
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom utils create_status_card create_actions_card create_info_card

# Enable glue syntax for logger
.onLoad <- function(libname, pkgname) {
  logger::log_formatter(logger::formatter_glue)
}

# the ui function
mod_analyze_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML(paste0(
        "#", ns("analyze_status_message"), " {
          display: block;
          padding-left: 0.5em;
          z-index: 10;
          width: 100%;
          font-family: 'Share Tech Mono' !important;
        }"
      )))
    ),
    bslib::navset_card_pill(
      id = "analyze_nav",
      bslib::nav_panel(
        "Overview",
        icon = phosphoricons::ph("lighthouse"),
        class = "main_content",
        fluidRow(
          style = "padding: 1em;",
          column(
            width = 8,
            h1(
              "Analyze Data",
              class = "analyze",
              style = "border-bottom: solid 1px #4A4A4A;"
            ),
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
          ),
        )
      ),
      bslib::nav_panel(
        "Analyze Data",
        icon = phosphoricons::ph("calculator"),
        class = "main_content",
        fluidRow(
          column(
            width = 7,
            create_info_card(
              ns,
              title = "Analyze Data", icon = "calculator",
              status = "analyze",
              content = tagList(
                p(
                  "This module allows you to analyze your project data. Select the type of analysis you want to perform and run the analysis. Use the buttons below to run each analysis."
                )
              )
            )
          ),
          column(
            width = 5,
            create_actions_card(
              ns,
              action_buttons = list(
                actionButton(
                  ns("analyze_alignment"),
                  label = tagList(
                    phosphoricons::ph("flower-lotus"),
                    "Analyze Alignment Data"
                  ),
                  class = "action-button action-alignment",
                  style = "width: 80%;"
                ),
                actionButton(ns("analyze_dynamics"),
                  label = tagList(
                    phosphoricons::ph("pulse"),
                    "Analyze Dynamics Data"
                  ),
                  class = "action-button action-dynamics",
                  style = "width: 80%;"
                ),
                actionButton(ns("analyze_cascade"),
                  label = tagList(
                    phosphoricons::ph("waveform"),
                    "Analyze Cascade Data"
                  ),
                  class = "action-button action-cascade",
                  style = "width: 80%;"
                ),
                actionButton(
                  ns("analyze_full"),
                  label = tagList(
                    phosphoricons::ph("calculator"),
                    "Run Full Analysis"
                  ),
                  class = "action-button action-full btn-lrg w-90",
                  style = "width: 80%;"
                ) |>
                  tooltip(
                    "Run all analyses in sequence"
                  )
              ),
              progress_bars = list(
                shinyWidgets::progressBar(
                  id = ns("progress_clean_data"),
                  title = tags$span("Clean Data Progress", class = "pb-title", style = "text-align: left !important;"),
                  value = 0,
                  display_pct = FALSE,
                  status = "success"
                ),
                NULL,
                NULL,
                NULL
              ),
              title = "Actions",
              icon = "person-simple-tai-chi"
            )
          )
        )
      ),
      # Data View Tabs
      bslib::nav_panel("Alignment",
        value = "alignment_panel", icon = phosphoricons::ph("flower-lotus"),
        class = "main_content",
          shiny::conditionalPanel(
            condition = "output.alignment_analyzed == false",
            ns = ns,
            create_flat_info_card(
              ns,
              title = "Alignment Analysis Results",
              icon = "flower-lotus",
              content = p("Please run the alignment analysis first by clicking the 'Analyze Alignment' button.")
            )
          ),
          shiny::conditionalPanel(
            condition = "output.alignment_analyzed == true",
            ns = ns,
            fluidRow(
              column(
                width = 12,
                style = "margin-top: 20px;",
                # Alignment Score
                uiOutput(ns("alignment_score_box"))
              )
            ),
            fluidRow(
              column(
                width = 6,
                style = "margin-top: 20px;",
                # Medians Table
                bslib::card(
                  bslib::card_header("Alignment Outcomes"),
                  bslib::card_body(
                    style = "font-family: var(--font-mono) !important;",
                    tableOutput(ns("alignment_medians_table")),
                    div(
                      style = "margin-bottom: 20px;",
                      downloadButton(ns("download_results"), "Download Results",
                        class = "btn-primary"
                      )
                    )
                )
              ),
              column(
                width = 6,
                style = "margin-top: 20px; font-family: var(--font-mono) !important;",
                bslib::card(
                  bslib::card_header(
                    "Summary Statistics",
                    icon = ph("table"),
                  ),
                  bslib::card_body(
                    verbatimTextOutput(ns("summary_statistics_output"))
                  )
                )
              )
            )
          )
        )
      ),
      bslib::nav_panel("Dynamics",
        value = "dynamics_panel",
        icon = phosphoricons::ph("pulse"),
        class = "main_content",
        fluidPage(
          fluidRow(
            column(
              width = 12,
              style = "margin-top: 20px;",
              uiOutput(ns("dynamics_analysis_ui"))
            )
          )
        )
      ),
      bslib::nav_panel("Cascade Effects",
        value = "cascade_panel", icon = phosphoricons::ph("waveform"),
        class = "main_content",
          shiny::conditionalPanel(
            condition = "!output.cascade_analyzed",
            ns = ns,
            create_flat_info_card(
              ns,
              title = "Cascade Analysis Results",
              icon = "waveform",
              content = p("Please run the cascade analysis first by clicking the 'Analyze Cascade' button.")
            )
          ),
          shiny::conditionalPanel(
            condition = "output.cascade_analyzed",
            ns = ns,
            fluidRow(
              column(
                width = 12,
                style = "margin-top: 20px;",
                uiOutput(ns("cascade_score_box"))
              )
            ),
            fluidRow(
              column(
                width = 12,
                style = "margin-top: 20px;",
                bslib::card(
                  bslib::card_header("Cascade Effects"),
                  bslib::card_body(
                    style = "font-family: var(--font-mono) !important;",
                    tableOutput(ns("cascade_results_table")),
                    div(
                      style = "margin-top: 20px;",
                      downloadButton(ns("download_cascade_results"), "Download Results",
                        class = "btn-primary"
                      )
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                style = "margin-top: 20px; font-family: var(--font-mono) !important;",
                bslib::card(
                  bslib::card_header("Summary Statistics"),
                  bslib::card_body(
                    verbatimTextOutput(ns("cascade_summary_output"))
                  )
                )
              )
            )
          )
      )
    )
  )
}




# the server function with enhanced debugging
mod_analyze_server <- function(id, ns_workflow) {
  message("=== mod_analyze_server CALLED with id: ", id, " ===")

  # Progress bar titles for updateProgressBar
  progress_titles <- list(
    alignment = tags$span("Alignment Analysis Progress", class = "pb-title", style = "text-align: left !important;"),
    dynamics = tags$span("Dynamics Analysis Progress", class = "pb-title", style = "text-align: left !important;"),
    cascade = tags$span("Cascade Analysis Progress", class = "pb-title", style = "text-align: left !important;"),
    full = tags$span("Full Analysis Progress", class = "pb-title", style = "text-align: left !important;")
  )

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

      # Reactive expressions to get data from ns_workflow
      alignment_data <- reactive({
        log_debug("Accessing alignment data reactively...")
        if (is.null(ns_workflow) || is.null(ns_workflow$alignment_data)) {
          log_warn("No alignment data available in ns_workflow")
          return(NULL)
        }
        log_info("Returning alignment data from reactive context")
        return(ns_workflow$alignment_data)
      })

      dynamics_data <- reactive({
        log_debug("Accessing dynamics data reactively...")
        if (is.null(ns_workflow) || is.null(ns_workflow$dynamics_data)) {
          log_warn("No dynamics data available in ns_workflow")
          return(NULL)
        }
        log_info("Returning dynamics data from reactive context")
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
        cascade_analyzed = FALSE,
        cascade_results = NULL,
        cascade_score = NULL
      )

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

      # Analyze dynamics button
      observeEvent(input$analyze_dynamics, {
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_dynamics"), value = 0, title = progress_titles$dynamics)
        log_info("=== ANALYZE DYNAMICS BUTTON CLICKED ===")

        # Get the data reactively
        data <- dynamics_data()

        # Check if data is available
        if (is.null(data)) {
          log_error("No dynamics data available for analysis")
          showNotification(
            "ERROR: Please load dynamics data in the 'Upload Data' tab first",
            type = "error",
            duration = 10
          )
          return()
        }

        # Validate the data
        if (!is.data.frame(data)) {
          msg <- glue::glue("dynamics_data() returned non-data.frame: {class(data)}")
          log_error(msg)
          showNotification(paste("ERROR:", msg),
            type = "error",
            duration = 10
          )
          return()
        }

        if (nrow(data) == 0) {
          log_warn("dynamics_data() returned empty data.frame")
          showNotification("WARNING: No data rows available for analysis",
            type = "warning",
            duration = 10
          )
          return()
        }

        # Show data info
        msg <- glue::glue("Analyzing dynamics data with {nrow(data)} rows and {ncol(data)} columns")
        log_info(msg)
        showNotification(msg, type = "message", duration = 5)

        # Run the analysis
        withProgress(message = "Analyzing project dynamics...", value = 0.5, {
          tryCatch(
            {
              # Run the dynamics analysis
              result <- centrimpact::analyze_dynamics(data)

              # Store results
              rv_analysis$dynamics_results <- result
              rv_analysis$dynamics_score <- result$dynamics_score
              rv_analysis$dynamics_analyzed <- TRUE

              # Switch to the dynamics panel
              updateNavbarPage(session, "analyze_tabs", selected = "dynamics_panel")

              showNotification("Dynamics analysis completed!",
                type = "message"
              )
            },
            error = function(e) {
              error_msg <- glue::glue("Error in dynamics analysis: {conditionMessage(e)}")
              log_error(error_msg)
              showNotification(error_msg, type = "error", duration = 10)
            }
          )
        })
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_dynamics"), value = 100, title = progress_titles$dynamics)
      })

      # Analyze alignment button
      observeEvent(input$analyze_alignment, {
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_alignment"), value = 0, title = progress_titles$alignment)
        log_info("=== ANALYZE ALIGNMENT BUTTON CLICKED ===")

        # Get the data reactively
        data <- alignment_data()

        # Check if data is available
        if (is.null(data)) {
          log_error("No alignment data available for analysis")
          showNotification(
            "ERROR: Please load alignment data in the 'Upload Data' tab first",
            type = "error",
            duration = 10
          )
          return()
        }

        # Step 4: Validate the data
        if (is.null(data)) {
          log_warn("alignment_data() returned NULL")
          showNotification("WARNING: No data available for analysis",
            type = "warning",
            duration = 10
          )
          return()
        }

        if (!is.data.frame(data)) {
          msg <- glue::glue("alignment_data() returned non-data.frame: {class(data)}")
          log_error(msg)
          showNotification(paste("ERROR:", msg),
            type = "error",
            duration = 10
          )
          return()
        }

        if (nrow(data) == 0) {
          log_warn("alignment_data() returned empty data.frame")
          showNotification("WARNING: No data rows available for analysis",
            type = "warning",
            duration = 10
          )
          return()
        }

        # Step 5: Success - show data info
        msg <- glue::glue("SUCCESS: Data has {nrow(data)} rows and {ncol(data)} columns")
        log_info(msg)
        log_info("Column names: {paste(names(data), collapse = ', ')}")
        showNotification(msg,
          type = "message",
          duration = 10
        )

        # Run the analysis using centrimpact::analyze_alignment
        log_info("Running centrimpact::analyze_alignment...")
        withProgress(message = "Analyzing alignment...", value = 0.5, {
          tryCatch(
            {
              # Verify data is in the correct long format
              log_info("Verifying data format...")
              required_cols <- c("role", "alignment", "rating")

              # Check for required columns
              missing_cols <- setdiff(required_cols, names(data))
              if (length(missing_cols) > 0) {
                stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
              }

              # Log data structure for debugging
              log_info(glue::glue(
                "Data structure: {nrow(data)} rows, {ncol(data)} columns. "
              ))
              log_info(glue::glue(
                "Roles: {paste(unique(data$role), collapse = ', ')}. "
              ))
              log_info(glue::glue(
                "Alignments: {length(unique(data$alignment))} unique values"
              ))

              # Run the analysis
              log_info("Calling centrimpact::analyze_alignment...")
              result <- centrimpact::analyze_alignment(data)

              # Debug: Log basic information about the result
              log_info("Analysis result class(es): ", paste(class(result), collapse = ", "))
              log_info("Result names: ", paste(names(result), collapse = ", "))

              # Log more detailed structure to console for debugging
              message("\n=== DEBUG: Analysis result structure ===")
              str(result)
              message("=== END DEBUG ===\n")

              # Validate the result structure
              if (!all(c("alignment_medians", "alignment_score") %in% names(result))) {
                stop("Unexpected result structure from analyze_alignment")
              }

              # Store results and update UI
              message("\n=== SETTING ALIGNMENT SCORE (ANALYSIS) ===")
              message("Type of result$alignment_score: ", typeof(result$alignment_score))
              message("Class of result$alignment_score: ", class(result$alignment_score))
              message("Value being set: ", result$alignment_score)
              rv_analysis$alignment_medians <- result$alignment_medians
              rv_analysis$alignment_score <- result$alignment_score

              # Store ICC score if it exists, otherwise log a warning
              if ("icc_score" %in% names(result)) {
                log_info("Storing ICC score from analysis result")
                rv_analysis$icc_scores <- result$icc_score
              } else {
                log_warn(
                  "No ICC score found in analysis result. Available elements: ",
                  paste(names(result), collapse = ", ")
                )
                rv_analysis$icc_scores <- NULL
              }
              rv_analysis$alignment_analyzed <- TRUE

              # Update workflow step for alignment analysis completion
              update_workflow_step(
                ns_workflow,
                stage = "Calculate Scores",
                status = "complete",
                session = session
              )

              # Update alignment workflow icons
              update_alignment_workflow_icons(ns_workflow, session, ns)

              # Show success message
              msg <- glue::glue("Analysis complete. Alignment score: {round(as.numeric(result$alignment_score), 3)}")
              log_info(msg)
              showNotification(msg, type = "message", duration = 5)

              # Log some debug info
              log_info(glue::glue("Alignment medians dimensions: {nrow(result$alignment_medians)}x{ncol(result$alignment_medians)}"))
              log_info(glue::glue("Alignment score: {result$alignment_score}"))
            },
            error = function(e) {
              error_msg <- glue::glue("Error in analyze_alignment: {conditionMessage(e)}")
              log_error(error_msg)
              showNotification(error_msg, type = "error", duration = 10)
              return()
            }
          )
        })
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_alignment"), value = 100, title = progress_titles$alignment)
      })

      # Full analysis observer - runs all analyses in sequence
      observeEvent(input$analyze_full, {
        # Reset all progress bars
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_full"), value = 0, title = progress_titles$full)
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_alignment"), value = 0, title = progress_titles$alignment)
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_dynamics"), value = 0, title = progress_titles$dynamics)
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_cascade"), value = 0, title = progress_titles$cascade)

        withProgress(message = "Running Full Analysis...", value = 0, {
          # 1. Run alignment analysis
          update_console(session, "Starting alignment analysis...", "info")
          incProgress(0.1, detail = "Running alignment analysis")
          shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_full"), value = 10, title = progress_titles$full)
          shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_alignment"), value = 25, title = progress_titles$alignment)

          tryCatch(
            {
              data <- alignment_data()
              if (is.null(data)) {
                stop("No alignment data available")
              }

              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_alignment"), value = 50, title = progress_titles$alignment)
              result <- centrimpact::analyze_alignment(data)

              message("\n=== SETTING ALIGNMENT SCORE (FULL ANALYSIS) ===")
              message("Type of result$alignment_score: ", typeof(result$alignment_score))
              message("Class of result$alignment_score: ", class(result$alignment_score))
              message("Value being set: ", result$alignment_score)

              rv_analysis$alignment_medians <- result$alignment_medians
              rv_analysis$alignment_score <- result$alignment_score
              rv_analysis$icc_scores <- result$icc_score
              rv_analysis$alignment_analyzed <- TRUE

              # Update workflow step for alignment analysis completion
              update_workflow_step(
                ns_workflow,
                stage = "Calculate Scores",
                status = "complete",
                session = session
              )

              # Update alignment workflow icons
              update_alignment_workflow_icons(ns_workflow, session, ns)

              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_alignment"), value = 100, title = progress_titles$alignment)
              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_full"), value = 33, title = progress_titles$full)

              update_console(
                session,
                sprintf("<U+2713> Alignment analysis complete (Score: %.2f)", result$alignment_score),
                "success"
              )
            },
            error = function(e) {
              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_alignment"), value = 100, title = progress_titles$alignment)
              update_console(
                session,
                paste("<U+2717> Alignment analysis failed:", e$message),
                "error"
              )
              return()
            }
          )

          # 2. Run dynamics analysis
          update_console(session, "Starting dynamics analysis...", "info")
          incProgress(0.4, detail = "Running dynamics analysis")
          shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_dynamics"), value = 25, title = progress_titles$dynamics)

          tryCatch(
            {
              data <- dynamics_data()
              if (is.null(data)) {
                stop("No dynamics data available")
              }

              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_dynamics"), value = 50, title = progress_titles$dynamics)
              result <- centrimpact::analyze_dynamics(data)
              rv_analysis$dynamics_results <- result$domain_df
              rv_analysis$dynamics_score <- result$dynamics_score
              rv_analysis$full_results <- result$dynamics_df
              rv_analysis$dynamics_analyzed <- TRUE

              # Update workflow step for dynamics analysis completion
              update_workflow_step(
                ns_workflow,
                stage = "Calculate Scores",
                status = "complete",
                session = session
              )

              # Update main data workflow icons for dynamics analysis completion
              update_main_data_workflow_icons("Calculate Scores", "complete", session, ns, ns_workflow)

              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_dynamics"), value = 100, title = progress_titles$dynamics)
              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_full"), value = 66, title = progress_titles$full)

              update_console(
                session,
                sprintf("<U+2713> Dynamics analysis complete (Score: %.2f)", result$dynamics_score),
                "success"
              )
            },
            error = function(e) {
              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_dynamics"), value = 100, title = progress_titles$dynamics)
              update_console(
                session,
                paste("<U+2717> Dynamics analysis failed:", e$message),
                "error"
              )
              return()
            }
          )

          # 3. Run cascade analysis
          update_console(session, "Starting cascade analysis...", "info")
          incProgress(0.8, detail = "Running cascade analysis")
          shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_cascade"), value = 25, title = progress_titles$cascade)

          tryCatch(
            {
              data <- cascade_data()
              if (is.null(data)) {
                stop("No cascade data available")
              }

              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_cascade"), value = 50, title = progress_titles$cascade)
              result <- centrimpact::analyze_cascade(data)
              rv_analysis$cascade_results <- result$cascade_df
              rv_analysis$cascade_score <- result$cascade_score
              rv_analysis$cascade_analyzed <- TRUE

              # Update workflow step for cascade analysis completion
              update_workflow_step(
                ns_workflow,
                stage = "Calculate Scores",
                status = "complete",
                session = session
              )

              # Update main data workflow icons for cascade analysis completion
              update_main_data_workflow_icons("Calculate Scores", "complete", session, ns, ns_workflow)

              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_cascade"), value = 100, title = progress_titles$cascade)
              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_full"), value = 100, title = progress_titles$full)

              update_console(
                session,
                sprintf("<U+2713> Cascade analysis complete (Score: %.2f)", result$cascade_score),
                "success"
              )
            },
            error = function(e) {
              shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_cascade"), value = 100, title = progress_titles$cascade)
              update_console(
                session,
                paste("<U+2717> Cascade analysis failed:", e$message),
                "error"
              )
              return()
            }
          )

          # Complete
          incProgress(1, detail = "Analysis complete!")
          update_console(session, "All analyses completed successfully!", "success")
        })
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_full"), value = 100, title = progress_titles$full)
      })

      # Cascade analysis observer
      observeEvent(input$analyze_cascade, {
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_cascade"), value = 0, title = progress_titles$cascade)
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_cascade"), value = 10, title = progress_titles$cascade)
        log_info("=== ANALYZE CASCADE BUTTON CLICKED ===")

        # Get the data reactively
        data <- cascade_data()

        # Check if data is available
        if (is.null(data)) {
          log_error("No cascade data available for analysis")
          showNotification(
            "ERROR: Please load cascade data in the 'Upload Data' tab first",
            type = "error",
            duration = 10
          )
          return()
        }

        # Validate the data
        if (!is.data.frame(data)) {
          msg <- glue::glue("cascade_data() returned non-data.frame: {class(data)}")
          log_error(msg)
          showNotification(paste("ERROR:", msg),
            type = "error",
            duration = 10
          )
          return()
        }

        if (nrow(data) == 0) {
          log_warn("cascade_data() returned empty data.frame")
          showNotification("WARNING: No data rows available for analysis",
            type = "warning",
            duration = 10
          )
          return()
        }

        # Show data info
        msg <- glue::glue("Analyzing cascade data with {nrow(data)} rows and {ncol(data)} columns")
        log_info(msg)
        showNotification(msg, type = "message", duration = 5)

        # Switch to the cascade panel immediately to show loading state
        # updateTabsetPanel(session, "analyze_tabs", selected = "cascade_panel")

        # Run the analysis
        withProgress(message = "Analyzing cascade effects...", value = 0.5, {
          tryCatch(
            {
              # Call analyze_cascade function
              result <- centrimpact::analyze_cascade(data)

              # Validate result structure
              if (!all(c("cascade_df", "cascade_score") %in% names(result))) {
                stop("Unexpected result structure from analyze_cascade. Expected 'cascade_df' and 'cascade_score'.")
              }

              # Store results
              rv_analysis$cascade_results <- result$cascade_df
              rv_analysis$cascade_score <- result$cascade_score
              rv_analysis$cascade_analyzed <- TRUE

              # Update workflow step for cascade analysis completion
              update_workflow_step(
                ns_workflow,
                stage = "Calculate Scores",
                status = "complete",
                session = session
              )

              # Update main data workflow icons for cascade analysis completion
              update_main_data_workflow_icons("Calculate Scores", "complete", session, ns, ns_workflow)

              # Show success message
              msg <- glue::glue("Cascade analysis complete. Score: {round(as.numeric(result$cascade_score), 2)}")
              log_info(msg)
              showNotification(msg, type = "message", duration = 5)
            },
            error = function(e) {
              error_msg <- glue::glue("Error in analyze_cascade: {conditionMessage(e)}")
              log_error(error_msg)
              showNotification(error_msg, type = "error", duration = 10)
            }
          )
        })
        shinyWidgets::updateProgressBar(session = session, id = session$ns("progress_analysis_cascade"), value = 100, title = progress_titles$cascade)
      })

      # Render dynamics analysis UI
      output$dynamics_analysis_ui <- renderUI({
        req(rv_analysis$dynamics_analyzed, rv_analysis$dynamics_results)

        tagList(
          h1("Project Dynamics Analysis"),
          hr(),

          # Score display
          fluidRow(
            column(
              width = 12,
              style = "margin-top: 20px;background-color: #f0e5d7; border-radius: 12px; box-shadow:  8px 8px 15px #d1c8b5; padding: 1rem;",
              uiOutput(ns("dynamics_score_box"))
            )
          ),

          # Domain scores table and summary
          fluidRow(
            column(
              width = 6,
              style = "margin-top: 20px;",
              bslib::card(
                bslib::card_header("Domain Scores"),
                bslib::card_body(
                  style = "margin-top: 20px; background-color: #f0e5d7; border-radius: 12px; box-shadow:  8px 8px 15px #d1c8b5; padding: 1rem; font-family: var(--font-mono) !important;",
                  tableOutput(ns("domain_scores_table")),
                  div(
                    style = "margin-top: 20px;",
                    downloadButton(ns("download_domain_scores"), "Download Domain Scores",
                      class = "btn-primary"
                    )
                  )
                )
              )
            ),
            column(
              width = 6,
              style = "margin-top: 20px; background-color: #f0e5d7; border-radius: 12px; box-shadow:  8px 8px 15px #d1c8b5; padding: 1rem; font-family: var(--font-mono) !important;",
              bslib::card(
                bslib::card_header("Dynamics Summary"),
                bslib::card_body(
                  verbatimTextOutput(ns("dynamics_summary_output"))
                )
              )
            )
          ),

          # Full dynamics results table
          fluidRow(
            column(
              width = 12,
              style = "margin-top: 30px;",
              div(
                bslib::card(
                  style = "margin-top: 20px;background-color: #f0e5d7; border-radius: 12px; box-shadow:  8px 8px 15px #d1c8b5;",
                  bslib::card_header("Detailed Dynamics Results"),
                  bslib::card_body(
                    style = "padding: 1rem;",
                    div(
                      style = "overflow-x: auto; width: 100%;",
                      DT::dataTableOutput(ns("full_dynamics_table"))
                    )
                  )
                ),
                div(
                  style = "margin-top: 15px; text-align: right;",
                  downloadButton(ns("download_dynamics"),
                    "Download Full Dynamics Data",
                    class = "btn-primary",
                    style = "background-color: #4a7c59; border-color: #3a6a4a;"
                  )
                )
              )
            )
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

      # Render cascade results table
      output$cascade_results_table <- renderTable(
        {
          req(rv_analysis$cascade_analyzed, rv_analysis$cascade_results)

          if (is.data.frame(rv_analysis$cascade_results)) {
            # Format the cascade results
            cascade_results <- rv_analysis$cascade_results

            # Round numeric columns to 2 decimal places
            numeric_cols <- sapply(cascade_results, is.numeric)
            if (any(numeric_cols)) {
              cascade_results[numeric_cols] <- round(cascade_results[numeric_cols], 2)
            }

            cascade_results
          } else {
            data.frame(Message = "No cascade results available")
          }
        },
        rownames = FALSE,
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE
      )

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
        req(rv_analysis$full_results)

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
        }
      })

      # Download handler for full dynamics data
      output$download_dynamics <- downloadHandler(
        filename = function() {
          paste0("dynamics_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
          req(rv_analysis$full_results)
          write.csv(rv_analysis$full_results, file, row.names = FALSE)
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

      # Download handler for results
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
