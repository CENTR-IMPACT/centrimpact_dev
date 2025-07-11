#!! ModName = mod_load_clean

# Debug message to verify module is loaded
logger::log_info("mod_load_clean module is being loaded")

# Import necessary functions
#' @importFrom dplyr filter select mutate rename case_when everything left_join group_by summarise ungroup arrange
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @importFrom stringr str_remove str_starts
#' @importFrom shiny showNotification HTML tagList tags NS icon
#' @importFrom shinyalert shinyalert
#' @importFrom utils str
#' @importFrom logger log_info log_error log_warn log_debug
#' @importFrom stats setNames
#' @importFrom phosphoricons ph
#' @importFrom bslib card_header card_body card
#' @importFrom yaml yaml.load as.yaml
#' @importFrom visNetwork visNetwork visOptions visPhysics visLayout visLegend
#' @importFrom DT datatable
#' @importFrom shinymgr.utils process_cascade
# !! ModDisplayName = Load Data
# !! ModDescription = This module handles the loading and cleaning of project and alignment data.
# !! ModCitation = Price, Jeremy.  (2025). mod_load_clean. [Source code].
# !! ModNotes = This module relies on helper functions from utils_clean_data.R and utils_clean_alignment_data.R
# !! ModActive = 1
# !! FunctionReturn = rv !! A reactive values list containing all cleaned data !! reactivevalues

# =================================================================================================
# UI Function
# =================================================================================================

mod_load_clean_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    tags$head(
      tags$style(HTML(paste0(
        "#", ns("upload_status_message"), " {
          display: block;
          padding-left: 0.5em;
          z-index: 10;
          width: 100%;
          font-family: 'Share Tech Mono' !important;
        }"
      )))
    ),
    bslib::navset_card_pill(
      id = ns("load_clean_tabs"),
      bslib::nav_panel(
        "Overview",
        class = "main_content",
        icon = phosphoricons::ph("lighthouse"),
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shiny::h1("Upload and Clean",
              class = "load",
              style = "border-bottom: solid 1px #4A4A4A;"
            ),
            shiny::p(
              "This module allows you to load and clean your project data
              efficiently. Follow the steps below to prepare your data for
              analysis.",
              style = "font-family: var(--font-sans) !important; padding:
              10px; margin-bottom: 20px;"
            )
          ),
          shiny::column(
            width = 4,
            shiny::img(
              src = "www/load.png", alt = "Data Load and Clean Image",
              style = "max-width: 100%; height: auto; margin-top: 20px;
              padding-left: 10%; padding-right: 10%;"
            )
          )
        )
      ),
      bslib::nav_panel(
        "Upload & Clean",
        value = "load_clean_panel",
        class = "main_content",
        icon = phosphoricons::ph("sparkle"),
        fluidRow(
          column(
            width = 7,
            create_info_card(
              ns,
              title = "Upload & Clean", icon = "sparkle",
              status = "upload",
              content = tagList(
                fluidRow(
                  p(
                    "Upload the main project data set containing all research team
              generated data, including:"
                  ),
                  tags$ul(
                    shiny::tags$li("Project indicators and metrics"),
                    shiny::tags$li("Cascade effects analysis")
                  )
                ),
                fluidRow(
                  class = "formrow",
                  shiny::fileInput(
                    ns("alignment_data_file"),
                    "Select Alignment Data File",
                    accept = c(".csv", "text/csv"),
                    buttonLabel = tagList(
                      phosphoricons::ph("flower-lotus", weight = "light"),
                      " Browse"
                    ),
                    placeholder = "No file selected",
                    width = "100%"
                  )
                ),
                fluidRow(
                  class = "formrow",
                  shiny::fileInput(
                    ns("main_data_file"),
                    "Select Main Data File",
                    accept = c(".csv", "text/csv"),
                    buttonLabel = tagList(
                      phosphoricons::ph("database", weight = "light"),
                      " Browse"
                    ),
                    placeholder = "No file selected",
                    width = "100%"
                  )
                )
              )
            )
          ),
          column(
            width = 5,
            create_actions_card(
              ns,
              action_buttons = list(
                shiny::actionButton(
                  ns("clean_data"),
                  label = tagList(
                    "Clean Data",
                    ph("spray-bottle", weight = "bold")
                  ),
                  class = "action-button action-clean",
                  style = "width: 80%;"
                )
              ),
              progress_bars = list(
                shinyWidgets::progressBar(
                  id = ns("progress_clean_data"),
                  title = tags$span("Clean Data Progress", class = "pb-title", style = "text-align: left !important;"),
                  value = 0,
                  display_pct = FALSE,
                  status = "success"
                )
              ),
              title = "Actions",
              icon = "person-simple-tai-chi"
            ),
          )
        )
      ),
      bslib::nav_panel(
        title = "Indicators",
        value = "indicators_panel",
        icon = phosphoricons::ph("gauge"),
        shiny::uiOutput(ns("indicators_ui"))
      ),
      bslib::nav_panel(
        title = "Alignment",
        value = "alignment_panel",
        icon = phosphoricons::ph("flower-lotus"),
        shiny::uiOutput(ns("alignment_ui"))
      ),
      bslib::nav_panel(
        title = "Dynamics",
        value = "dynamics_panel",
        icon = phosphoricons::ph("pulse"),
        shiny::uiOutput(ns("dynamics_ui"))
      ),
      bslib::nav_panel(
        title = "Cascade Effects",
        value = "cascade_panel",
        icon = phosphoricons::ph("waveform"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::div(
              style = "display: flex; justify-content: space-between;
              align-items: center; margin-top: 20px;",
              shiny::h4("Network Parameters", style = "margin: 0;"),
              shiny::div(
                shiny::downloadButton(ns("download_cascade_yaml"),
                  label = "Download Parameters",
                  icon = shiny::icon("download"),
                  class = "btn-sm",
                  style = "background-color: #3B6B35; color: white;
                  border: none;"
                )
              )
            ),
            shinyAce::aceEditor(
              outputId = ns("cascade_yaml_editor"),
              mode = "yaml",
              theme = "github",
              height = "520px",
              fontSize = 14,
              debounce = 750,
              autoScrollEditorIntoView = TRUE,
              highlightActiveLine = TRUE
            )
          ),
          shiny::column(
            width = 6,
            shiny::uiOutput(ns("network_plot_ui")),
            # Add legend
            shiny::div(
              style = "margin-top: 20px; text-align: center;",
              shiny::h5("Legend"),
              shiny::div(
                style = "display: flex; justify-content: center;
                flex-wrap: wrap; gap: 15px;",
                shiny::div(
                  style = "display: flex; align-items: center; margin: 5px;",
                  shiny::div(
                    style = "width: 15px; height: 15px; background-color:
                    #1E325C; margin-right: 5px; border-radius: 3px;"
                  ),
                  shiny::span("Researcher")
                ),
                shiny::div(
                  style = "display: flex; align-items: center; margin: 5px;",
                  shiny::div(
                    style = "width: 15px; height: 15px; background-color:
                    #3B6B35; margin-right: 5px; border-radius: 3px;"
                  ),
                  shiny::span("Community Member")
                ),
                shiny::div(
                  style = "display: flex; align-items: center; margin: 5px;",
                  shiny::div(
                    style = "width: 15px; height: 15px; background-color:
                    #A64B42; margin-right: 5px; border-radius: 3px;"
                  ),
                  shiny::span("2nd Degree")
                ),
                shiny::div(
                  style = "display: flex; align-items: center; margin: 5px;",
                  shiny::div(
                    style = "width: 15px; height: 15px; background-color:
                    #E0D0A6; margin-right: 5px; border-radius: 3px;"
                  ),
                  shiny::span("3rd Degree")
                )
              )
            )
          )
        )
      )
    ),

    # Add JavaScript for icon highlighting and status updates
    shiny::tags$script(shiny::HTML(paste0("
      Shiny.addCustomMessageHandler('highlightWorkflowIcon', function(message) {
        var iconId = message.icon_id;
        var color = message.color;

        // Set the color of the icon
        if (document.getElementById(iconId)) {
          document.getElementById(iconId).style.color = color;
          console.log('Updated icon ' + iconId + ' to color ' + color);
        } else {
          console.warn('Element with ID ' + iconId + ' not found');
        }
      });

      Shiny.addCustomMessageHandler('updateAlignmentStatus', function(message) {
        console.log('Received updateAlignmentStatus message:', message);
        var element = document.getElementById(message.id);
        console.log('Looking for element with ID:', message.id);
        console.log('Element found:', element);

        if (element) {
          // Update text content only
          element.textContent = message.text;

          // Update the color
          element.style.color = message.color;

          console.log('Updated alignment status to: ' + message.text +
          ' with color ' + message.color);
        } else {
          console.warn('Element with ID ' + message.id + ' not found');
          console.log('Available elements with alignment_status_display:');
          var allElements = document.querySelectorAll(
            '[id*=\"alignment_status_display\"]'
          );
          for (var i = 0; i < allElements.length; i++) {
            console.log('Found element:', allElements[i].id);
          }
        }
      });
    ")))
  )
}

# =================================================================================================
# Server Function
# =================================================================================================
mod_load_clean_server <- function(id, ns_workflow) {
  message("=== mod_load_clean_server CALLED with id: ", id, " ===")

  # Debug messages
  cat("mod_load_clean_server starting with id:", id, "\n")
  cat("Testing if basic navset functions are working\n")

  # Ensure ns_workflow is properly initialized
  if (missing(ns_workflow) || is.null(ns_workflow)) {
    message("Creating new reactive values for ns_workflow")
    ns_workflow <- reactiveValues(
      stage = "Upload Data",
      status = "not started",
      alignment_data = NULL,
      dynamics_data = NULL,
      cascade_data = NULL
    )
  }

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track status for all indicators
    project_status <- reactiveVal("empty") # empty, circle, or check-circle
    alignment_status <- reactiveVal("empty")
    pulse_status <- reactiveVal("empty")
    waveform_status <- reactiveVal("empty")

    # Update project status when main data file is selected
    observeEvent(input$main_data_file, {
      req(input$main_data_file)
      project_status("circle")
      pulse_status("circle")
      waveform_status("circle")
      rv$main_data_file <- input$main_data_file
      logger::log_info("Main data file stored for cleaning: {input$main_data_file$name}")

      # Check if both files are now uploaded
      if (!is.null(rv$alignment_data_file)) {
        rv$both_files_uploaded <- TRUE
        logger::log_info("Both files are now uploaded")
      } else {
        # Mark as partially uploaded even without both files
        update_workflow_step(
          ns_workflow,
          stage = "Upload Data",
          status = "in progress",
          session = session
        )
        logger::log_info("Upload Data marked as in progress - main data file uploaded")
      }
    })

    # Update alignment status when file is selected (keep icon gray until validation)
    observeEvent(input$alignment_data_file, {
      if (!is.null(input$alignment_data_file)) {
        # Set to circle state for local status
        alignment_status("circle")
        # Note: workflow icon will stay gray until successful validation
      } else {
        alignment_status("empty")
      }
    })

    # Create workflow observers using centralized utility functions
    create_workflow_observers(ns_workflow, ns, session)

    # Debug workflow state at initialization - safe to call directly now with isolate() fix
    debug_workflow_state(ns_workflow, "mod_load_clean_init")

    # Create status outputs
    output$debug_output <- renderText({
      paste(
        "Debugging information:",
        "\nModule ID:", id,
        "\nMain file:", ifelse(!is.null(rv$main_data_file), "Uploaded", "Not uploaded"),
        "\nAlignment file:", ifelse(!is.null(rv$alignment_data_file), "Uploaded", "Not uploaded")
      )
    })

    output$project_status <- renderText({
      if (!is.null(rv$main_data_file)) {
        "File uploaded"
      } else {
        "No file uploaded"
      }
    })

    output$alignment_status <- renderText({
      if (!is.null(rv$alignment_data_file)) {
        "File uploaded"
      } else {
        "No file uploaded"
      }
    })

    output$status_text <- renderUI({
      main_status <- if (!is.null(rv$main_data_file)) "Uploaded" else "Not uploaded"
      alignment_status <- if (!is.null(rv$alignment_data_file)) "Uploaded" else "Not uploaded"
      both_ready <- !is.null(rv$main_data_file) && !is.null(rv$alignment_data_file)

      tags$div(
        tags$p(
          "Overall: ",
          if (both_ready) {
            tags$span(style = "color:green; font-weight:bold;", "&#x2713; Both files uploaded")
          } else if (!is.null(rv$main_data_file) || !is.null(rv$alignment_data_file)) {
            tags$span(style = "color:#3F5E78; font-weight:bold;", "&#x27F3; Partial upload (click Clean to process available file)")
          } else {
            tags$span(style = "color:#990000; font-weight:bold;", "&#x2717; No files uploaded")
          }
        ),
        tags$p(
          "Main Data: ",
          if (!is.null(rv$main_data_file)) {
            tags$span(style = "color:green;", "&#x2713; ", rv$main_data_file$name)
          } else {
            tags$span(style = "color:#990000;", "&#x2717; Missing")
          }
        ),
        tags$p(
          "Alignment Data: ",
          if (!is.null(rv$alignment_data_file)) {
            tags$span(style = "color:green;", "&#x2713; ", rv$alignment_data_file$name)
          } else {
            tags$span(style = "color:#990000;", "&#x2717; Missing")
          }
        ),
        if (both_ready) {
          tags$p(
            style = "margin-top:10px; border-top:1px solid #ddd; padding-top:5px;",
            "Ready to proceed with ", tags$b("Clean Data"), " step!"
          )
        } else if (!is.null(rv$main_data_file) || !is.null(rv$alignment_data_file)) {
          tags$p(
            style = "margin-top:10px; color:#3F5E78; border-top:1px solid #ddd; padding-top:5px;",
            "You can proceed with partial cleaning of the uploaded file(s).",
            tags$br(),
            tags$span(style = "font-style:italic; font-size:0.9em;", "Note: Workflow status will remain 'in progress' until both files are uploaded.")
          )
        } else {
          tags$p(
            style = "margin-top:10px; color:#990000; border-top:1px solid #ddd; padding-top:5px;",
            "Please upload at least one file to proceed."
          )
        }
      )
    })

    # Add clean status output
    # Render status icon with colors
    render_status_icon <- function(status) {
      color <- switch(status,
        "circle" = "#3F5E78",
        "check-circle" = "#4B7F52",
        "#6c757d" # Default gray for empty state
      )
      tags$span(style = paste0("color:", color), ph(status, weight = "bold"))
    }

    # Render all status icons
    output$project_status_icon <- renderUI(render_status_icon(project_status()))
    output$alignment_status_icon <- renderUI(render_status_icon(alignment_status()))
    output$pulse_status_icon <- renderUI(render_status_icon(pulse_status()))
    output$waveform_status_icon <- renderUI(render_status_icon(waveform_status()))

    # Initialize alignment data in ns_workflow if it doesn't exist
    observe({
      if (is.null(ns_workflow$alignment_data)) {
        ns_workflow$alignment_data <- NULL
      }
    })

    # Store the workflow object in a reactive value for consistency
    rv <- reactiveValues(
      main_data_file = NULL,
      alignment_data_file = NULL,
      indicators = NULL,
      alignment = NULL,
      dynamics = NULL,
      cascade = NULL,
      both_files_uploaded = FALSE,
      partial_cleaning = FALSE,
      workflow = ns_workflow # Store the workflow object (for backward compatibility)
    )

    # Initialize workflow values immediately if they don't exist
    isolate({
      # Direct initialization without reactive context
      # Already initialized as a named list above
      # Ensure alignment upload icon starts with correct default color (#D3D3D3)
      session$sendCustomMessage(
        "highlightWorkflowIcon",
        list(icon_id = ns("alignment-upload"), color = "#D3D3D3")
      )

      # Initialize main data workflow icons to gray
      main_data_types <- c("indicators", "dynamics", "cascade")
      for (data_type in main_data_types) {
        # Initialize upload icons to gray
        session$sendCustomMessage(
          "highlightWorkflowIcon",
          list(icon_id = ns(paste0(data_type, "-upload")), color = "#D3D3D3")
        )
        # Initialize clean icons to gray
        session$sendCustomMessage(
          "highlightWorkflowIcon",
          list(icon_id = ns(paste0(data_type, "-clean")), color = "#D3D3D3")
        )
      }
    })

    # Initialize all workflow icons - wait for session to be fully established
    shiny::observeEvent(session$clientData$url_hostname,
      {
        # Wait a short time to ensure all reactive values are properly initialized
        shiny::invalidateLater(800)
        logger::log_info("Session established, initializing workflow icons")
        # Safe initialization of workflow icons
        tryCatch(
          {
            if (!is.null(ns_workflow)) {
              debug_workflow_state(ns_workflow, "mod_load_clean_icon_init")
              # Workflow icons are now updated centrally by the main server observer
              logger::log_info("Workflow icons initialization skipped - handled centrally")
              logger::log_info("Workflow icons initialized successfully")
            } else {
              logger::log_warn("Cannot initialize workflow icons - ns_workflow is NULL")
            }
          },
          error = function(e) {
            logger::log_error("Error initializing workflow icons: {conditionMessage(e)}")
          }
        )
      },
      once = TRUE
    )

    # Observer to store main data file info when uploaded
    observeEvent(input$main_data_file, {
      req(input$main_data_file)
      # Defensive check to ensure input$main_data_file has the expected structure
      if (is.null(input$main_data_file$name) || is.null(input$main_data_file$datapath)) {
        logger::log_warn("input$main_data_file is not structured as expected.")
        return()
      }
      rv$main_data_file <- input$main_data_file
      logger::log_info("Main data file stored for cleaning: {input$main_data_file$name}")
    })

    # Observer to store alignment data file info when uploaded
    observeEvent(input$alignment_data_file, {
      req(input$alignment_data_file)
      # Defensive check
      if (is.null(input$alignment_data_file$name) || is.null(input$alignment_data_file$datapath)) {
        logger::log_warn("input$alignment_data_file is not structured as expected.")
        return()
      }
      rv$alignment_data_file <- input$alignment_data_file
      logger::log_info("Alignment data file stored for cleaning: {input$alignment_data_file$name}")
    })

    # Combined observer for cleaning data
    observeEvent(input$clean_data, {
      logger::log_info("Clean Data button clicked")

      # Check if any file has been uploaded
      if (is.null(rv$main_data_file) && is.null(rv$alignment_data_file)) {
        shinyalert::shinyalert(
          title = "No Files Uploaded",
          text = "Please upload at least one data file before cleaning.",
          type = "warning",
          confirmButtonText = "OK",
          confirmButtonCol = "#3F5E78"
        )
        return()
      }

      # Determine if this is a partial clean
      is_partial_clean <- is.null(rv$main_data_file) || is.null(rv$alignment_data_file)
      rv$partial_cleaning <- is_partial_clean
      logger::log_info("Partial cleaning status: {is_partial_clean}")

      # Update workflow status to "in progress"
      update_workflow_step(
        ns_workflow,
        stage = "Upload Data",
        status = "in progress",
        session = session
      )

      # --- Clean Main Data if available ---
      if (!is.null(rv$main_data_file)) {
        tryCatch(
          {
            logger::log_info("Starting to clean main project data...")
            raw_data <- readr::read_csv(rv$main_data_file$datapath, show_col_types = FALSE)

            # Clean the data using the centralized function
            cleaned_data_list <- clean_data(raw_data)

            # Assign cleaned data to reactive values
            rv$indicators <- cleaned_data_list$indicators
            rv$dynamics <- cleaned_data_list$dynamics
            rv$cascade <- cleaned_data_list$cascade

            # Update ns_workflow with the cleaned data
            ns_workflow$indicators_data <- cleaned_data_list$indicators
            ns_workflow$dynamics_data <- cleaned_data_list$dynamics
            ns_workflow$cascade_data <- cleaned_data_list$cascade

            # Update workflow icons to green
            main_data_types <- c("indicators", "dynamics", "cascade")
            for (data_type in main_data_types) {
              logger::log_info("Sub-stage '{data_type}' completed successfully")
            }

            logger::log_info("Main project data cleaned and stored successfully.")
            shiny::showNotification("Main project data cleaned successfully!", type = "message")
          },
          error = function(e) {
            logger::log_error("Error cleaning main data: {conditionMessage(e)}")
            shinyalert::shinyalert(
              title = "Error Cleaning Main Data",
              text = paste("An error occurred while cleaning the main data file:", e$message),
              type = "error"
            )
            # Update workflow icons to red (error)
            main_data_types <- c("indicators", "dynamics", "cascade")
            for (data_type in main_data_types) {
              logger::log_error("Sub-stage '{data_type}' failed")
            }
          }
        )
      }

      # --- Clean Alignment Data if available ---
      if (!is.null(rv$alignment_data_file)) {
        tryCatch(
          {
            logger::log_info("Starting to clean alignment data...")
            alignment_raw <- readr::read_csv(rv$alignment_data_file$datapath, show_col_types = FALSE)

            # Clean the alignment data
            cleaned_alignment <- clean_alignment_data(alignment_raw)
            rv$alignment <- cleaned_alignment
            ns_workflow$alignment_data <- cleaned_alignment

            # Update workflow for alignment
            logger::log_info("Sub-stage 'alignment' completed successfully")

            logger::log_info("Alignment data cleaned and stored successfully.")
            shiny::showNotification("Alignment data cleaned successfully!", type = "message")
          },
          error = function(e) {
            logger::log_error("Error cleaning alignment data: {conditionMessage(e)}")
            shinyalert::shinyalert(
              title = "Error Cleaning Alignment Data",
              text = paste("An error occurred while cleaning the alignment data file:", e$message),
              type = "error"
            )
            # Update workflow for alignment to error state
            logger::log_error("Sub-stage 'alignment' failed")
          }
        )
      }

      # --- Final Workflow Status Update ---
      # Check if both files were processed successfully
      main_data_ok <- !is.null(rv$main_data_file) && !is.null(rv$indicators)
      alignment_data_ok <- !is.null(rv$alignment_data_file) && !is.null(rv$alignment)

      if ((is.null(rv$main_data_file) || main_data_ok) && (is.null(rv$alignment_data_file) || alignment_data_ok)) {
        # If all available files are processed, determine overall status
        if (!is_partial_clean) {
          update_workflow_step(
            ns_workflow,
            stage = "Upload Data",
            status = "complete",
            session = session
          )
          logger::log_info("Both files cleaned. Workflow step 'Upload Data' marked as complete.")
        } else {
          logger::log_info("Partial cleaning complete. Workflow step 'Upload Data' remains 'in progress'.")
        }
      } else {
        # If there was an error with any file
        update_workflow_step(
          ns_workflow,
          stage = "Upload Data",
          status = "error",
          session = session
        )
        logger::log_warn("Errors occurred during cleaning. Workflow step 'Upload Data' marked as error.")
      }

      # Navigate to the first available data panel
      if (!is.null(rv$indicators)) {
        bslib::nav_select("load_clean_tabs", "indicators_panel")
      } else if (!is.null(rv$alignment)) {
        bslib::nav_select("load_clean_tabs", "alignment_panel")
      }

      # Update progress bar
      shinyWidgets::updateProgressBar(
        session = session,
        id = session$ns("progress_clean_data"),
        value = 10,
        title = tags$span("Clean Data Progress", class = "pb-title", style = "text-align: left !important;")
      )

      # Simulate data validation (replace with your actual validation logic)
      # ...
      shinyWidgets::updateProgressBar(
        session = session,
        id = session$ns("progress_clean_data"),
        value = 50,
        title = tags$span("Clean Data Progress", class = "pb-title", style = "text-align: left !important;")
      )

      # Simulate cleaning process (replace with your actual cleaning logic)
      # ...
      shinyWidgets::updateProgressBar(
        session = session,
        id = session$ns("progress_clean_data"),
        value = 100,
        title = tags$span("Clean Data Progress", class = "pb-title", style = "text-align: left !important;")
      )
    })

    # --- UI Rendering for Data Tables ---
    output$indicators_ui <- renderUI({
      if (is.null(rv$indicators)) {
        return(tags$div(class = "data-placeholder", "Indicators data will be displayed here after cleaning."))
      }
      DT::datatable(rv$indicators, options = list(scrollX = TRUE, pageLength = 10), class = "cell-border stripe")
    })

    output$alignment_ui <- renderUI({
      if (is.null(rv$alignment)) {
        return(tags$div(class = "data-placeholder", "Alignment data will be displayed here after cleaning."))
      }
      DT::datatable(rv$alignment, options = list(scrollX = TRUE, pageLength = 10), class = "cell-border stripe")
    })

    output$dynamics_ui <- renderUI({
      if (is.null(rv$dynamics)) {
        return(tags$div(class = "data-placeholder", "System dynamics data will be displayed here after cleaning."))
      }
      DT::datatable(rv$dynamics, options = list(scrollX = TRUE, pageLength = 10), class = "cell-border stripe")
    })

    # --- Cascade Effects YAML and Network Plot ---
    # Load default YAML content
    observe({
      req(ns_workflow$cascade_data)
      tryCatch(
        {
          # Use the YAML model from cascade analysis if available
          if (!is.null(ns_workflow$cascade_data$model)) {
            yaml_content <- yaml::as.yaml(ns_workflow$cascade_data$model)
            logger::log_info("Cascade YAML editor populated from cascade analysis model.")
          } else {
            yaml_content <- generate_cascade_yaml(ns_workflow$cascade_data)
            logger::log_info("Cascade YAML editor populated with default template.")
          }
          shinyAce::updateAceEditor(session, "cascade_yaml_editor", value = yaml_content)
        },
        error = function(e) {
          logger::log_error("Error generating or updating cascade YAML: {conditionMessage(e)}")
          shiny::showNotification("Failed to generate cascade parameters.", type = "error")
        }
      )
    })

    # Download handler for YAML
    output$download_cascade_yaml <- downloadHandler(
      filename = function() {
        paste0("cascade-parameters-", Sys.Date(), ".yml")
      },
      content = function(file) {
        writeLines(input$cascade_yaml_editor, file)
      }
    )

    # UI for network plot
    output$network_plot_ui <- renderUI({
      req(ns_workflow$cascade_data)
      visNetwork::visNetworkOutput(ns("network_plot"), height = "600px")
    })

    # Render the network plot
    output$network_plot <- visNetwork::renderVisNetwork({
      req(input$cascade_yaml_editor)
      tryCatch(
        {
          params <- yaml::yaml.load(input$cascade_yaml_editor)
          # Fallback to default colors if missing from YAML
          if (is.null(params$network_colors)) {
            params$network_colors <- list(
              researcher = "#1E325C",
              community = "#3B6B35",
              second_degree = "#A64B42",
              third_degree = "#E0D0A6"
            )
          }
          edges <- ns_workflow$cascade_data$edges

          # Use the nodes data frame from cascade analysis, which includes correct roles
          nodes <- ns_workflow$cascade_data$nodes
          nodes$label <- as.character(nodes$id)
          nodes$group <- nodes$role
          nodes$title <- paste0("ID: ", nodes$id, "<br>Role: ", nodes$role)
          nodes$id <- as.character(nodes$id)
          edges$from <- as.character(edges$from)
          edges$to <- as.character(edges$to)

          # Create the network
          visNetwork::visNetwork(nodes, edges) |>
            visNetwork::visGroups(groupname = "Researcher", color = params$network_colors$researcher) |>
            visNetwork::visGroups(groupname = "Community Member", color = params$network_colors$community) |>
            visNetwork::visGroups(groupname = "2nd Degree", color = params$network_colors$second_degree) |>
            visNetwork::visGroups(groupname = "3rd Degree", color = params$network_colors$third_degree) |>
            visNetwork::visOptions(
              highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
              nodesIdSelection = TRUE
            ) |>
            visNetwork::visPhysics(
              solver = "forceAtlas2Based",
              forceAtlas2Based = list(
                gravitationalConstant = -200,
                centralGravity = 0.05,
                springLength = 150,
                springConstant = 0.12,
                damping = 0.4
              ),
              stabilization = list(enabled = TRUE, iterations = 1500)
            ) |>
            visNetwork::visLayout(randomSeed = 42)
        },
        error = function(e) {
          showNotification(
            paste("Error creating network:", e$message),
            type = "error",
            duration = NULL
          )
          return(NULL)
        }
      )
    })

    # Return reactive values for use in other modules
    return(rv)
  })
}

# =================================================================================================
# Helper Functions
# =================================================================================================

#' Clean Main Project Data
#'
#' This function takes the raw project data and splits it into three separate
#' data frames: indicators, dynamics, and cascade effects.
#'
#' @param raw_data A data frame containing the raw project data.
#' @return A list of three data frames: `indicators`, `dynamics`, and `cascade`.
clean_data <- function(raw_data) {
  # Clean Indicators Data
  indicators <- raw_data %>%
    dplyr::filter(stringr::str_starts(indicator, "CIMP_")) %>%
    dplyr::select(-metric) %>%
    dplyr::rename(
      impact_indicator_code = indicator,
      impact_indicator_name = impact_indicator
    ) %>%
    dplyr::mutate(
      value = as.numeric(value),
      year = as.integer(year)
    )

  # Clean System Dynamics Data
  dynamics <- raw_data %>%
    dplyr::filter(stringr::str_starts(indicator, "DYN_")) %>%
    dplyr::select(indicator, metric, value, year) %>%
    dplyr::rename(
      dynamics_indicator_code = indicator,
      dynamics_indicator_name = metric,
      dynamics_value = value
    ) %>%
    dplyr::mutate(
      dynamics_value = as.numeric(dynamics_value),
      year = as.integer(year)
    )

  # Clean Cascade Effects Data using process_cascade from utils_clean_data.R
  # First, check if the function is available; if not, source it
  if (!exists("process_cascade")) {
    source("shinymgr/utils/utils_clean_data.R")
  }
  # Prepare a Qualtrics-like structure for process_cascade if needed
  # For now, pass raw_data and let process_cascade handle filtering
  cascade_result <- process_cascade(raw_data)
  cascade <- cascade_result$cascade

  return(list(
    indicators = indicators,
    dynamics = dynamics,
    cascade = cascade
  ))
}

#' Clean Alignment Data
#'
#' This function cleans the alignment data, which links project indicators to
#' community capitals.
#'
#' @param alignment_raw A data frame containing the raw alignment data.
#' @return A cleaned data frame.
clean_alignment_data <- function(alignment_raw) {
  alignment_raw %>%
    dplyr::rename(
      impact_indicator_code = `Impact Indicator Code`,
      community_capital_code = `Community Capital Code`
    ) %>%
    dplyr::select(impact_indicator_code, community_capital_code) %>%
    dplyr::mutate(
      community_capital = dplyr::case_when(
        community_capital_code == "N" ~ "Natural",
        community_capital_code == "C" ~ "Cultural",
        community_capital_code == "H" ~ "Human",
        community_capital_code == "S" ~ "Social",
        community_capital_code == "P" ~ "Political",
        community_capital_code == "F" ~ "Financial",
        community_capital_code == "B" ~ "Built",
        TRUE ~ "Unknown"
      )
    )
}


#' Generate Cascade YAML
#'
#' Generates YAML configuration for the cascade effects network visualization.
#'
#' @param cascade_data A data frame with 'from' and 'to' columns for the network edges.
#' @return A string containing the YAML configuration.
generate_cascade_yaml <- function(cascade_data) {
  # Default parameters
  params <- list(
    network_layout = "layout_with_fr",
    network_colors = list(
      researcher = "#1E325C",
      community = "#3B6B35",
      second_degree = "#A64B42",
      third_degree = "#E0D0A6"
    ),
    network_physics = list(
      enabled = TRUE,
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -50,
        centralGravity = 0.01,
        springLength = 100,
        springConstant = 0.08
      )
    )
  )
  # Convert to YAML string
  yaml::as.yaml(params)
}


# ============================================================================
# Workflow Utility Functions (Centralized)
# ============================================================================

# Note: All workflow functions are now centralized in utils_workflow.R
# to prevent conflicts with the main server observer.
# Local workflow functions have been removed to avoid competing observers.

# Debug workflow state (kept for compatibility)
debug_workflow_state <- function(ns_workflow, context = "Debug") {
  if (is.null(ns_workflow)) {
    logger::log_warn("ns_workflow is NULL in debug_workflow_state")
    return()
  }

  stage <- tryCatch(
    {
      if (is.null(isolate(ns_workflow$stage))) "NULL" else isolate(ns_workflow$stage)
    },
    error = function(e) {
      logger::log_error("Error accessing stage in debug_workflow_state: {conditionMessage(e)}")
      "ERROR"
    }
  )

  status <- tryCatch(
    {
      if (is.null(isolate(ns_workflow$status))) "NULL" else isolate(ns_workflow$status)
    },
    error = function(e) {
      logger::log_error("Error accessing status in debug_workflow_state: {conditionMessage(e)}")
      "ERROR"
    }
  )

  logger::log_info("DEBUG {context}: Workflow State - Stage: {stage}, Status: {status}")
}
