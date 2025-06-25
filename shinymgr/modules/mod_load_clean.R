#!! ModName = mod_load_clean

# Debug message to verify module is loaded
logger::log_info("mod_load_clean module is being loaded")

# Import necessary functions
#' @importFrom dplyr filter select mutate rename case_when everything left_join
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @importFrom stringr str_remove str_starts
#' @importFrom shiny showNotification HTML
#' @importFrom shinyalert shinyalert
#' @importFrom utils str
#' @importFrom logger log_info log_error log_warn log_debug
#' @importFrom stats setNames
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
  ns <- NS(id)

  # Initialize shinyjs
  shinyjs::useShinyjs()

  tagList(
    bslib::navset_card_tab(
      id = ns("load_clean_tabs"), # Add an ID to the navset for switching
      bslib::nav_panel(
        "Overview",
        icon = ph("lighthouse"),
        fluidRow(
          column(
            width = 8,
            h1("Upload and Clean",
              class = "load",
              style = "border-bottom: solid 1px #4A4A4A;"
            ),
            p("This module allows you to load and clean your project data efficiently. Follow the steps below to prepare your data for analysis.",
              style = "font-family: var(--font-sans) !important; padding: 10px; margin-bottom: 20px;"
            )
          ),
          column(
            width = 4,
            img(
              src = "load.png", alt = "Data Load and Clean Image",
              style = "max-width: 100%; height: auto; margin-top: 20px; padding-left: 10%; padding-right: 10%;"
            )
          )
        )
      ),
      bslib::nav_panel(
        "Upload & Clean",
        value = "load_clean_panel",
        icon = ph("upload"),
        fluidRow(
          column(
            width = 12,
            h1("Upload & Clean",
              class = "load",
              style = "border-bottom: solid 1px #4A4A4A;"
            )
          )
        ),
        fluidRow(
          column(
            width = 7,
            p("Upload the main project data set containing all research team generated data, including:"),
            tags$ul(tags$li("Project indicators and metrics"), tags$li("Cascade effects analysis"))
          ),
          column(
            width = 5,
            bslib::card(
              class = "rounded shadow p-0",
              style = "margin-right: 1em; margin-top: 1em;",
              bslib::card_body(
                style = "background-color: #f0e5d7; padding-top: 10px; border: solid #d2bfa3 1px;",
                fileInput(ns("main_data_file"), "Select Project Data File", accept = c(".csv", "text/csv"), buttonLabel = "Browse...", placeholder = "No file selected", width = "100%"),
                fileInput(ns("alignment_data_file"), "Select Alignment Data File", accept = c(".csv", "text/csv"), buttonLabel = "Browse...", placeholder = "No file selected", width = "100%"),
                div(
                  class = "d-flex justify-content-between align-items-center",
                  span(
                    class = "d-flex justify-content-center gap-3 align-items-center w-25",
                    uiOutput(ns("project_status_icon")),
                    ph("gauge", weight = "bold")
                  ),
                  span(
                    class = "d-flex justify-content-center gap-3 align-items-center w-25",
                    uiOutput(ns("alignment_status_icon")),
                    ph("flower-lotus", weight = "bold")
                  ),
                  span(
                    class = "d-flex justify-content-center gap-3 align-items-center w-25",
                    uiOutput(ns("pulse_status_icon")),
                    ph("pulse", weight = "bold")
                  ),
                  span(
                    class = "d-flex justify-content-center gap-3 align-items-center w-25",
                    uiOutput(ns("waveform_status_icon")),
                    ph("waveform", weight = "bold")
                  )
                ),
                bslib::card_footer(
                  style = "background-color: #d2bfa3; border: none;",
                  class = "rounded",
                  actionButton(ns("clean_data"),
                    label = div("Clean Data",
                      icon("wand-magic-sparkles"),
                      style = "color: white;"
                    ),
                    class = "btn-primary btn-lg",
                    style = "width: 100%;"
                  )
                )
              )
            )
            # img(
            #   src = "load-clean-painting.jpg",
            #   class = "content-justify-center round shadow",
            #   style = "max-width: 60%; width: auto; height: auto; max-height: 180px; margin-top: 2em; display: block;"
            # )
          )
        )
      ),
      bslib::nav_panel("Indicators", value = "indicators_panel", icon = ph("gauge"), uiOutput(ns("indicators_ui"))),
      bslib::nav_panel("Alignment", value = "alignment_panel", icon = ph("flower-lotus"), uiOutput(ns("alignment_ui"))),
      bslib::nav_panel("Dynamics", value = "dynamics_panel", icon = ph("pulse"), uiOutput(ns("dynamics_ui"))),
      bslib::nav_panel("Cascade Effects",
        value = "cascade_panel", icon = ph("waveform"),
        fluidRow(
          column(
            width = 6,
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-top: 20px;",
              h4("Network Parameters", style = "margin: 0;"),
              div(
                downloadButton(ns("download_cascade_yaml"),
                  label = "Download Parameters",
                  icon = icon("download"),
                  class = "btn-sm",
                  style = "background-color: #3B6B35; color: white; border: none;"
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
          column(
            width = 6,
            uiOutput(ns("network_plot_ui")),
            # Add legend
            div(
              style = "margin-top: 20px; text-align: center;",
              h5("Legend"),
              div(
                style = "display: flex; justify-content: center; flex-wrap: wrap; gap: 15px;",
                div(
                  style = "display: flex; align-items: center; margin: 5px;",
                  div(style = "width: 15px; height: 15px; background-color: #1E325C; margin-right: 5px; border-radius: 3px;"),
                  span("Researcher")
                ),
                div(
                  style = "display: flex; align-items: center; margin: 5px;",
                  div(style = "width: 15px; height: 15px; background-color: #3B6B35; margin-right: 5px; border-radius: 3px;"),
                  span("Community Member")
                ),
                div(
                  style = "display: flex; align-items: center; margin: 5px;",
                  div(style = "width: 15px; height: 15px; background-color: #A64B42; margin-right: 5px; border-radius: 3px;"),
                  span("2nd Degree")
                ),
                div(
                  style = "display: flex; align-items: center; margin: 5px;",
                  div(style = "width: 15px; height: 15px; background-color: #E0D0A6; margin-right: 5px; border-radius: 3px;"),
                  span("3rd Degree")
                )
              )
            )
          )
        )
      )
    )
  )
}

# =================================================================================================
# Server Function
# =================================================================================================
mod_load_clean_server <- function(id, ns_workflow) {
  # For backward compatibility, also accept rv_workflow
  if (missing(ns_workflow) && !missing(rv_workflow)) {
    ns_workflow <- rv_workflow
  }
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track status for all indicators
    project_status <- reactiveVal("empty")  # empty, circle, or check-circle
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
    })
    
    # Update alignment status when file is selected
    observeEvent(input$alignment_data_file, {
      if (!is.null(input$alignment_data_file)) {
        alignment_status("circle")
      } else {
        alignment_status("empty")
      }
    })
    
    # Render status icon with colors
    render_status_icon <- function(status) {
      color <- switch(
        status,
        "circle" = "#3F5E78",
        "check-circle" = "#4B7F52",
        "#6c757d"  # Default gray for empty state
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
      workflow = ns_workflow # Store the workflow object
    )

    # Observer to store main data file info when uploaded
    observeEvent(input$main_data_file, {
      req(input$main_data_file)
      rv$main_data_file <- input$main_data_file
      logger::log_info("Main data file stored for cleaning: {input$main_data_file$name}")
    })

    # Observer for when the Load & Clean panel is viewed
    observeEvent(input$load_clean_tabs,
      {
        if (input$load_clean_tabs == "load_clean_panel") {
          logger::log_info("Load & Clean panel viewed - updating workflow to in progress")
          update_workflow_step(
            ns_workflow,
            step = "Upload Data",
            stage = "in progress",
            session = session
          )
        }
      },
      ignoreInit = TRUE
    )

    # Observer to store alignment data file info when uploaded
    observeEvent(input$alignment_data_file, {
      req(input$alignment_data_file)
      rv$alignment_data_file <- input$alignment_data_file
      logger::log_info("Alignment data file stored for cleaning: {input$alignment_data_file$name}")

      # Update workflow to complete when alignment file is uploaded
      logger::log_info("Alignment file uploaded - marking Load Data as complete")
      update_workflow_step(
        ns_workflow,
        step = "Upload Data",
        stage = "complete",
        session = session
      )
    })

    # Test observer for button click
    observeEvent(input$clean_data,
      {
        cat("BUTTON CLICKED!\n")
        showNotification("Button was clicked!", type = "message")
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Main observer for the Clean Data button
    observeEvent(input$clean_data, {
      # Update workflow to Clean Data in progress
      logger::log_info("Starting data cleaning - updating workflow to in progress")
      update_workflow_step(
        ns_workflow,
        step = "Clean Data",
        stage = "in progress",
        session = session
      )

      # Use the stored file data from rv
      if (is.null(rv$main_data_file) || is.null(rv$alignment_data_file)) {
        showNotification("Please upload both data files before cleaning.", type = "error")
        return()
      }

      progress_notif <- showNotification("Cleaning data...", type = "message", duration = NULL)
      on.exit(removeNotification(progress_notif), add = TRUE)

      tryCatch(
        {
          # 1. Clean main data (which returns a list)
          logger::log_info("Reading main data file: {rv$main_data_file$name}")
          main_data_raw <- readr::read_csv(rv$main_data_file$datapath, show_col_types = FALSE)
          logger::log_info("Main data file read successfully. Rows: {nrow(main_data_raw)}, Cols: {ncol(main_data_raw)}")

          # Check if clean_data function exists
          if (!exists("clean_data")) {
            stop("clean_data function not found. Please ensure utils_clean_data.R is properly sourced.")
          }

          logger::log_info("Starting main data cleaning...")
          cleaned_main_list <- tryCatch(
            clean_data(main_data_raw),
            error = function(e) {
              logger::log_error("Error in clean_data: {conditionMessage(e)}")
              stop("Failed to clean main data: ", e$message)
            }
          )

          # Validate the structure of cleaned data
          required_components <- c("indicators", "dynamics", "cascade")
          missing_components <- setdiff(required_components, names(cleaned_main_list))
          if (length(missing_components) > 0) {
            stop(
              "Clean data is missing required components: ",
              paste(missing_components, collapse = ", ")
            )
          }

          # 2. Clean alignment data
          logger::log_info("Reading alignment data file: {rv$alignment_data_file$name}")
          alignment_data_raw <- readr::read_csv(rv$alignment_data_file$datapath, show_col_types = FALSE)
          logger::log_info("Alignment data file read successfully. Rows: {nrow(alignment_data_raw)}")

          logger::log_info("Starting alignment data cleaning...")
          rv$alignment <- tryCatch(
            clean_alignment_data(alignment_data_raw),
            error = function(e) {
              logger::log_error("Error in clean_alignment_data: {conditionMessage(e)}")
              stop("Failed to clean alignment data: ", e$message)
            }
          )

          # Store the cleaned alignment data in ns_workflow for other modules to use
          observe({
            ns_workflow$alignment_data <- rv$alignment
            logger::log_info("Stored alignment data in ns_workflow")
          })

          # Update reactive values after successful cleaning
          rv$indicators <- cleaned_main_list$indicators
          rv$dynamics <- cleaned_main_list$dynamics
          rv$cascade <- cleaned_main_list$cascade

          # Store data in ns_workflow for other modules
          observe({
            ns_workflow$dynamics_data <- cleaned_main_list$dynamics
            # Store just the edgelist for cascade analysis
            if (!is.null(cleaned_main_list$cascade$edgelist)) {
              ns_workflow$cascade_data <- cleaned_main_list$cascade$edgelist
              log_info("Stored cascade edgelist in ns_workflow$cascade_data")
              log_info(sprintf(
                "Cascade edgelist: %d rows, %d columns",
                nrow(cleaned_main_list$cascade$edgelist),
                ncol(cleaned_main_list$cascade$edgelist)
              ))
            } else {
              log_warn("No edgelist found in cascade data")
              ns_workflow$cascade_data <- NULL
            }
            log_info("Stored dynamics data in ns_workflow")
          })

          logger::log_info("Data cleaning completed successfully!")
          showNotification("Data cleaned successfully!", type = "message")
          # Update all status indicators to check-circle
          project_status("check-circle")
          alignment_status("check-circle")
          pulse_status("check-circle")
          waveform_status("check-circle")
          logger::log_info("Data cleaning complete - updating workflow")
          update_workflow_step(
            ns_workflow,
            step = "Clean Data",
            stage = "complete",
            session = session
          )

          # Log the structure of the cleaned data for debugging
          logger::log_info("Cleaned data structure:")
          logger::log_info("- Indicators: {nrow(rv$indicators)} rows")
          logger::log_info("- Dynamics: {nrow(rv$dynamics)} rows")
          logger::log_info("- Cascade: {length(rv$cascade)} components")
          logger::log_info("- Alignment: {nrow(rv$alignment)} rows")
        },
        error = function(e) {
          error_msg <- conditionMessage(e)
          logger::log_error("Error during data cleaning: {error_msg}")

          # More detailed error notification
          showNotification(
            HTML(paste(
              "<b>Error during data cleaning:</b>",
              "<br><br>",
              gsub("\n", "<br>", error_msg)
            )),
            type = "error",
            duration = 15,
            closeButton = TRUE
          )

          # Update workflow to show error
          update_workflow_step(
            rv_workflow,
            step = "Clean Data",
            stage = "error",
            session = session
          )
        }
      )
    })

    # Generic UI for "No Data" message
    no_data_ui <- function(data_name) {
      div(
        style = "text-align: center; padding: 50px;",
        h4(paste("No", data_name, "data to display.")),
        p("Please upload and clean your data in the 'Load & Clean' tab.")
      )
    }

    # Module status output
    output$module_status <- renderText({
      paste(
        "Module loaded: ", format(Sys.time(), "%H:%M:%OS"), "\n",
        "Session token: ", session$token, "\n",
        "Namespace: ", session$ns(""), "\n"
      )
    })

    # Test button observer
    observeEvent(input$test_button, {
      showNotification("Test button was clicked!", type = "message")
    })

    # Debug output
    output$debug_info <- renderText({
      tryCatch(
        {
          # Get button ID
          button_id <- ns("clean_data")

          # Get input names safely
          input_names <- tryCatch(names(input), error = function(e) "Error accessing input names")

          # Build debug info
          paste(
            "=== Basic Info ===\n",
            "Current time: ", format(Sys.time(), "%H:%M:%OS"), "\n\n",
            "=== Button Status ===\n",
            "Button ID: ", button_id, "\n",
            "Button in input: ", button_id %in% input_names, "\n\n",
            "=== Inputs ===\n",
            "Total inputs: ", length(input_names), "\n",
            "First 5 inputs: ", paste(head(input_names, 5), collapse = ", "), "\n\n",
            "=== Files ===\n",
            "Main data: ", if (!is.null(rv$main_data_file)) "Uploaded" else "Not uploaded", "\n",
            "Alignment data: ", if (!is.null(rv$alignment_data_file)) "Uploaded" else "Not uploaded"
          )
        },
        error = function(e) {
          paste("Error generating debug info:", conditionMessage(e))
        }
      )
    })

    # Render Indicators UI and Table
    output$indicators_ui <- renderUI({
      if (is.null(rv$indicators)) {
        no_data_ui("Indicators")
      } else {
        tagList(
          div(
            style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
            downloadButton(ns("download_indicators"),
              label = "Download CSV",
              icon = icon("file-csv"),
              class = "btn-sm",
              style = "background-color: #3B6B35; color: white; border: none;"
            )
          ),
          DT::dataTableOutput(ns("indicators_table"))
        )
      }
    })
    output$indicators_table <- DT::renderDataTable({
      req(rv$indicators)
      DT::datatable(rv$indicators, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    # Render Alignment UI and Table
    output$alignment_ui <- renderUI({
      if (is.null(rv$alignment)) {
        no_data_ui("Alignment")
      } else {
        tagList(
          div(
            style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
            downloadButton(ns("download_alignment"),
              label = "Download CSV",
              icon = icon("file-csv"),
              class = "btn-sm",
              style = "background-color: #3B6B35; color: white; border: none;"
            )
          ),
          DT::dataTableOutput(ns("alignment_table"))
        )
      }
    })
    output$alignment_table <- DT::renderDataTable({
      req(rv$alignment)
      DT::datatable(rv$alignment, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    # Render Dynamics UI and Table
    output$dynamics_ui <- renderUI({
      if (is.null(rv$dynamics)) {
        no_data_ui("Dynamics")
      } else {
        tagList(
          div(
            style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
            downloadButton(ns("download_dynamics"),
              label = "Download CSV",
              icon = icon("file-csv"),
              class = "btn-sm",
              style = "background-color: #3B6B35; color: white; border: none;"
            )
          ),
          DT::dataTableOutput(ns("dynamics_table"))
        )
      }
    })
    output$dynamics_table <- DT::renderDataTable({
      req(rv$dynamics)
      DT::datatable(rv$dynamics, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    # Download handlers for data tables
    output$download_indicators <- downloadHandler(
      filename = function() {
        paste0("indicators_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(rv$indicators)
        readr::write_csv(rv$indicators, file)
      },
      contentType = "text/csv"
    )

    output$download_alignment <- downloadHandler(
      filename = function() {
        paste0("alignment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(rv$alignment)
        readr::write_csv(rv$alignment, file)
      },
      contentType = "text/csv"
    )

    output$download_dynamics <- downloadHandler(
      filename = function() {
        paste0("dynamics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(rv$dynamics)
        readr::write_csv(rv$dynamics, file)
      },
      contentType = "text/csv"
    )

    # Update aceEditor with cascade config
    observe({
      req(rv$cascade)
      shinyAce::updateAceEditor(
        session,
        editorId = "cascade_yaml_editor",
        value = yaml::as.yaml(rv$cascade$model)
      )
    })

    # Handle download of cascade YAML
    output$download_cascade_yaml <- downloadHandler(
      filename = function() {
        paste0("cascade_parameters_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yaml")
      },
      content = function(file) {
        # Get the current content from the aceEditor
        content <- input$cascade_yaml_editor
        # Write the content to the file
        writeLines(content, file)
      },
      contentType = "text/yaml"
    )

    # Screenshot functionality using shinyscreenshot
    observeEvent(input$take_screenshot, {
      req(rv$cascade)

      tryCatch(
        {
          # Get the network container selector
          plot_selector <- paste0("#", ns("network_container"))

          # Take the screenshot
          shinyscreenshot::screenshot(
            selector = plot_selector,
            filename = paste0("network_plot_", format(Sys.time(), "%Y%m%d_%H%M%S")),
            scale = 2
          )
        },
        error = function(e) {
          showNotification(paste("Error taking screenshot:", e$message), type = "error")
        }
      )
    })

    # Download handler for edgelist CSV
    output$download_edgelist <- downloadHandler(
      filename = function() {
        paste0("cascade_edgelist_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(rv$cascade$edgelist)
        readr::write_csv(rv$cascade$edgelist, file)
      },
      contentType = "text/csv"
    )

    # Function to create network plot
    create_network_plot <- function(cascade_data) {
      # Extract nodes and edges from cascade data
      nodes <- cascade_data$nodes
      edges <- cascade_data$edges

      # Define role colors
      role_colors <- c(
        "Researcher" = "#1E325C",
        "Community Member" = "#3B6B35",
        "2nd Degree" = "#A64B42",
        "3rd Degree" = "#E0D0A6"
      )

      # Create the network
      visNetwork::visNetwork(nodes, edges) |>
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
        visNetwork::visLayout(randomSeed = 42) |>
        visNetwork::visLegend(
          useGroups = FALSE,
          addNodes = lapply(names(role_colors), function(role) {
            list(label = role, color = role_colors[role], shape = "dot")
          }),
          width = 0.2,
          position = "right",
          zoom = FALSE
        )
    }

    # Render network plot
    output$network_plot_ui <- renderUI({
      req(rv$cascade) # Ensure cascade data exists

      tryCatch(
        {
          # Debug output
          logger::log_info("Rendering network plot UI")

          # Create the network plot output container
          tagList(
            visNetwork::visNetworkOutput(ns("network_plot"), height = "600px")
          )
        },
        error = function(e) {
          logger::log_error("Error creating network plot UI: {conditionMessage(e)}")
          p("Error generating network visualization.")
        }
      )
    })

    # Render the actual network plot
    output$network_plot <- visNetwork::renderVisNetwork({
      req(rv$cascade)

      tryCatch(
        {
          logger::log_info("Rendering network plot")
          create_network_plot(rv$cascade)
        },
        error = function(e) {
          logger::log_error("Error rendering network plot: {conditionMessage(e)}")
          NULL
        }
      )
    })

    # Debug output for cascade data structure
    observe({
      req(rv$cascade)
      logger::log_info("Cascade data structure:")
      logger::log_info(paste("Nodes:", nrow(rv$cascade$nodes)))
      if (!is.null(rv$cascade$edgelist)) {
        logger::log_info(paste("Edges:", nrow(rv$cascade$edgelist)))
      }
    })

    # Network plot UI
    output$network_plot_ui <- renderUI({
      req(rv$cascade)

      if (is.null(rv$cascade$edgelist) || nrow(rv$cascade$edgelist) == 0 ||
        is.null(rv$cascade$nodes) || nrow(rv$cascade$nodes) == 0) {
        tagList(
          h4("Social Network"),
          p("No network data available to display."),
          p("Available data in cascade:", paste(names(rv$cascade), collapse = ", "))
        )
      } else {
        tagList(
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            h4("Model Social Network", style = "margin: 0;"),
            div(
              style = "display: flex; gap: 8px;",
              actionButton(ns("take_screenshot"),
                label = "Save as PNG",
                icon = icon("camera"),
                class = "btn-sm",
                style = "background-color: #3B6B35; color: white; border: none;"
              ),
              downloadButton(ns("download_edgelist"),
                label = "Download CSV",
                icon = icon("file-csv"),
                class = "btn-sm",
                style = "background-color: #3B6B35; color: white; border: none;"
              )
            )
          ),
          div(
            style = "position: relative; margin-bottom: 15px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background: white;",
            id = ns("network_container"),
            `data-screenshot` = "true", # Add data attribute for easier selection
            div(
              style = "width: 100%; height: 100%;",
              id = ns("network_wrapper"),
              shinycssloaders::withSpinner(
                visNetwork::visNetworkOutput(ns("network_plot"), height = "500px"),
                type = 8,
                color = "#3F5E78"
              )
            )
          )
        )
      }
    })

    # Render the actual network plot with export button
    output$network_plot <- visNetwork::renderVisNetwork({
      req(rv$cascade)

      tryCatch(
        {
          edges <- rv$cascade$edgelist
          nodes <- rv$cascade$nodes

          # Create the network
          net <- visNetwork::visNetwork(nodes, edges) |>
            visNetwork::visOptions(
              highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
              nodesIdSelection = TRUE,
              manipulation = FALSE
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
            visNetwork::visLayout(randomSeed = 42) |>
            visNetwork::visInteraction(
              navigationButtons = TRUE,
              keyboard = TRUE,
              tooltipDelay = 200
            )

          net

          # Debug output
          logger::log_info("Rendering network with {nrow(nodes)} nodes and {nrow(edges)} edges")
          logger::log_info("Node roles: {paste(unique(nodes$role), collapse=', ')}")
          logger::log_info("First 10 nodes: {paste(nodes$id[1:min(10, nrow(nodes))], '(', nodes$role[1:min(10, nrow(nodes))], ')', collapse=', ')}")

          # Ensure required columns exist
          if (!all(c("from", "to") %in% names(edges))) {
            stop("Edges must contain 'from' and 'to' columns")
          }
          if (!all(c("id", "role") %in% names(nodes))) {
            stop("Nodes must contain 'id' and 'role' columns")
          }

          # Define role colors
          role_colors <- c(
            "Researcher" = "#1E325C",
            "Community Member" = "#3B6B35",
            "2nd Degree" = "#A64B42",
            "3rd Degree" = "#E0D0A6"
          )

          # Ensure all roles have colors
          nodes$color <- "#CCCCCC" # Default color
          for (role in names(role_colors)) {
            nodes$color[nodes$role == role] <- role_colors[role]
          }

          # Create labels and tooltips
          nodes$label <- ifelse(nodes$role %in% c("Researcher", "Community Member"),
            paste0(nodes$role, " ", nodes$id),
            nodes$role
          )
          nodes$title <- paste0("ID: ", nodes$id, "<br>Role: ", nodes$role)

          # Ensure node IDs are characters
          nodes$id <- as.character(nodes$id)
          edges$from <- as.character(edges$from)
          edges$to <- as.character(edges$to)

          # Create the network
          visNetwork::visNetwork(nodes, edges) |>
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
            visNetwork::visLayout(randomSeed = 42) |>
            visNetwork::visLegend(
              useGroups = TRUE,
              position = "right",
              main = "Role",
              zoom = FALSE
            )
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
