#!! ModName = mod_load_clean

# Debug message to verify module is loaded
logger::log_info("mod_load_clean module is being loaded")

# Import necessary functions
#' @importFrom dplyr filter select mutate rename case_when everything left_join group_by summarise ungroup arrange
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @importFrom stringr str_remove str_starts
#' @importFrom shiny showNotification HTML tagList tags NS icon
#' @importFrom shinyjs useShinyjs delay html runjs
#' @importFrom shinyalert shinyalert
#' @importFrom utils str
#' @importFrom logger log_info log_error log_warn log_debug
#' @importFrom stats setNames
#' @importFrom phosphoricons ph
#' @importFrom bslib card_header card_body card accordion accordion_panel
#' @importFrom yaml yaml.load as.yaml
#' @importFrom visNetwork visNetwork visOptions visPhysics visLayout visLegend
#' @importFrom DT datatable
#' @importFrom igraph graph_from_data_frame V E layout_with_fr
#' @importFrom shinyAce aceEditor updateAceEditor
# Note: utils functions are sourced in global.R, so they are available globally
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
    shinyjs::useShinyjs(), # Initialize shinyjs
    tags$head(
      tags$style(HTML(paste0(
        "#", ns("progress_clean_data_container"), " {
          visibility: hidden;
        }"
      )))
    ),
    bslib::accordion(
      id = ns("load_clean_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "overview",
        title = tagList(ph("lighthouse"), HTML("&nbsp;"), "Overview"),
        fluidRow(
          column(
            width = 8,
            p(
              "This module allows you to load and clean your project data
              efficiently. Follow the steps below to prepare your data for
              analysis."
            )
          ),
          shiny::column(
            width = 4,
            shiny::img(
              src = "load.png", alt = "Data Load and Clean Image",
              style = "max-width: 100%; height: auto; margin-top: 20px;
              padding-left: 10%; padding-right: 10%;"
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "upload_clean",
        title = tagList(ph("sparkle"), HTML("&nbsp;"), "Upload & Clean"),
        
        # Note: Status alerts are now shown inline via status_alert_ui
        
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
        ),
        fluidRow(
          class = "d-flex justify-content-center",
          div(
            id = ns("progress_clean_data_container"),
            class = "progress-container", # New custom class
            style = "width: 50%; visibility: hidden;", # Start hidden
            
            # Add a label above the progress bar
            tags$p("Cleaning Data...", class = "progress-label", id = ns("progress_label")),
            
            # Use a standard shiny::div for the progress bar
            div(class = "progress",
                div(class = "progress-bar progress-bar-striped progress-bar-animated", 
                    role = "progressbar", 
                    style = "width: 0%;", 
                    `aria-valuenow` = "0", 
                    `aria-valuemin` = "0", 
                    `aria-valuemax` = "100",
                    id = ns("progress_bar_inner") # ID for the inner bar
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
          shiny::actionButton(
            ns("clean_data"),
            label = tagList(
              "Clean Data",
              ph("spray-bottle", weight = "bold")
            ),
            class = "btn btn-primary",
            style = "width: 40%;"
          )
        )
      ),
      bslib::accordion_panel(
        value = "indicators",
        title = tagList(ph("gauge"), HTML("&nbsp;"), "Indicators"),
        shiny::uiOutput(ns("indicators_ui"))
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

# =================================================================================================
# Server Function
# =================================================================================================
mod_load_clean_server <- function(id, ns_workflow) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for data
    rv <- reactiveValues(
      main_data_file = NULL,
      alignment_data_file = NULL,
      indicators = NULL,
      alignment = NULL,
      dynamics = NULL,
      cascade = NULL,
      workflow = ns_workflow
    )

    # REFACTORED: Centralized reactive values for the dynamic status alert
    rv_status <- reactiveValues(
      type = "info", # Can be 'info', 'success', 'warning', 'danger'
      message = "Ready to start. Please upload one or both data files."
    )

    # Helper function to determine the persistent (non-temporary) status
    get_persistent_status <- function() {
      main_up <- !is.null(rv$main_data_file)
      align_up <- !is.null(rv$alignment_data_file)
      
      # Check if we have cleaned data
      has_cleaned_data <- !is.null(rv$indicators) || !is.null(rv$alignment) || !is.null(rv$dynamics) || !is.null(rv$cascade)

      if (has_cleaned_data) {
        rv_status$type <- "info"
        rv_status$message <- "Data cleaning complete! Explore your cleaned data in the panels below."
      } else if (main_up && align_up) {
        rv_status$type <- "info"
        rv_status$message <- "Both data files are uploaded. Click 'Clean Data' to process them."
      } else if (main_up || align_up) {
        file_name <- if (main_up) rv$main_data_file$name else rv$alignment_data_file$name
        rv_status$type <- "info"
        rv_status$message <- paste(file_name, "is uploaded. You can clean it now or upload the other file.")
      } else {
        rv_status$type <- "info"
        rv_status$message <- "Ready to start. Please upload one or both data files."
      }
    }

    # Observer for main data file upload
    observeEvent(input$main_data_file, {
      req(input$main_data_file)
      rv$main_data_file <- input$main_data_file
      rv_status$type <- "success"
      rv_status$message <- paste("Main data file uploaded:", input$main_data_file$name)
      shinyjs::delay(2500, get_persistent_status())
    })

    # Observer for alignment data file upload
    observeEvent(input$alignment_data_file, {
      req(input$alignment_data_file)
      rv$alignment_data_file <- input$alignment_data_file
      rv_status$type <- "success"
      rv_status$message <- paste("Alignment data file uploaded:", input$alignment_data_file$name)
      shinyjs::delay(2500, get_persistent_status())
    })

    # Observer for the clean data button
    observeEvent(input$clean_data, {
      if (is.null(rv$main_data_file) && is.null(rv$alignment_data_file)) {
        rv_status$type <- "warning"
        rv_status$message <- "Please upload at least one data file before cleaning."
        shinyjs::delay(2500, get_persistent_status())
        return()
      }
      
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
      
      # 1. Update status message
      rv_status$type <- "info"
      rv_status$message <- "Data cleaning in progress..."
      
      # 2. Delay before showing progress bar
      shinyjs::delay(100, {
        # Show the progress container
        shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_clean_data_container")))
      })
      
      tryCatch({
        # Clean main data if uploaded
        if (!is.null(rv$main_data_file)) {
          update_progress(30, "Reading main data file...")
          main_data_raw <- readr::read_csv(rv$main_data_file$datapath)
          logger::log_info("Main data file read successfully: {rv$main_data_file$name}")
          logger::log_info("Main data raw dimensions: {nrow(main_data_raw)} rows, {ncol(main_data_raw)} columns")
          
          update_progress(50, "Cleaning main data...")
          logger::log_info("Starting clean_data function...")
          main_cleaned <- clean_data(main_data_raw)
          logger::log_info("Main data cleaned successfully")
          logger::log_info("Cleaned data structure: {paste(names(main_cleaned), collapse = ', ')}")
          
          # Store cleaned data in reactive values
          rv$indicators <- main_cleaned$indicators
          rv$dynamics <- main_cleaned$dynamics
          rv$cascade <- main_cleaned$cascade
          
          # Log what was stored in local reactive values
          logger::log_info("Local reactive values updated:")
          logger::log_info("  - indicators: {if(!is.null(rv$indicators)) nrow(rv$indicators) else 'NULL'} rows")
          logger::log_info("  - dynamics: {if(!is.null(rv$dynamics)) nrow(rv$dynamics) else 'NULL'} rows")
          logger::log_info("  - cascade: {if(!is.null(rv$cascade)) if(is.list(rv$cascade)) paste(names(rv$cascade), collapse=', ') else class(rv$cascade)}")
          
          # Update workflow with all cleaned data for other modules to access
          if (!is.null(main_cleaned$indicators)) {
            ns_workflow$indicators_data <- main_cleaned$indicators
            logger::log_info("Indicators data updated in workflow: {nrow(main_cleaned$indicators)} rows, {ncol(main_cleaned$indicators)} columns")
          }
          if (!is.null(main_cleaned$dynamics)) {
            ns_workflow$dynamics_data <- main_cleaned$dynamics
            logger::log_info("Dynamics data updated in workflow: {nrow(main_cleaned$dynamics)} rows, {ncol(main_cleaned$dynamics)} columns")
          }
          if (!is.null(main_cleaned$cascade)) {
            ns_workflow$cascade_data <- main_cleaned$cascade
            logger::log_info("Cascade data updated in workflow: {if(is.list(main_cleaned$cascade)) paste(names(main_cleaned$cascade), collapse=', ') else class(main_cleaned$cascade)}")
          }
        }
        
        # Clean alignment data if uploaded
        if (!is.null(rv$alignment_data_file)) {
          update_progress(70, "Reading alignment data file...")
          alignment_data_raw <- readr::read_csv(rv$alignment_data_file$datapath)
          logger::log_info("Alignment data file read successfully: {rv$alignment_data_file$name}")
          logger::log_info("Alignment data raw dimensions: {nrow(alignment_data_raw)} rows, {ncol(alignment_data_raw)} columns")
          
          update_progress(85, "Cleaning alignment data...")
          logger::log_info("Starting clean_alignment_data function...")
          rv$alignment <- clean_alignment_data(alignment_data_raw)
          logger::log_info("Alignment data cleaned successfully")
          logger::log_info("Cleaned alignment data dimensions: {nrow(rv$alignment)} rows, {ncol(rv$alignment)} columns")
          
          # Update workflow with alignment data for other modules to access
          ns_workflow$alignment_data <- rv$alignment
          logger::log_info("Alignment data updated in workflow: {nrow(rv$alignment)} rows, {ncol(rv$alignment)} columns")
        }
        
        update_progress(100, "Finalizing...")
        
        # Log what data was shared with the workflow
        logger::log_info("=== DATA SHARING SUMMARY ===")
        if (!is.null(ns_workflow$alignment_data)) {
          logger::log_info("Alignment data shared: {nrow(ns_workflow$alignment_data)} rows, {ncol(ns_workflow$alignment_data)} columns")
        } else {
          logger::log_info("No alignment data shared")
        }
        if (!is.null(ns_workflow$dynamics_data)) {
          logger::log_info("Dynamics data shared: {nrow(ns_workflow$dynamics_data)} rows, {ncol(ns_workflow$dynamics_data)} columns")
        } else {
          logger::log_info("No dynamics data shared")
        }
        if (!is.null(ns_workflow$cascade_data)) {
          if (is.list(ns_workflow$cascade_data)) {
            logger::log_info("Cascade data shared: list with elements: {paste(names(ns_workflow$cascade_data), collapse = ', ')}")
          } else {
            logger::log_info("Cascade data shared: {class(ns_workflow$cascade_data)}")
          }
        } else {
          logger::log_info("No cascade data shared")
        }
        logger::log_info("=== END DATA SHARING SUMMARY ===")
        
        # On success, update status and hide progress bar
        rv_status$type <- "success"
        rv_status$message <- "Data cleaning complete! You can now explore the cleaned data in the tabs below."
        logger::log_info("Success status set: type={rv_status$type}, message={rv_status$message}")
        
        # Hide progress bar immediately
        shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_clean_data_container")))
        update_progress(0) # Reset for next time
        
        # Let the success message show for 3 seconds before auto-hiding
        # (The auto-hide observer will handle this)
        
      }, error = function(e) {
        logger::log_error("Error during data cleaning: {conditionMessage(e)}")
        
        # On error, update status and hide progress bar
        rv_status$type <- "danger"
        rv_status$message <- paste("An error occurred during cleaning:", conditionMessage(e))
        shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_clean_data_container")))
      })
    })
    
    # --- NEW RENDERUI LOGIC FOR INLINE ALERTS ---
    output$status_alert_ui <- renderUI({
      req(rv_status$type, rv_status$message)
      logger::log_info("Rendering status alert: type={rv_status$type}, message={rv_status$message}")
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
          # Check if we have cleaned data to show appropriate message
          if (!is.null(rv$indicators) || !is.null(rv$alignment) || !is.null(rv$dynamics) || !is.null(rv$cascade)) {
            rv_status$type <- "info"
            rv_status$message <- "Data cleaning complete! Explore your cleaned data in the panels below."
          } else {
            rv_status$type <- "info"
            rv_status$message <- "Ready to start. Please upload one or both data files."
          }
        })
      }
      
      # Special handling for success messages - show for 3 seconds then show data viewing message
      if (rv_status$type == "success") {
        shinyjs::delay(3000, {
          rv_status$type <- "info"
          rv_status$message <- "Data cleaning complete! Explore your cleaned data in the panels below."
        })
      }
    })

    # Note: The old dynamic_status_ui has been replaced with the new inline status_alert_ui

    # --- UI Rendering for Data Tables ---
    output$indicators_ui <- renderUI({
      if (is.null(rv$indicators)) {
        return(
          tags$div(
            class = "data-placeholder",
            tags$div(
              class = "d-flex align-items-center justify-content-center gap-2",
              ph("warning-circle", weight = "bold", class = "warning-icon"),
              tags$div(
                tags$strong("No Indicators Data"),
                tags$br(),
                "Upload and clean your data to view indicators here."
              )
            )
          )
        )
      }
      DT::datatable(rv$indicators, options = list(scrollX = TRUE, pageLength = 10), class = "cell-border stripe")
    })

    output$alignment_ui <- renderUI({
      if (is.null(rv$alignment)) {
        return(
          tags$div(
            class = "data-placeholder",
            tags$div(
              class = "d-flex align-items-center justify-content-center gap-2",
              ph("warning-circle", weight = "bold", class = "warning-icon"),
              tags$div(
                tags$strong("No Alignment Data"),
                tags$br(),
                "Upload and clean your data to view alignment here."
              )
            )
          )
        )
      }
      DT::datatable(rv$alignment, options = list(scrollX = TRUE, pageLength = 8), class = "cell-border stripe")
    })

    output$cascade_ui <- renderUI({
      if (is.null(ns_workflow$cascade_data)) {
        return(
          tags$div(
            class = "data-placeholder",
            tags$div(
              class = "d-flex align-items-center justify-content-center gap-2",
              ph("warning-circle", weight = "bold", class = "warning-icon"),
              tags$div(
                tags$strong("No Cascade Effects Data"),
                tags$br(),
                "Upload and clean your data to view cascade effects here."
              )
            )
          )
        )
      }
      shiny::fluidRow(
        shiny::column(
          width = 6,
          create_flat_info_card(
            ns,
            title = "Network Parameters",
            icon = "sliders-horizontal",
            content = tagList(
              div(
                shiny::downloadButton(ns("download_cascade_yaml"),
                  label = "Download Parameters",
                  icon = shiny::icon("download"),
                  class = "btn-sm",
                  style = "background-color: #3B6B35; color: white;
                          border: none;"
                )
              ),
              div(
                shinyAce::aceEditor(
                  outputId = ns("cascade_yaml_editor"),
                  mode = "yaml",
                  theme = "github",
                  height = "320px",
                  fontSize = 14,
                  debounce = 750,
                  autoScrollEditorIntoView = TRUE,
                  highlightActiveLine = TRUE
                )
              )
            )
          )
        ),
        shiny::column(
          width = 6,
          create_flat_info_card(
            ns,
            title = "Network Plot",
            icon = "graph",
            content = tagList(
              plotOutput(ns("static_network_plot"), height = "300px"),
              # Add legend
              shiny::div(
                style = "text-align: center;",
                shiny::h5("Legend"),
                shiny::div(
                  style = "display: flex; justify-content: center; flex-wrap: wrap; gap: 15px;",
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
      )
    })

    output$dynamics_ui <- renderUI({
      if (is.null(rv$dynamics)) {
        return(
          tags$div(
            class = "data-placeholder",
            tags$div(
              class = "d-flex align-items-center justify-content-center gap-2",
              ph("warning-circle", weight = "bold", class = "warning-icon"),
              tags$div(
                tags$strong("No Project Dynamics Data"),
                tags$br(),
                "Upload and clean your data to view project dynamics here."
              )
            )
          )
        )
      }
      DT::datatable(rv$dynamics, options = list(scrollX = TRUE, pageLength = 8), class = "cell-border stripe")
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
          # Removed shiny::showNotification("Failed to generate cascade parameters.", type = "error")
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




    # UI for network plot (dynamic - kept for future toggle option)
    output$network_plot_ui <- renderUI({
      req(ns_workflow$cascade_data)
      visNetwork::visNetworkOutput(ns("network_plot"), height = "300px")
    })

    # Render the dynamic network plot (kept for future toggle option)
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
              nodesIdSelection = FALSE,
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
          # Removed showNotification(paste("Error creating network:", e$message), type = "error", duration = NULL)
          return(NULL)
        }
      )
    })

    # Render the static network plot
    output$static_network_plot <- renderPlot({
      req(ns_workflow$cascade_data)
      tryCatch(
        {
          create_static_network_plot(ns_workflow$cascade_data)
        },
        error = function(e) {
          logger::log_error("Error rendering static network plot: {conditionMessage(e)}")
          plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Error creating network plot", col = "red")
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


#' Create Static Network Plot
#'
#' Creates a static network visualization using igraph that matches the dynamic
#' visNetwork plot in styling and layout.
#'
#' @param cascade_data A list containing 'nodes' and 'edges' data frames from cascade analysis.
#' @return A ggplot object representing the network visualization.
create_static_network_plot <- function(cascade_data) {
  tryCatch(
    {
      # Extract nodes and edges
      nodes <- cascade_data$nodes
      edges <- cascade_data$edges

      # Check if data is available
      if (is.null(nodes) || is.null(edges) || nrow(nodes) == 0 || nrow(edges) == 0) {
        logger::log_warn("No valid network data available for static plot")
        return(NULL)
      }

      # Create igraph object
      g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

      # Define color scheme to match visNetwork
      color_map <- c(
        "Researcher" = "#1E325C",
        "Community Member" = "#3B6B35",
        "2nd Degree" = "#A64B42",
        "3rd Degree" = "#E0D0A6"
      )

      # Get node colors based on roles
      node_colors <- color_map[nodes$role]

      # Calculate layout (similar to visNetwork forceAtlas2Based)
      layout_coords <- igraph::layout_with_fr(g, niter = 1000)

      # Create the plot
      par(mar = c(0, 0, 0, 0)) # Remove margins
      plot(g,
        layout = layout_coords,
        vertex.color = node_colors,
        vertex.size = 3,
        vertex.label = NA, # No labels for cleaner look
        vertex.frame.color = "#4a4a4a",
        vertex.frame.width = 0.5,
        edge.color = "#888888",
        edge.width = 0.5,
        edge.curved = 0.2,
        asp = 0
      ) # Maintain aspect ratio
      logger::log_info("Static network plot created successfully with {nrow(nodes)} nodes and {nrow(edges)} edges")
    },
    error = function(e) {
      logger::log_error("Error creating static network plot: {conditionMessage(e)}")
      # Return a simple error plot
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error creating network plot", col = "red", cex = 1.2)
    }
  )
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
