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
#' @importFrom shinyWidgets progressBar updateProgressBar
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
        title = tagList(ph("pencil-ruler"), HTML("&nbsp;"), "Upload & Clean"),
        fluidRow(
            width = 12,
            p(
              "This module allows you to load and clean your project data
              efficiently. Follow the steps below to prepare your data for
              analysis."
            ),
            tags$fieldset(
              class = "custom-fieldset",
              tags$legend(
                "Select",
                class = "custom-legend"
              ),
              layout_columns(
                col_widths = c(6, 6),
              shiny::fileInput(
                ns("alignment_data_file"),
                "Alignment Data File",
                accept = c(".csv", "text/csv"),
                buttonLabel = tagList(
                  phosphoricons::ph("flower-lotus", weight = "light"),
                  " Choose File"
                ),
                placeholder = "No file selected",
                width = "100%"
            ),
              shiny::fileInput(
                ns("main_data_file"),
                "Main Data File",
                accept = c(".csv", "text/csv"),
                buttonLabel = tagList(
                  phosphoricons::ph("database", weight = "light"),
                  " Choose File"
                ),
                placeholder = "No file selected",
                width = "100%"
            )
              )
            ),
            tags$fieldset(
              class = "custom-fieldset",
              tags$legend(
                "Clean",
                class = "custom-legend"
              ),
              fluidRow(
                class = "d-flex justify-content-center align-items-center",
                actionButton(
                  inputId = ns("clean_data"),
                  label = tagList(
                    "Clean Data ",
                    ph("spray-bottle", weight = "bold")
                  ),
                  width = "50%",
                  class = "btn btn-primary btn-lg"
                )
              ),
              fluidRow(
                class = "d-flex justify-content-center",
                div(
                  style = "margin-bottom: 1em; width: 50%; visibility: hidden;",
                  id = ns("container_bar_clean"),
                  shinyWidgets::progressBar(
                    id = ns("progress_bar_clean"),
                    value = 0,
                    total = 100,
                    status = "success",
                    title = "Ready to Clean",
                    striped = FALSE,
                    display_pct = FALSE
                  )
                )
              )
          )
          # shiny::column(
          #   width = 1,
          #   shiny::img(
          #     src = "load.png", alt = "Data Load and Clean Image",
          #     style = "width: auto; max-width: 100%; max-height: 7em; margin-top: 0.5em; padding-left: auto; padding-right: 0.5em;"
          #   )
          # )
        )
      ),
      bslib::accordion_panel(
        value = "results",
        title = tagList(ph("flag-checkered", weight = "fill"), HTML("&nbsp;"), "Results"),
        uiOutput(ns("results_ui"))
      )
    )
  )
}

# =================================================================================================
# Server Function
# =================================================================================================
mod_load_clean_server <- function(id, ns_project) {
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
      workflow = ns_project
    ) # closes reactiveValues

    # Observer for main data file upload
    observeEvent(input$main_data_file, {
      req(input$main_data_file)
      rv$main_data_file <- input$main_data_file
      shinyjs::delay(2500, {
        shinyWidgets::updateProgressBar(
          session = session,
          id = ns("progress_bar_clean"),
          value = 30,
          status = "success",
          title = "Reading main data file..."
        )
      })
    }) # closes observeEvent (main_data_file)

    # Observer for alignment data file upload
    observeEvent(input$alignment_data_file, {
      req(input$alignment_data_file)
      rv$alignment_data_file <- input$alignment_data_file
      shinyjs::delay(2500, {
        shinyWidgets::updateProgressBar(
          session = session,
          id = ns("progress_bar_clean"),
          value = 50,
          status = "success",
          title = "Reading alignment data file..."
        )
      })
    }) # closes observeEvent (alignment_data_file)

    # Observer for the clean data button
    observeEvent(input$clean_data, {
      # Show the progress bar container
      shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("container_bar_clean")))
      if (is.null(rv$main_data_file) && is.null(rv$alignment_data_file)) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = ns("progress_bar_clean"),
          value = 0,
          status = "warning",
          title = "Ready to Clean"
        )
        # Hide the progress bar container again after 1 second
        shinyjs::delay(1000, shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("container_bar_clean"))))
        return()
      }

      # --- PROGRESS BAR LOGIC ---

      # 1. Define a helper function to update the bar
      update_progress <- function(value, text = NULL) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = ns("progress_bar_clean"),
          value = value,
          status = "success",
          title = if (!is.null(text)) text else NULL
        )
      } # closes update_progress

      # 1. Update status message
      update_progress(70, "Data cleaning in progress...")

      # 2. Show the progress bar (always visible, so no need to show/hide)

      tryCatch(
        {
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
            rv$qualtrics_metadata_removed <- isTRUE(main_cleaned$qualtrics_metadata_removed)

            # Log what was stored in local reactive values
            logger::log_info("Local reactive values updated:")
            logger::log_info("  - indicators: {if(!is.null(rv$indicators)) nrow(rv$indicators) else 'NULL'} rows")
            logger::log_info("  - dynamics: {if(!is.null(rv$dynamics)) nrow(rv$dynamics) else 'NULL'} rows")
            logger::log_info("  - cascade: {if(!is.null(rv$cascade)) if(is.list(rv$cascade)) paste(names(rv$cascade), collapse=', ') else class(rv$cascade)}")

            # Update workflow with all cleaned data for other modules to access
            if (!is.null(main_cleaned$indicators)) {
              ns_project$indicators_data <- main_cleaned$indicators
              logger::log_info("Indicators data updated in workflow: {nrow(main_cleaned$indicators)} rows, {ncol(main_cleaned$indicators)} columns")
            }
            if (!is.null(main_cleaned$dynamics)) {
              ns_project$dynamics_data <- main_cleaned$dynamics
              logger::log_info("Dynamics data updated in workflow: {nrow(main_cleaned$dynamics)} rows, {ncol(main_cleaned$dynamics)} columns")
            }
            if (!is.null(main_cleaned$cascade)) {
              ns_project$cascade_data <- main_cleaned$cascade
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
            alignment_cleaned <- clean_alignment_data(alignment_data_raw)
            # alignment_cleaned is now a list: $data, $qualtrics_metadata_removed
            rv$alignment <- alignment_cleaned$data
            rv$alignment_qualtrics_metadata_removed <- isTRUE(alignment_cleaned$qualtrics_metadata_removed)
            logger::log_info("Alignment data cleaned successfully")
            logger::log_info("Cleaned alignment data dimensions: {nrow(rv$alignment)} rows, {ncol(rv$alignment)} columns")

            # Update workflow with alignment data for other modules to access
            ns_project$alignment_data <- rv$alignment
            logger::log_info("Alignment data updated in workflow: {nrow(rv$alignment)} rows, {ncol(rv$alignment)} columns")
          }

          update_progress(100, "Finalizing...")

          # Log what data was shared with the workflow
          logger::log_info("=== DATA SHARING SUMMARY ===")
          if (!is.null(ns_project$alignment_data)) {
            logger::log_info("Alignment data shared: {nrow(ns_project$alignment_data)} rows, {ncol(ns_project$alignment_data)} columns")
          } else {
            logger::log_info("No alignment data shared")
          }
          if (!is.null(ns_project$dynamics_data)) {
            logger::log_info("Dynamics data shared: {nrow(ns_project$dynamics_data)} rows, {ncol(ns_project$dynamics_data)} columns")
          } else {
            logger::log_info("No dynamics data shared")
          }
          if (!is.null(ns_project$cascade_data)) {
            if (is.list(ns_project$cascade_data)) {
              logger::log_info("Cascade data shared: list with elements: {paste(names(ns_project$cascade_data), collapse = ', ')}")
            } else {
              logger::log_info("Cascade data shared: {class(ns_project$cascade_data)}")
            }
          } else {
            logger::log_info("No cascade data shared")
          }
          logger::log_info("=== END DATA SHARING SUMMARY ===")

          # On success, update status and reset progress bar after 1 second
          update_progress(0, "Ready to Clean")
          # Hide the progress bar container again after 1 second
          shinyjs::delay(1000, shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("container_bar_clean"))))

          # Switch to the results accordion panel
          tryCatch(
            {
              bslib::accordion_panel_set(
                id = "load_clean_accordion", 
                value = "results",
                session = session)
            },
            error = function(e) {
              logger::log_error(sprintf("Error switching to results accordion panel: %s", conditionMessage(e)))
            }
          )
        },
        error = function(e) {
          logger::log_error("Error during data cleaning: {conditionMessage(e)}")

          # On error, update status and reset progress bar after 1 second
          update_progress(0, "Ready to Clean")
          # Hide the progress bar container again after 1 second
          shinyjs::delay(1000, shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("container_bar_clean"))))
        }
      ) # closes tryCatch
    }) # closes observeEvent (clean_data)

    # --- NEW RENDERUI LOGIC FOR INLINE ALERTS ---
    # (REMOVED)

    # Auto-hide logic for success/info/warning messages
    # (REMOVED)

    output$cascade_ui <- renderUI({
      if (is.null(ns_project$cascade_data)) {
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
          ) # closes return (no cascade data)
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
                  style = "background-color: #3B6B35; color: white;\n                          border: none;"
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
          ) # closes create_flat_info_card (parameters)
        ), # closes shiny::column (parameters)
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
                      style = "width: 15px; height: 15px; background-color:\n                    #1E325C; margin-right: 5px; border-radius: 3px;"
                    ),
                    shiny::span("Researcher")
                  ),
                  shiny::div(
                    style = "display: flex; align-items: center; margin: 5px;",
                    shiny::div(
                      style = "width: 15px; height: 15px; background-color:\n                    #3B6B35; margin-right: 5px; border-radius: 3px;"
                    ),
                    shiny::span("Community Member")
                  ),
                  shiny::div(
                    style = "display: flex; align-items: center; margin: 5px;",
                    shiny::div(
                      style = "width: 15px; height: 15px; background-color:\n                    #A64B42; margin-right: 5px; border-radius: 3px;"
                    ),
                    shiny::span("2nd Degree")
                  ),
                  shiny::div(
                    style = "display: flex; align-items: center; margin: 5px;",
                    shiny::div(
                      style = "width: 15px; height: 15px; background-color:\n                    #E0D0A6; margin-right: 5px; border-radius: 3px;"
                    ),
                    shiny::span("3rd Degree")
                  )
                )
              )
            )
          ) # closes create_flat_info_card (network plot)
        ) # closes shiny::column (network plot)
      ) # closes shiny::fluidRow
    }) # closes renderUI (cascade_ui)

    # --- Cascade Effects YAML and Network Plot ---
    # Load default YAML content
    observe({
      req(ns_project$cascade_data)
      tryCatch(
        {
          # Use the YAML model from cascade analysis if available
          if (!is.null(ns_project$cascade_data$model)) {
            yaml_content <- yaml::as.yaml(ns_project$cascade_data$model)
            logger::log_info("Cascade YAML editor populated from cascade analysis model.")
          } else {
            yaml_content <- generate_cascade_yaml(ns_project$cascade_data)
            logger::log_info("Cascade YAML editor populated with default template.")
          }
          shinyAce::updateAceEditor(session, "cascade_yaml_editor", value = yaml_content)
        },
        error = function(e) {
          logger::log_error("Error generating or updating cascade YAML: {conditionMessage(e)}")
          # Removed shiny::showNotification("Failed to generate cascade parameters.", type = "error")
        }
      ) # closes tryCatch (cascade YAML)
    }) # closes observe (cascade YAML)

    # Download handler for YAML
    output$download_cascade_yaml <- downloadHandler(
      filename = function() {
        paste0("cascade-parameters-", Sys.Date(), ".yml")
      },
      content = function(file) {
        writeLines(input$cascade_yaml_editor, file)
      }
    ) # closes downloadHandler (download_cascade_yaml)

    # Render the static network plot
    output$static_network_plot <- renderPlot({
      req(ns_project$cascade_data)
      tryCatch(
        {
          create_static_network_plot(ns_project$cascade_data)
        },
        error = function(e) {
          logger::log_error("Error rendering static network plot: {conditionMessage(e)}")
          plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Error creating network plot", col = "red")
        }
      ) # closes tryCatch (static_network_plot)
    }) # closes renderPlot (static_network_plot)

    # --- Overlay Card UI and Server Logic for Results Panel ---
    # Helper for overlay card UI
    overlay_card_ui <- function(id, title, content_ui) {
      ns <- NS(id)
      uiOutput(ns("overlay_ui"))
    } # closes overlay_card_ui

    # Add overlay UI outputs to the UI (already in UI via uiOutput)

    # Overlay server logic (modular)
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
        }) # closes renderUI (overlay_ui)
      }) # closes moduleServer (overlay_card_server)
    } # closes overlay_card_server

    # Indicators overlay
    overlay_card_server("indicators_overlay",
      show = reactive(input[["view_indicators_overlay"]]),
      close = reactive(NULL),
      title = "Cleaned Indicators Data",
      content_ui = DT::dataTableOutput(ns("indicators_overlay_table"))
    ) # closes overlay_card_server (indicators_overlay)
    output$indicators_overlay_table <- DT::renderDataTable({
      req(rv$indicators)
      DT::datatable(rv$indicators, options = list(scrollX = TRUE, pageLength = 10, dom = "ftip"), rownames = FALSE)
    }) # closes renderDataTable (indicators_overlay_table)
    output$indicators_overlay_ui <- renderUI({
      overlay_card_ui("indicators_overlay", "Cleaned Indicators Data", DT::dataTableOutput(ns("indicators_overlay_table")))
    }) # closes renderUI (indicators_overlay_ui)

    # Alignment overlay
    overlay_card_server("alignment_overlay",
      show = reactive(input[["view_alignment_overlay"]]),
      close = reactive(NULL),
      title = "Cleaned Alignment Data",
      content_ui = DT::dataTableOutput(ns("alignment_overlay_table"))
    ) # closes overlay_card_server (alignment_overlay)
    output$alignment_overlay_table <- DT::renderDataTable({
      req(rv$alignment)
      DT::datatable(rv$alignment, options = list(scrollX = TRUE, pageLength = 10, dom = "ftip"), rownames = FALSE)
    }) # closes renderDataTable (alignment_overlay_table)
    output$alignment_overlay_ui <- renderUI({
      overlay_card_ui("alignment_overlay", "Cleaned Alignment Data", DT::dataTableOutput(ns("alignment_overlay_table")))
    }) # closes renderUI (alignment_overlay_ui)

    # Dynamics overlay
    overlay_card_server("dynamics_overlay",
      show = reactive(input[["view_dynamics_overlay"]]),
      close = reactive(NULL),
      title = "Cleaned Dynamics Data",
      content_ui = DT::dataTableOutput(ns("dynamics_overlay_table"))
    ) # closes overlay_card_server (dynamics_overlay)
    output$dynamics_overlay_table <- DT::renderDataTable({
      req(rv$dynamics)
      DT::datatable(rv$dynamics, options = list(scrollX = TRUE, pageLength = 10, dom = "ftip"), rownames = FALSE)
    }) # closes renderDataTable (dynamics_overlay_table)
    output$dynamics_overlay_ui <- renderUI({
      overlay_card_ui("dynamics_overlay", "Cleaned Dynamics Data", DT::dataTableOutput(ns("dynamics_overlay_table")))
    }) # closes renderUI (dynamics_overlay_ui)

    # Cascade overlay
    overlay_card_server("cascade_overlay",
      show = reactive(input[["view_cascade_overlay"]]),
      close = reactive(NULL),
      title = "Cleaned Cascade Effects Data",
      content_ui = DT::dataTableOutput(ns("cascade_overlay_table"))
    ) # closes overlay_card_server (cascade_overlay)
    output$cascade_overlay_table <- DT::renderDataTable({
      req(ns_project$cascade_data)
      req(ns_project$cascade_data$edges)
      DT::datatable(ns_project$cascade_data$edges, options = list(scrollX = TRUE, pageLength = 10, dom = "ftip"), rownames = FALSE)
    }) # closes renderDataTable (cascade_overlay_table)
    output$cascade_overlay_ui <- renderUI({
      overlay_card_ui("cascade_overlay", "Cleaned Cascade Effects Data", DT::dataTableOutput(ns("cascade_overlay_table")))
    }) # closes renderUI (cascade_overlay_ui)

    output$results_ui <- renderUI({
      ns <- session$ns
      
      # Use a single tagList to build the entire UI at once.
      # This is cleaner and avoids the errors from the previous structure.
      tagList(
        # -- INDICATORS SECTION --
        tags$fieldset(
          class = "custom-fieldset",
          style = "margin-bottom: 2rem;",
          tags$legend(
            class = "custom-legend",
            "INDICATORS"
          ),
          metric_section_ui(
            data = rv$indicators,
            value_box_title = "indicators",
            score = if (!is.null(rv$indicators)) nrow(rv$indicators) else 0,
            type = "indicators",
            bgcolor = "#7E8480",
            icon_choice = ph_i("gauge", weight = "regular", size = "4x", style = "color:rgba(255,255,255,0.8);"),
            input_id = "view_indicators_overlay",
            ns = ns,
            section_name = "INDICATORS",
            placeholder_title = "No Indicators Data",
            placeholder_text = "Upload and clean your data to view indicators here.",
            placeholder_icon = ph("warning-circle", weight = "bold", size = "2x", style = "color: #bf8f36;margin-bottom:0.25rem;"),
            qualtrics_metadata_removed = rv$qualtrics_metadata_removed
          )
        ),
        # -- ALIGNMENT SECTION --
        tags$fieldset(
          class = "custom-fieldset",
          style = "margin-bottom: 1em;",
          tags$legend(class = "custom-legend", "Alignment"),
          metric_section_ui(
            data = rv$alignment,
            value_box_title = "Alignment",
            score = if (!is.null(rv$alignment)) nrow(rv$alignment) else 0,
            type = "alignment",
            bgcolor = "#A08E6F",
            icon_choice = ph_i("flower-lotus", weight = "bold", size = "4x"),
            input_id = "view_alignment_overlay",
            extra_info = if (isTRUE(rv$alignment_qualtrics_metadata_removed)) "Qualtrics Metadata Removed" else NULL,
            ns = ns,
            section_name = "Alignment",
            placeholder_title = "No Alignment Data",
            placeholder_text = "Upload and clean your data to view alignment here.",
            placeholder_icon = ph("warning-circle", weight = "bold", class = "warning-icon"),
            qualtrics_metadata_removed = NULL
          )
        ),
        # -- DYNAMICS SECTION --
        tags$fieldset(
          class = "custom-fieldset",
          style = "margin-bottom: 1em;",
          tags$legend(class = "custom-legend", "Dynamics"),
          metric_section_ui(
            data = rv$dynamics,
            value_box_title = "Dynamics",
            score = if (!is.null(rv$dynamics)) nrow(rv$dynamics) else 0,
            type = "dynamics",
            bgcolor = "#88707E",
            icon_choice = ph_i("pulse", weight = "bold", size = "4x"),
            input_id = "view_dynamics_overlay",
            ns = ns,
            section_name = "Dynamics",
            placeholder_title = "No Dynamics Data",
            placeholder_text = "Upload and clean your data to view dynamics here.",
            placeholder_icon = ph("warning-circle", weight = "bold", class = "warning-icon"),
            qualtrics_metadata_removed = rv$qualtrics_metadata_removed
          )
        ),
        # -- CASCADE SECTION --
        tags$fieldset(
          class = "custom-fieldset",
          style = "margin-bottom: 1em;",
          tags$legend(class = "custom-legend", "Cascade Effects"),
          metric_section_ui(
            data = if (!is.null(ns_project$cascade_data) && !is.null(ns_project$cascade_data$edges)) ns_project$cascade_data$edges else NULL,
            value_box_title = "Cascade Effects",
            score = if (!is.null(ns_project$cascade_data) && !is.null(ns_project$cascade_data$edges)) nrow(ns_project$cascade_data$edges) else 0,
            type = "cascade",
            bgcolor = "#B49291",
            icon_choice = ph_i("waveform", weight = "bold", size = "4x"),
            input_id = "view_cascade_overlay",
            ns = ns,
            section_name = "Cascade Effects",
            placeholder_title = "No Cascade Effects Data",
            placeholder_text = "Upload and clean your data to view cascade effects here.",
            placeholder_icon = ph("warning-circle", weight = "bold", class = "warning-icon"),
            qualtrics_metadata_removed = rv$qualtrics_metadata_removed
          )
        ),
        # -- OVERLAY UI OUTPUTS --
        uiOutput(ns("indicators_overlay_ui")),
        uiOutput(ns("alignment_overlay_ui")),
        uiOutput(ns("dynamics_overlay_ui")),
        uiOutput(ns("cascade_overlay_ui"))
      ) # closes the main tagList
    }) # closes renderUI
    # Return reactive values for use in other modules
    return(rv)
  }) # closes moduleServer
} # closes mod_load_clean_server

# =================================================================================================
# Helper Functions
# =================================================================================================

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
} # closes generate_cascade_yaml


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
  ) # closes tryCatch
} # closes create_static_network_plot

# Helper: create_score_value_box (local copy)

create_score_value_box <- function(title, score, type, bgcolor, icon_choice = ph_i("circle")) {
  score_display <- round(score, 0)

  bslib::value_box(
    style = "
      padding: 0.25em !important;
      border-radius: 10px 0 0 10px !important;
      border: 1px 0 1px 1px !important;
      border-style: solid !important;
      border-color: #d4c5b9 !important;
      box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06) !important;",
    title = tags$div(
      "",
      class = "text-uppercase fw-semibold",
      style = "font-size: 0rem;"
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
    theme = value_box_theme(bg = bgcolor, fg = "#f5f1e8"),
    tags$div(
      class = "text-uppercase fw-normal",
      style = "
        font-size: 0.9rem;
        margin-top: 0.25rem;
        color: #f5f1e8;",
      "records processed"
    )
  ) # closes bslib::value_box
} # closes create_score_value_box

#' Metric Section UI
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
metric_section_ui <- function(data, value_box_title, score, type, bgcolor, icon_choice, input_id, extra_info = NULL, ns, section_name, placeholder_title, placeholder_text, placeholder_icon, qualtrics_metadata_removed = NULL) {
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
        icon_choice = icon_choice
      )
    },
    # RIGHT SIDE: Details Card (if data)
    if (!is.null(data) && nrow(data) > 0) {
      data_completeness <- round(sum(!is.na(data)) / (nrow(data) * ncol(data)) * 100, 1)
      case_completeness <- round(sum(complete.cases(data)) / nrow(data) * 100, 1)
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
                "Ready for Analysis",
                class = "fw-bold",
                style = "font-size:1.1rem; text-transform: uppercase; color: var(--bs-success);font-family:var(--bs-font-monospace);"
              ),
              ph("check-circle", weight = "fill", size = "3x", style = "color: var(--bs-success);")
            )
          ),
          bslib::card_body(
            style = "padding:0.22rem .5rem; background: var(--bs-body-bg); overflow-x: hidden;",
            tagList(
              tags$div(
                class = "row",
                tags$div(
                  class = "col-6",
                  style = "font-size:0.8rem;line-height:1.14;color: var(--bs-primary);",
                  tags$div(
                    class = "d-flex align-items-start mb-1",
                    ph("check-circle",
                       weight = "regular", size = "1.5x",
                       style = "color: var(--bs-primary); margin-right:0.36rem; margin-top:0.05rem;"
                    ),
                    tags$div(
                      tags$strong("Data Completeness", style = "color: var(--bs-primary); text-transform: uppercase;"),
                      tags$br(),
                      tags$span(
                        style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace);",
                        paste0(data_completeness, "%")
                      )
                    )
                  )
                ),
                tags$div(
                  class = "col-6",
                  style = "font-size:0.8rem;line-height:1.14;color: var(--bs-primary);",
                  tags$div(
                    class = "d-flex align-items-start",
                    ph("check-circle",
                       weight = "regular", size = "1.5x",
                       style = "color: var(--bs-primary); margin-right:0.36rem; margin-top:0.05rem;"
                    ),
                    tags$div(
                      tags$strong("Case Completeness", style = "color: var(--bs-primary); text-transform: uppercase;"),
                      tags$br(),
                      tags$span(
                        style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace);",
                        paste0(case_completeness, "%")
                      )
                    )
                  )
                )
              ),
              tags$div(
                class = "row",
                style = "font-size:0.8rem;line-height:1.14;color:#8A7A8F;",
                tags$div(
                  class = "d-flex align-items-start mb-1",
                  ph("check-circle",
                     weight = "regular", size = "1.5x",
                     style = "color: var(--bs-primary); margin-right:0.36rem; margin-top:0.05rem;"
                  ),
                  tags$div(
                    tags$strong("Cleaned", style = "color: var(--bs-primary); text-transform: uppercase;"),
                    tags$br(),
                    tags$span(
                      style = "color: var(--bs-primary); text-transform: uppercase;",
                      extra_info,
                      if (!is.null(extra_info)) ", ",
                      "Data Structure Verified"
                    )
                  )
                )
              )
            )
          ),
          bslib::card_footer(
            style = "\n          background: var(--bs-body-bg);\n          border-top:1px solid #e8ddd4;\n          padding:0.5rem 1rem; text-align:center;",
            actionButton(
              inputId = ns(input_id),
              label = tagList(
                ph("table", weight = "fill", style = "margin-right:0.45rem;"),
                "VIEW CLEANED DATA"
              ),
              class = "text-uppercase fw-semibold",
              style = paste0("background-color: #8A7A8F; color: var(--bs-body-bg); border: none;\n                  padding:0.44rem 1.1rem;font-size:0.7rem;letter-spacing:0.4px;border-radius:6px;")
            )
          )
        )
      )
    }
  )
  do.call(layout_columns, c(list(col_widths = c(5, 7), gap = 0), cols))
}
