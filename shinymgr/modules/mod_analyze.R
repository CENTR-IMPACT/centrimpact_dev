#!! ModName = mod_analyze
# !! ModDisplayName = Analyze Data
# !! ModDescription = Analyze project data including alignment, dynamics, and cascade metrics
# !! ModCitation = Price, Jeremy F. (2025). mod_analyze. [Source code].
# !! ModNotes = This module provides functionality to analyze project data across multiple dimensions.
# !! ModActive = 1
# !! FunctionArg = project_data !! Project data for analysis !! reactive

# Load required libraries and utilities (assumed to be in global.R)
#' @importFrom phosphoricons ph ph_i
#' @importFrom shiny NS tagList observe observeEvent reactive reactiveVal reactiveValues req debounce updateTextInput updateDateInput updateTextAreaInput renderUI showNotification renderTable uiOutput
#' @importFrom shinyjs useShinyjs delay html runjs
#' @importFrom bslib navset_card_tab nav_panel card card_body accordion accordion_panel value_box value_box_theme tooltip layout_columns
#' @importFrom DT renderDataTable datatable dataTableOutput
#' @importFrom logger log_info log_warn log_error log_trace
#' @importFrom shinyWidgets radioGroupButtons progressBar updateProgressBar
#' @importFrom centrimpact analyze_alignment analyze_dynamics analyze_cascade
#' @importFrom utils capture.output str head write.csv

# Minimal logger:: logging for module load
logger::log_info("[@mod_analyze.R] Analyze module loaded")

# Define null-coalescing operator for cleaner code
`%||%` <- function(x, y) if (is.null(x)) y else x

#==============================================================================
#
# UI FUNCTION
#
#==============================================================================

mod_analyze_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    bslib::accordion(
      id = ns("analyze_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "analyze_data",
        title = tagList(ph("calculator"), HTML("&nbsp;"), "Analyze Data"),
        fluidRow(
          column(
            width = 11,
            p("This module allows you to analyze your project data. Select the type of analysis you want to perform and run the analysis."),
            tags$fieldset(
              class = "custom-fieldset",
              tags$legend("Select", class = "custom-legend"),
              shinyWidgets::radioGroupButtons(
                inputId = ns("analysis_type"),
                label = "ANALYSIS TYPE",
                choices = list(
                  "All Data" = "full",
                  "Alignment Data" = "alignment",
                  "Dynamics Data" = "dynamics",
                  "Cascade Data" = "cascade"
                ),
                selected = "full",
                direction = "horizontal",
                justified = TRUE
              )
            ),
            tags$fieldset(
              class = "custom-fieldset",
              tags$legend("Analyze", class = "custom-legend"),
              fluidRow(
                class = "d-flex justify-content-center align-items-center",
                actionButton(
                  inputId = ns("run_analysis"),
                  label = tagList("Analyze Data ", ph("calculator", weight = "bold")),
                  width = "50%",
                  class = "btn btn-primary btn-lg"
                )
              ),
              fluidRow(
                class = "d-flex justify-content-center",
                div(
                  id = ns("progress_container"),
                  style = "width: 50%; visibility: hidden; margin-top: 1em;",
                  shinyWidgets::progressBar(
                    id = ns("progress_bar"),
                    value = 0,
                    total = 100,
                    status = "success",
                    title = "Ready to Analyze"
                  )
                )
              )
            )
          ),
          column(
            width = 1,
            img(
              src = "analyze.png",
              alt = "Analyze Data Image",
              style = "width: auto; max-height: 7em; margin-top: 0.5em;"
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "results",
        title = tagList(ph("flag-checkered", weight = "fill"), HTML("&nbsp;"), "Results"),
        # Refactored: Use separate UI outputs for each result type.
        # This prevents re-rendering everything when only one result changes.
        uiOutput(ns("results_ui"))
      )
    )
  )
}


#==============================================================================
#
# SERVER FUNCTION
#
#==============================================================================

mod_analyze_server <- function(id, project_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    logger::log_info("[@mod_analyze.R] Module server initialized for id: {id}")

    # Initialize analysis reactiveValues if they don't exist
    if (is.null(project_data$analysis)) {
      project_data$analysis <- reactiveValues(
        alignment_analyzed = FALSE,
        dynamics_analyzed = FALSE,
        cascade_analyzed = FALSE,
        last_analysis_type = NULL,
        last_updated = NULL,
        alignment_score = NULL,
        alignment_table = NULL,
        alignment_icc = NULL,
        dynamics_score = NULL,
        dynamics_domains = NULL,
        dynamics_dynamics = NULL,
        cascade_score = NULL,
        cascade_data = NULL
      )
    }

    # =========================================================================
    # >> HELPER: ANALYSIS FUNCTIONS
    #
    # These functions are now defined once in the server scope, not inside
    # an observer. They contain the core logic for running each analysis type
    # and return a standardized list structure.
    # =========================================================================

    #' @title Run Alignment Analysis
    #' @description Wraps the centrimpact::analyze_alignment call with validation.
    #' @param data A data.frame with alignment data.
    #' @return A standardized list: list(score, table, icc).
    run_alignment_analysis <- function(data) {
      req(data)
      logger::log_info("Running alignment analysis...")
      result <- centrimpact::analyze_alignment(alignment_df = data)
      logger::log_info("Alignment analysis complete.")
      return(list(
        score = result$alignment_score %||% NA_real_,
        table = result$alignment_medians %||% data.frame(),
        icc = result$icc_score %||% NA_real_
      ))
    }

    #' @title Run Dynamics Analysis
    #' @description Wraps the centrimpact::analyze_dynamics call with validation.
    #' @param data A data.frame with dynamics data.
    #' @return A standardized list: list(score, domains, dynamics).
    run_dynamics_analysis <- function(data) {
      req(data)
      logger::log_info("Running dynamics analysis...")
      result <- centrimpact::analyze_dynamics(dynamics_df = data)
      logger::log_info("Dynamics analysis complete.")
      return(list(
        score = result$dynamics_score %||% NA_real_,
        domains = result$domain_df %||% data.frame(),
        dimensions = result$dynamics_df %||% data.frame()
      ))
    }

    #' @title Run Cascade Analysis
    #' @description Wraps the centrimpact::analyze_cascade call with validation.
    #' @param data A data.frame or list with cascade data.
    #' @return A standardized list: list(score, data).
    run_cascade_analysis <- function(data) {
      req(data)
      # The analysis function might expect a specific structure (e.g., an edgelist)
      # This helper can prepare the data if needed.
      cascade_input <- if (is.list(data) && !is.null(data$edges)) data$edges else data
      
      logger::log_info("Running cascade analysis...")
      result <- centrimpact::analyze_cascade(cascade_input)
      logger::log_info("Cascade analysis complete.")
      return(list(
        score = result$cascade_score %||% NA_real_,
        data = result$cascade_df %||% data.frame()
      ))
    }

    # =========================================================================
    # >> REACTIVE DATA ACCESSORS
    # =========================================================================

    alignment_data <- reactive({ project_data$cleaned_data$alignment %||% NULL })
    dynamics_data <- reactive({ project_data$cleaned_data$dynamics %||% NULL })
    cascade_data <- reactive({ project_data$cleaned_data$cascade %||% NULL })


    # =========================================================================
    # >> OBSERVER: RUN ANALYSIS
    #
    # This is the primary observer for the "Analyze Data" button. It has been
    # refactored to be a generic runner that calls the specific helper
    # functions. It handles progress updates and notifications.
    # =========================================================================

    observeEvent(input$run_analysis, {
      req(input$analysis_type)
      analysis_type <- input$analysis_type
      log_info("=== RUN ANALYSIS: {analysis_type} ===")

      # --- Progress Bar Setup ---
      shinyjs::runjs(sprintf('document.getElementById("%s").style.visibility = "visible";', ns("progress_container")))
      update_progress <- function(value, text) {
        shinyWidgets::updateProgressBar(session, ns("progress_bar"), value = value, title = text)
      }
      update_progress(0, "Starting Analysis...")

      # --- Generic Analysis Runner ---
      run_single_analysis <- function(type, data_func, analysis_func) {
        data <- data_func()
        
        # FIX: More robust data validation to prevent errors with lists vs data.frames
        is_data_missing <- if (type %in% c("alignment", "dynamics")) {
          !is.data.frame(data) || nrow(data) == 0
        } else {
          is.null(data)
        }
        
        if (is_data_missing) {
          log_warn("No valid data available for '{type}' analysis. Skipping.")
          showNotification(paste("Skipping", type, "analysis: No data."), type = "warning")
          return(NULL)
        }
        
        tryCatch({
          result <- analysis_func(data)
          # Store results in the main reactiveValues object
          isolate({
            project_data$analyzed_data[[paste0(type, "_score")]] <- result$score
            project_data$analyzed_data[[paste0(type, "_analyzed")]] <- TRUE
            # Store other parts of the result based on type
            if(type == "alignment") {
              project_data$analyzed_data$alignment_table <- result$table
              project_data$analyzed_data$alignment_score <- result$icc
            } else if (type == "dynamics") {
              project_data$analyzed_data$dynamics_domains_table <- result$domains
              project_data$analyzed_data$dynamics_dimensions_table <- result$dynamics
              project_data$analyzed_data$dynamics_score <- result$score
            } else if (type == "cascade") {
              project_data$analyzed_data$cascade_table <- result$data
              project_data$analyzed_data$cascade_score <- result$score
            }
          })
          showNotification(paste(tools::toTitleCase(type), "analysis complete!"), type = "message")
          return(TRUE)
        }, error = function(e) {
          log_error("Analysis failed for type '{type}': {e$message}")
          showNotification(paste("Error in", type, "analysis:", e$message), type = "error")
          return(FALSE)
        })
      }

      # --- Execute Analysis ---
      if (analysis_type == "full") {
        update_progress(10, "Analyzing Alignment...")
        run_single_analysis("alignment", alignment_data, run_alignment_analysis)
        update_progress(40, "Analyzing Dynamics...")
        run_single_analysis("dynamics", dynamics_data, run_dynamics_analysis)
        update_progress(70, "Analyzing Cascade...")
        run_single_analysis("cascade", cascade_data, run_cascade_analysis)
        update_progress(100, "All Analyses Finished")
      } else {
        run_single_analysis(analysis_type, get(paste0(analysis_type, "_data")), get(paste0("run_", analysis_type, "_analysis")))
        update_progress(100, "Analysis Finished")
      }

      # --- Finalize ---
      isolate({
        project_data$analyzed_data$last_analysis_type <- analysis_type
        project_data$analyzed_data$last_updated <- Sys.time()
      })
      delay(2000, shinyjs::runjs(sprintf('document.getElementById("%s").style.visibility = "hidden";', ns("progress_container"))))
    })

    output$results_ui <- renderUI({
      ns <- session$ns
      
      # Debug: Log basic structure information
      logger::log_info("analyzed_data names: {paste(names(project_data$analyzed_data), collapse = ", ")}")
      
      # Check if alignment data exists and log its structure
      if (!is.null(project_data$analyzed_data$alignment)) {
        logger::log_info("Alignment data exists. Names: {paste(names(project_data$analyzed_data$alignment), collapse = ", ")}")
        
        # Check if alignment_table exists and log its structure
        if (!is.null(project_data$analyzed_data$alignment$alignment_table)) {
          logger::log_info("alignment_table class: {class(project_data$analyzed_data$alignment$alignment_table)}")
          logger::log_info("alignment_table dimensions: {nrow(project_data$analyzed_data$alignment$alignment_table)} rows, {ncol(project_data$analyzed_data$alignment$alignment_table)} columns")
          logger::log_info("alignment_table column names: {paste(names(project_data$analyzed_data$alignment$alignment_table), collapse = ", ")}")
        } else {
          logger::log_info("alignment_table is NULL")
        }
        
        # Log alignment score
        if (!is.null(project_data$analyzed_data$alignment$alignment_score)) {
          logger::log_info("alignment_score: {project_data$analyzed_data$alignment$alignment_score}")
        } else {
          logger::log_info("alignment_score is NULL")
        }
      } else {
        logger::log_info("No alignment data found in project_data$analyzed_data")
      }
      
      tagList(
        # -- ALIGNMENT SECTION --
        metric_section_ui(
          data = project_data$analyzed_data$alignment_table,
          
          # Value box parameters
          title = "Alignment Score",
          value = project_data$analyzed_data$alignment_score,
          
          # Card parameters  
          card_header_text = "Ready for Visualization",
          card_body = tagList(
            p("Hello, this is the alignment section!"),
          ),
          card_footer_button_text = "VIEW Data Table", 
          card_footer_button_id = "view_alignment_overlay",
          
          # Placeholder parameters (shown when data is NULL/empty)
          placeholder_title = "No Project Alignment Data",
          placeholder_text = "Upload data to begin",
          placeholder_icon = ph("upload", weight = "fill", size = "2x"),
          
          # Required namespace function
          ns = ns,
          
          # Custom fieldset title (defaults to 'title' if not provided)
          fieldset_title = "Project Alignment"
        ),
        # -- DYNAMICS SECTION --
        metric_section_ui(
          data = project_data$analyzed_data$dynamics_domains_table,
          
          # Value box parameters
          title = "Project Dynamics Data",
          value = project_data$analyzed_data$dynamics_score,
          value_subtitle = "Records Processed",
          
          # Card parameters  
          card_header_text = "Ready for Visualization",
          card_body = tagList(
            p("Hello, this is the dynamics section!"),
          ),
          card_footer_button_text = "VIEW Data Table",
          card_footer_button_id = "view_dynamics_overlay",
          
          # Placeholder parameters (shown when data is NULL/empty)
          placeholder_title = "No Project Dynamics Data",
          placeholder_text = "Upload data to begin",
          placeholder_icon = ph("upload", weight = "fill", size = "2x"),
          
          # Required namespace function
          ns = ns,
          
          # Custom fieldset title (defaults to 'title' if not provided)
          fieldset_title = "Project Dynamics"
        ),
        # -- CASCADE SECTION --
        metric_section_ui(
          data = project_data$analyzed_data$cascade_table,
          
          # Value box parameters
          title = "Cascade Effects Data",
          value = project_data$analyzed_data$cascade_score,
          value_subtitle = "Records Processed",
          
          # Card parameters  
          card_header_text = "Ready for Visualization",
          card_body = tagList(
            p("Hello, this is the cascade section!"),
          ),
          card_footer_button_text = "VIEW Data Table", 
          card_footer_button_id = "view_cascade_overlay",
          
          # Placeholder parameters (shown when data is NULL/empty)
          placeholder_title = "No Cascade Effects Data",
          placeholder_text = "Upload data to begin",
          placeholder_icon = ph("upload", weight = "fill", size = "2x"),
          
          # Required namespace function
          ns = ns,
          
          # Custom fieldset title (defaults to 'title' if not provided)
          fieldset_title = "Cascade Effects"
        )
      )
    })


    # =========================================================================
    # >> DATA TABLE RENDERING
    # =========================================================================

    output$alignment_table <- DT::renderDataTable({
      req(project_data$analyzed_data$alignment_table)
      DT::datatable(
        project_data$analyzed_data$alignment_table,
        options = list(pageLength = 5, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      )
    })

    output$dynamics_table <- DT::renderDataTable({
      # The dynamics analysis returns multiple tables, choose the primary one
      req(project_data$analyzed_data$dynamics_domains)
      DT::datatable(
        project_data$analyzed_data$dynamics_domains,
        options = list(pageLength = 5, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      )
    })
    
    output$cascade_table <- DT::renderDataTable({
      req(project_data$analyzed_data$cascade_data)
      DT::datatable(
        project_data$analyzed_data$cascade_data,
        options = list(pageLength = 5, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      )
    })

  }) # End moduleServer
} # End mod_analyze_server
