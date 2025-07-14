#!! ModName = mod_visualize
# !! ModDisplayName = Enter your module shiny display name here.
# !! ModDescription = Enter your module description here.
# !! ModCitation = Price, Jeremy F.  (2025). mod_visualize. [Source code].
# !! ModNotes = Enter your module notes here.
# !! ModActive = 1/0
# !! FunctionArg = argName1 !! argDescription !! argClass
# !! FunctionArg = argName2 !! argDescription !! argClass
# !! FunctionReturn = returnName1 !! returnDescription !! returnClass
# !! FunctionReturn = returnName2 !! returnDescription !! returnClass


# the ui function
mod_visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::accordion(
      id = ns("visualize_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "overview",
        title = tagList(phosphoricons::ph("lighthouse"), HTML("&nbsp;"), "Overview"),
        fluidRow(
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
              src = "visualize.png",
              alt = "Visualize Data Image",
              style = "max-width: 100%; height: auto; margin-top: 20px; padding-left: 10%; padding-right: 10%;"
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "visualize_data",
        title = tagList(phosphoricons::ph("blueprint"), HTML("&nbsp;"), "Visualize Data"),
        p(
          "This module allows you to visualize your project data. Select the type of visualization you want to perform and run the process. Use the button below to run the selected visualization."
        ),
        fluidRow(
          shinyWidgets::radioGroupButtons(
            inputId = ns("visualization_type"),
            label = "SELECT VISUALIZATION TYPE",
            choices = list(
              "Visualize Indicators" = "indicators",
              "Visualize Alignment Data" = "alignment",
              "Visualize Dynamics Data" = "dynamics",
              "Visualize Cascade Data" = "cascade",
              "Visualize All Data" = "full"
            ),
            selected = NULL,
            direction = "horizontal",
            justified = TRUE
          )
        ),
        fluidRow(
          class = "d-flex justify-content-center",
          div(
            id = ns("progress_visualization_container"),
            class = "progress-container",
            style = "width: 50%; visibility: hidden;",
            tags$p("Running Visualization...", class = "progress-label", id = ns("progress_visualization_label")),
            div(
              class = "progress",
              div(
                class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar",
                style = "width: 0%;",
                `aria-valuenow` = "0",
                `aria-valuemin` = "0",
                `aria-valuemax` = "100",
                id = ns("progress_visualization_bar_inner")
              )
            )
          )
        ),
        fluidRow(
          class = "d-flex justify-content-center",
          div(
            style = "width: 50%; position: relative;",
            uiOutput(ns("visualize_status_alert_ui"))
          )
        ),
        fluidRow(
          class = "d-flex justify-content-center align-items-center",
          actionButton(
            inputId = ns("run_visualization"),
            label = "Run Visualization",
            class = "btn-primary",
            style = "width: 20%; margin: 0 auto; display: block;"
          )
        )
      ),
      bslib::accordion_panel(
        value = "indicators_panel",
        title = tagList(phosphoricons::ph("gauge"), HTML("&nbsp;"), "Indicators"),
        # Place your indicators UI here
      ),
      bslib::accordion_panel(
        value = "alignment_panel",
        title = tagList(phosphoricons::ph("flower-lotus"), HTML("&nbsp;"), "Alignment"),
        # Place your alignment UI here
      ),
      bslib::accordion_panel(
        value = "dynamics_panel",
        title = tagList(phosphoricons::ph("pulse"), HTML("&nbsp;"), "Dynamics"),
        # Place your dynamics UI here
      ),
      bslib::accordion_panel(
        value = "cascade_panel",
        title = tagList(phosphoricons::ph("waveform"), HTML("&nbsp;"), "Cascade Effects"),
        # Place your cascade effects UI here
      )
    )
  )
}


# the server function
mod_visualize_server <- function(id, ns_workflow) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create workflow observers using centralized utility functions
    create_workflow_observers(ns_workflow, ns, session)

    # Track visualization completion
    rv_visualization <- reactiveValues(
      alignment_visualized = FALSE,
      indicators_visualized = FALSE,
      dynamics_visualized = FALSE,
      cascade_visualized = FALSE,
      full_visualized = FALSE
    )

    # --- Observe per-metric workflow state for Alignment Clean Data completion and update status/icon ---
    observe({
      wf <- ns_workflow$workflow$alignment
      if (!is.null(wf) && wf$stage == "Clean Data" && wf$status == "complete") {
        update_alignment_status_display("cleaned", session, ns, ns_workflow)
        update_alignment_workflow_icons(ns_workflow, session)
      }
    })

    # Visualize Alignment Data
    observeEvent(input$visualize_alignment, {
      logger::log_info("Visualize Alignment button clicked")

      tryCatch(
        {
          # 1. Update status message
          rv_visualization$type <- "info"
          rv_visualization$message <- "Visualization in progress..."
          
          # 2. Delay before showing progress bar
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_visualization_container")))
          })

          # Update progress bar
          shinyWidgets::updateProgressBar(session = session, id = "visualize_alignment_progress", value = 50)

          # Simulate visualization process
          Sys.sleep(1) # Remove this in actual implementation

          # Mark as completed
          rv_visualization$alignment_visualized <- TRUE

          # Update workflow step for alignment visualization completion
          update_workflow_step(
            ns_workflow,
            stage = "Visualize Findings",
            status = "complete",
            session = session
          )

          # Update alignment workflow icons
          update_alignment_workflow_icons(ns_workflow, session, ns)

          # Complete progress bar
          shinyWidgets::updateProgressBar(session = session, id = "visualize_alignment_progress", value = 100)

          logger::log_info("Alignment visualization completed")
          showNotification("Alignment visualization completed successfully!", type = "success")
        },
        error = function(e) {
          logger::log_error("Error in alignment visualization: {conditionMessage(e)}")
          showNotification("Error in alignment visualization", type = "error")
          shinyWidgets::updateProgressBar(session = session, id = "visualize_alignment_progress", value = 0)
        }
      )
    })

    # Visualize Indicators Data
    observeEvent(input$visualize_indicators, {
      logger::log_info("Visualize Indicators button clicked")

      tryCatch(
        {
          # 1. Update status message
          rv_visualization$type <- "info"
          rv_visualization$message <- "Visualization in progress..."
          
          # 2. Delay before showing progress bar
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_visualization_container")))
          })

          # Update progress bar
          shinyWidgets::updateProgressBar(session = session, id = "visualize_indicators_progress", value = 50)

          # Simulate visualization process
          Sys.sleep(1) # Remove this in actual implementation

          # Mark as completed
          rv_visualization$indicators_visualized <- TRUE

          # Update workflow step for indicators visualization completion
          update_workflow_step(
            ns_workflow,
            stage = "Visualize Findings",
            status = "complete",
            session = session
          )

          # Update main data workflow icons for indicators visualization completion
          update_main_data_workflow_icons("Visualize Findings", "complete", session, ns, ns_workflow)

          # Complete progress bar
          shinyWidgets::updateProgressBar(session = session, id = "visualize_indicators_progress", value = 100)

          logger::log_info("Indicators visualization completed")
          showNotification("Indicators visualization completed successfully!", type = "success")
        },
        error = function(e) {
          logger::log_error("Error in indicators visualization: {conditionMessage(e)}")
          showNotification("Error in indicators visualization", type = "error")
          shinyWidgets::updateProgressBar(session = session, id = "visualize_indicators_progress", value = 0)
        }
      )
    })

    # Visualize Dynamics Data
    observeEvent(input$visualize_dynamics, {
      logger::log_info("Visualize Dynamics button clicked")

      tryCatch(
        {
          # 1. Update status message
          rv_visualization$type <- "info"
          rv_visualization$message <- "Visualization in progress..."
          
          # 2. Delay before showing progress bar
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_visualization_container")))
          })

          # Update progress bar
          shinyWidgets::updateProgressBar(session = session, id = "visualize_dynamics_progress", value = 50)

          # Simulate visualization process
          Sys.sleep(1) # Remove this in actual implementation

          # Mark as completed
          rv_visualization$dynamics_visualized <- TRUE

          # Update workflow step for dynamics visualization completion
          update_workflow_step(
            ns_workflow,
            stage = "Visualize Findings",
            status = "complete",
            session = session
          )

          # Update main data workflow icons for dynamics visualization completion
          update_main_data_workflow_icons("Visualize Findings", "complete", session, ns, ns_workflow)

          # Complete progress bar
          shinyWidgets::updateProgressBar(session = session, id = "visualize_dynamics_progress", value = 100)

          logger::log_info("Dynamics visualization completed")
          showNotification("Dynamics visualization completed successfully!", type = "success")
        },
        error = function(e) {
          logger::log_error("Error in dynamics visualization: {conditionMessage(e)}")
          showNotification("Error in dynamics visualization", type = "error")
          shinyWidgets::updateProgressBar(session = session, id = "visualize_dynamics_progress", value = 0)
        }
      )
    })

    # Visualize Cascade Data
    observeEvent(input$visualize_cascade, {
      logger::log_info("Visualize Cascade button clicked")

      tryCatch(
        {
          # 1. Update status message
          rv_visualization$type <- "info"
          rv_visualization$message <- "Visualization in progress..."
          
          # 2. Delay before showing progress bar
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_visualization_container")))
          })

          # Update progress bar
          shinyWidgets::updateProgressBar(session = session, id = "visualize_cascade_progress", value = 50)

          # Simulate visualization process
          Sys.sleep(1) # Remove this in actual implementation

          # Mark as completed
          rv_visualization$cascade_visualized <- TRUE

          # Update workflow step for cascade visualization completion
          update_workflow_step(
            ns_workflow,
            stage = "Visualize Findings",
            status = "complete",
            session = session
          )

          # Update main data workflow icons for cascade visualization completion
          update_main_data_workflow_icons("Visualize Findings", "complete", session, ns, ns_workflow)

          # Complete progress bar
          shinyWidgets::updateProgressBar(session = session, id = "visualize_cascade_progress", value = 100)

          logger::log_info("Cascade visualization completed")
          showNotification("Cascade visualization completed successfully!", type = "success")
        },
        error = function(e) {
          logger::log_error("Error in cascade visualization: {conditionMessage(e)}")
          showNotification("Error in cascade visualization", type = "error")
          shinyWidgets::updateProgressBar(session = session, id = "visualize_cascade_progress", value = 0)
        }
      )
    })

    # Run Full Visualization
    observeEvent(input$visualize_full, {
      logger::log_info("Run Full Visualization button clicked")

      tryCatch(
        {
          # 1. Update status message
          rv_visualization$type <- "info"
          rv_visualization$message <- "Visualization in progress..."
          
          # 2. Delay before showing progress bar
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_visualization_container")))
          })

          # Update progress bar
          shinyWidgets::updateProgressBar(session = session, id = "full_visualization_progress", value = 25)

          # Simulate full visualization process
          Sys.sleep(0.5)
          shinyWidgets::updateProgressBar(session = session, id = "full_visualization_progress", value = 50)
          Sys.sleep(0.5)
          shinyWidgets::updateProgressBar(session = session, id = "full_visualization_progress", value = 75)
          Sys.sleep(0.5)

          # Mark all as completed
          rv_visualization$alignment_visualized <- TRUE
          rv_visualization$indicators_visualized <- TRUE
          rv_visualization$dynamics_visualized <- TRUE
          rv_visualization$cascade_visualized <- TRUE
          rv_visualization$full_visualized <- TRUE

          # Update workflow steps for full visualization completion
          update_workflow_step(
            ns_workflow,
            stage = "Visualize Findings",
            status = "complete",
            metric = "alignment",
            session = session
          )

          # Update workflow step for main data visualization completion
          update_workflow_step(
            ns_workflow,
            stage = "Visualize Findings",
            status = "complete",
            metric = "indicators",
            session = session
          )
          update_workflow_step(
            ns_workflow,
            stage = "Visualize Findings",
            status = "complete",
            metric = "dynamics",
            session = session
          )
          update_workflow_step(
            ns_workflow,
            stage = "Visualize Findings",
            status = "complete",
            metric = "cascade",
            session = session
          )

          # Update all workflow icons
          update_alignment_workflow_icons(ns_workflow, session, ns)
          update_main_data_workflow_icons("Visualize Findings", "complete", session, ns, ns_workflow)

          # Complete progress bars
          shinyWidgets::updateProgressBar(session = session, id = "full_visualization_progress", value = 100)
          shinyWidgets::updateProgressBar(session = session, id = "visualize_alignment_progress", value = 100)
          shinyWidgets::updateProgressBar(session = session, id = "visualize_indicators_progress", value = 100)
          shinyWidgets::updateProgressBar(session = session, id = "visualize_dynamics_progress", value = 100)
          shinyWidgets::updateProgressBar(session = session, id = "visualize_cascade_progress", value = 100)

          logger::log_info("Full visualization completed")
          showNotification("Full visualization completed successfully!", type = "success")
        },
        error = function(e) {
          logger::log_error("Error in full visualization: {conditionMessage(e)}")
          showNotification("Error in full visualization", type = "error")
          shinyWidgets::updateProgressBar(session = session, id = "full_visualization_progress", value = 0)
        }
      )
    })

    # Initialize workflow icons on module load with current state
    observe({
      # Workflow icons are now updated centrally by the main server observer
      logger::log_info("Workflow icons initialization skipped - handled centrally")
    })

    output$visualize_status_alert_ui <- renderUI({
      req(rv_visualization$type, rv_visualization$message)
      icon_name <- switch(rv_visualization$type,
                          "success" = "check-circle",
                          "danger"  = "x-circle",
                          "info"    = "info",
                          "warning" = "warning-circle")
      div(
        class = paste0("alert alert-", rv_visualization$type, " mt-4 d-flex align-items-center"),
        phosphoricons::ph(icon_name, weight = "fill", class = "alert-icon"),
        tags$span(rv_visualization$message, class = "alert-message-text")
      )
    })

    return(
      reactiveValues(
        alignment_visualized = reactive(rv_visualization$alignment_visualized),
        indicators_visualized = reactive(rv_visualization$indicators_visualized),
        dynamics_visualized = reactive(rv_visualization$dynamics_visualized),
        cascade_visualized = reactive(rv_visualization$cascade_visualized),
        full_visualized = reactive(rv_visualization$full_visualized)
      )
    )
  })
}
