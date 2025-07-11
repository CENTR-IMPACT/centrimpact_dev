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
    bslib::navset_card_pill(
      id = ns("visualize_tabs"),
      bslib::nav_panel(
        "Overview",
        icon = phosphoricons::ph("lighthouse"),
        h1(
          "Visualize Data",
          style = "border-bottom: solid 1px #4A4A4A;"
        ),
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
          ),
        )
      ),
      bslib::nav_panel(
        "Visualize Data",
        icon = phosphoricons::ph("blueprint"),
        style = "padding: 1em !important;",
        fluidRow(
          column(
            width = 7,
            create_info_card(
              ns,
              title = "Visualize Data", icon = "blueprint",
              status = "visualize",
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
                  ns("visualize_indicators"),
                  label = tagList(
                    phosphoricons::ph("gauge"),
                    "Visualize Indicators"
                  ),
                  class = "action-button action-indicators",
                  style = "width: 80%;"
                ),
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
                shinyWidgets::progressBar(
                  id = "visualize_indicators_progress",
                  title = tags$span("Visualize Indicators Progress", class = "pb-title", style = "text-align: left !important;"),
                  value = 0,
                  display_pct = FALSE,
                  status = "success"
                ),
                shinyWidgets::progressBar(
                  id = "visualize_alignment_progress",
                  title = tags$span("Visualize Alignment Progress", class = "pb-title", style = "text-align: left !important;"),
                  value = 0,
                  display_pct = FALSE,
                  status = "success"
                ),
                shinyWidgets::progressBar(
                  id = "visualize_dynamics_progress",
                  value = 0,
                  display_pct = FALSE,
                  title = tags$span("Visualize Dynamics Progress", class = "pb-title", style = "text-align: left !important;"),
                  status = "success"
                ),
                shinyWidgets::progressBar(
                  id = "visualize_cascade_progress",
                  value = 0,
                  title = tags$span("Visualize Cascade Progress", class = "pb-title", style = "text-align: left !important;"),
                  display_pct = FALSE,
                  status = "success"
                )
              ),
              title = "Actions",
              icon = "person-simple-tai-chi"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            h1(
              "Visualize Data",
              style = "border-bottom: solid 1px #4A4A4A; margin-bottom: .25em;"
            ),
            p("This module allows you to visualize your project data. Select the type of visualization you want to perform and run the process Use the buttons below to run each visualization."),
          ),
          column(
            width = 6,
            bslib::card(
              class = "infocard neumorphic",
              style = "width: 100% !important; height: 100% !important;",
              bslib::card_header(
                style = "color: var(--bs-danger); box-shadow: inset 0 6px 12px #8e838066;",
                class = "infocard",
                tagList(
                  ph("person-simple-tai-chi", weight = "bold"),
                  "Actions"
                )
              ),
              bslib::card_body(
                class = "infocard",
                style = "padding: 20px 0px 0px 0px !important;",
                fluidRow(
                  style = "height: 18%;",
                  class = "action-row",
                  column(
                    width = 7,
                    style = "text-align: center;",
                    actionButton(
                      ns("visualize_alignment"),
                      label = tagList(
                        phosphoricons::ph("flower-lotus"),
                        "Visualize Alignment Data"
                      ),
                      style = "width: 90%; color: #4A4A4A; background: #d2bfa3 !important;",
                      class = "btn-neumorphic"
                    )
                  ),
                  column(
                    width = 5,
                    style = "text-align: center;",
                    div(
                      style = "width: 90%; text-align: center;",
                      class = "justify-content-center",
                      shinyWidgets::progressBar(
                        id = "visualize_alignment_progress",
                        title = tags$span("Visualize Alignment Progress", class = "pb-title", style = "text-align: left !important;"),
                        value = 0,
                        display_pct = FALSE,
                        status = "success"
                      )
                    )
                  )
                ),
                fluidRow(
                  style = "height: 18%;",
                  class = "action-row",
                  column(
                    width = 7,
                    style = "text-align: center;",
                    actionButton(
                      ns("visualize_indicators"),
                      label = tagList(
                        phosphoricons::ph("gauge"),
                        "Visualize Indicator Data"
                      ),
                      style = "width: 90%; color: #4A4A4A; background: #d2bfa3 !important;",
                      class = "btn-neumorphic"
                    )
                  ),
                  column(
                    width = 5,
                    style = "text-align: center;",
                    div(
                      style = "width: 90%; text-align: center;",
                      class = "justify-content-center",
                      shinyWidgets::progressBar(
                        id = "visualize_indicators_progress",
                        title = tags$span("Visualize Indicators Progress", class = "pb-title", style = "text-align: left !important;"),
                        value = 0,
                        display_pct = FALSE,
                        status = "success"
                      )
                    )
                  )
                ),
                fluidRow(
                  style = "height: 18%;",
                  class = "action-row",
                  column(
                    width = 7,
                    style = "text-align: center;",
                    actionButton(ns("visualize_dynamics"),
                      label = tagList(
                        phosphoricons::ph("pulse"),
                        "Visualize Dynamics Data"
                      ),
                      style = "width: 90% !important;",
                      class = "btn-neumorphic"
                    )
                  ),
                  column(
                    width = 5,
                    style = "text-align: center;",
                    div(
                      style = "width: 90%; text-align: center;",
                      class = "justify-content-center",
                      shinyWidgets::progressBar(
                        id = "visualize_dynamics_progress",
                        value = 0,
                        display_pct = FALSE,
                        title = tags$span("Visualize Dynamics Progress", class = "pb-title", style = "text-align: left !important;"),
                        status = "success"
                      )
                    )
                  )
                ),
                fluidRow(
                  style = "height: 18%;",
                  class = "action-row",
                  column(
                    width = 7,
                    style = "text-align: center;",
                    actionButton(ns("visualize_cascade"),
                      label = tagList(
                        phosphoricons::ph("waveform"),
                        "Visualize Cascade Data"
                      ),
                      style = "width: 90%;",
                      class = "btn-neumorphic"
                    )
                  ),
                  column(
                    width = 5,
                    div(
                      style = "width: 90%; text-align: center;",
                      class = "justify-content-center",
                      shinyWidgets::progressBar(
                        id = "visualize_cascade_progress",
                        value = 0,
                        title = tags$span("Visualize Cascade Progress", class = "pb-title", style = "text-align: left !important;"),
                        display_pct = FALSE,
                        status = "success"
                      )
                    )
                  )
                ),
                fluidRow(
                  style = "height: 18%;",
                  class = "action-row",
                  column(
                    width = 7,
                    style = "text-align: center;",
                    actionButton(ns("visualize_full"),
                      label = tagList(
                        phosphoricons::ph("blueprint"),
                        "Run Full Visualization"
                      ),
                      class = "btn-neumorphic run-analysis-button w-90",
                      style = "width: 90%;",
                    )
                  ),
                  column(
                    width = 5,
                    div(
                      style = "width: 90%; text-align: center;",
                      class = "justify-content-center",
                      shinyWidgets::progressBar(
                        id = "full_visualization_progress",
                        title = tags$span("Full Visualization Progress", class = "pb-title", style = "text-align: left !important;"),
                        value = 0,
                        display_pct = FALSE,
                        status = "success"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # Data View Tabs
      bslib::nav_panel("Indicators",
        value = "indicators_panel", icon = ph("gauge"), # uiOutput(ns("indicators_ui")
      ),
      bslib::nav_panel("Alignment",
        value = "alignment_panel", icon = ph("flower-lotus"),
        # plotOutput("alignment_plot",
        #           width = "90%",      # Leave some margin
        #           height = "50.625vw") # 90% <U+00D7> 56.25% = ~16:9
      ),
      bslib::nav_panel("Dynamics",
        value = "dynamics_panel", icon = ph("pulse")
      ),
      bslib::nav_panel("Cascade Effects",
        value = "cascade_panel", icon = ph("waveform")
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
