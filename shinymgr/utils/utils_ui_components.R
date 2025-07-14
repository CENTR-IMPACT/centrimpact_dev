# Reusable UI Components for shinymgr
# This file contains reusable UI components that can be used across different modules

create_status_card <- function(ns,
                               show_workflow_icons = TRUE,
                               show_score_displays = TRUE,
                               show_report_icons = TRUE,
                               show_project_info = TRUE) {
  # Build the card body content based on parameters
  card_content <- list(
    # div(
    #   id = "workflow_status_display",
    #   style = "font-weight: bold; color: #D3D3D3; margin-bottom: 0.5em;",
    #   "Not Initiated"
    # )
  )

  # Project info section
  # if (show_project_info) {
  #   card_content <- append(card_content, list(
  #     fluidRow(
  #       div(
  #         style = "font-family: var(--bs-font-code); font-size: 1em; font-weight: bold; text-transform: uppercase;",
  #         class = "d-flex align-items-center justify-content-center gap-2 project-info-text",
  #         id = ns("pi_text"),
  #         phosphoricons::ph("tree-structure", weight = "bold"),
  #         uiOutput(outputId = ns("project_info_text"))
  #       )
  #     )
  #   ))
  # }

  # Workflow icons section
  if (show_workflow_icons) {
    card_content <- append(card_content, list(
      fluidRow(
        class = "action-row",
        style = "padding-left: 2em; padding-right: 2em;",
        shiny::div(
          class = "d-flex justify-content-between",
          style = "font-size: 1.1em; margin-bottom: 10px;",
          shiny::span(
            id = ns("workflow-setup"),
            phosphoricons::ph("plant", weight = "bold"),
            style = "color: #D3D3D3;",
            class = "workflow-icon"
          ),
          shiny::span(
            id = ns("workflow-upload"),
            phosphoricons::ph("upload-simple", weight = "bold"),
            style = "color: #D3D3D3;",
            class = "workflow-icon"
          ),
          shiny::span(
            id = ns("workflow-clean"),
            phosphoricons::ph("broom", weight = "bold"),
            style = "color: #D3D3D3;",
            class = "workflow-icon"
          ),
          shiny::span(
            id = ns("workflow-analyze"),
            phosphoricons::ph("calculator", weight = "bold"),
            style = "color: #D3D3D3;",
            class = "workflow-icon"
          ),
          shiny::span(
            id = ns("workflow-visualize"),
            phosphoricons::ph("blueprint", weight = "bold"),
            style = "color: #D3D3D3;",
            class = "workflow-icon"
          ),
          shiny::span(
            id = ns("workflow-generate"),
            phosphoricons::ph("newspaper-clipping", weight = "bold"),
            style = "color: #D3D3D3;",
            class = "workflow-icon"
          )
        )
      )
    ))
  }

  # Score displays section
  if (show_score_displays) {
    card_content <- append(card_content, list(
      fluidRow(
        div(
          class = "action-row",
          style = "padding-left: 2em; padding-right: 2em; display: flex; align-items: center; justify-content: evenly;",
          div(
            uiOutput(ns("alignment_icon")),
            uiOutput(ns("alignment_score_display")),
            style = "display: flex; font-size: 1.5em; font-weight: 700; align-times: center; justify-content: between; gap: 20px;"
          ),
          div(
            uiOutput(ns("dynamics_icon")),
            uiOutput(ns("dynamics_score_display")),
            style = "display: flex; font-size: 1.5em; font-weight: 700; align-times: center; justify-content: between; gap: 20px;"
          ),
          div(
            uiOutput(ns("cascade_icon")),
            uiOutput(ns("cascade_score_display")),
            style = "display: flex; font-size: 1.5em; font-weight: 700; align-times: center; justify-content: between; gap: 20px;"
          )
        )
      )
    ))
  }

  # Report icons and project date section
  if (show_report_icons || show_project_info) {
    # Build the content for this section
    report_date_content <- list()

    if (show_report_icons) {
      report_date_content <- append(report_date_content, list(
        span(
          style = "margin-left: 0.5rem; color: #D3D3D3;",
          class = "d-flex gap-2 align-items-center justify-content-center",
          uiOutput("report_output_icons")
        )
      ))
    }

    if (show_project_info) {
      report_date_content <- append(report_date_content, list(
        span(
          class = "d-flex gap-1 align-items-center justify-content-end project-info-date",
          style = "font-family: 'Jersey 20'; margin: 0px; margin-right: 0.5rem; font-size: 1.25em;",
          id = "pi_date",
          uiOutput(outputId = "project_info_date"),
          phosphoricons::ph("calendar-dots", weight = "bold")
        )
      ))
    }

    card_content <- append(card_content, list(
      fluidRow(
        div(
          class = "d-flex gap-1 align-items-center justify-content-between w-100",
          do.call(tagList, report_date_content)
        )
      )
    ))
  }

  # Create the card
  bslib::card(
    style = "width: 100% !important;
            border-radius: 10px !important;
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2) !important;
    border: 1px solid #8e8380 !important;",
    bslib::card_header(
      style = "color: #F2ECD7; background: linear-gradient(135deg, rgba(255, 255, 255, 0.1) 0%, rgba(255, 255, 255, 0.2) 100%); text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.2);",
      class = "infocard",
      h1("Status"),
      phosphoricons::ph("terminal", weight = "bold")
    ),
    bslib::card_body(
      class = "infocard",
      style = "padding: 1em 0 1em 0 !important;
      box-shadow: inset 0 4px 10px #8e838044 !important;",
      do.call(tagList, card_content)
    )
  )
}

create_actions_card <- function(
    ns,
    action_buttons = list(),
    progress_bars = list(),
    title = "Actions",
    icon = "person-simple-tai-chi") {
  # Ensure progress_bars list has the same length as action_buttons
  if (length(progress_bars) < length(action_buttons)) {
    progress_bars <- c(progress_bars, rep(list(NULL), length(action_buttons) - length(progress_bars)))
  }

  # Create content for each button + progress bar combination
  button_progress_content <- if (length(action_buttons) > 0) {
    mapply(function(btn, prog_bar, index) {
      if (!is.null(btn)) {
        # Create container for button and progress bar
        fluidRow(
          class = "action-row",
          style = "min-height: 60px; position: relative; margin-bottom: 10px;",
          column(
            class = "d-flex flex-column justify-content-center align-items-center",
            width = 12,
            # Action button
            div(
              style = "width: 100%; margin-bottom: 5px; display: flex; justify-content: center;",
              btn
            ),
            # Progress bar container (initially hidden)
            if (!is.null(prog_bar)) {
              div(
                id = ns(paste0("progress_container_", index)),
                class = "progress-bar-hidden",
                style = "width: 100%; margin-top: 5px;",
                div(
                  class = "progress custom-progressbar-style",
                  prog_bar
                )
              )
            }
          )
        )
      }
    }, action_buttons, progress_bars, seq_along(action_buttons), SIMPLIFY = FALSE)
  } else {
    list()
  }

  bslib::card(
    style = "width: 100% !important;
            border-radius: 10px !important;
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2) !important;
    border: 1px solid #8e8380 !important;",
    bslib::card_header(
      style = "color: #F2ECD7; background: linear-gradient(135deg, rgba(255, 255, 255, 0.1) 0%, rgba(255, 255, 255, 0.2) 100%); text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.2);",
      class = "infocard",
      h1(
        title,
        phosphoricons::ph(icon, weight = "bold", class = "sidebar-icon")
      )
    ),
    bslib::card_body(
      class = "infocard",
      style = "padding: 20px 0px 0px 0px !important;
      box-shadow: inset 0 4px 10px #8e838044 !important;",
      do.call(tagList, button_progress_content)
    )
  )
}

create_info_card <- function(ns, title, icon, status = "test", content, style = NULL) {
  # Build the style string safely
  base_style <- "width: 100% !important;
            border-radius: 10px !important;
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2) !important;
    border: 1px solid #8e8380 !important;"

  final_style <- if (!is.null(style)) {
    paste0(base_style, " ", style)
  } else {
    base_style
  }

  bslib::card(
    style = final_style,
    bslib::card_header(
      style = "color: #F2ECD7; background: linear-gradient(135deg, rgba(255, 255, 255, 0.1) 0%, rgba(255, 255, 255, 0.2) 100%); text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.2);",
      fluidRow(
        column(
          width = 8,
          class = "infocard",
          h1(
            title,
            phosphoricons::ph(icon, weight = "bold", class = "sidebar-icon")
          )
        ),
        column(
          width = 4,
          div(
            class = "status-line",
            column(
              width = 1,
              div(
                style = "color: #3F5E78;",
                id = ns(paste0(status, "_status_icon")),
                ph("check-circle", weight = "bold", class = "sidebar-icon")
              )
            ),
            column(
              width = 11,
              div(
                span(
                  style = "color: #3F5E78;",
                  id = ns(paste0(status, "_status_message")),
                  "Ready"
                )
              )
            )
          )
        )
      )
    ),
    bslib::card_body(
      class = "infocard",
      style = "padding: 1.5em 2.5em 0em 2.5em !important;
      border-radius: 0px 0px 10px 10px !important;
      box-shadow: inset 0 4px 10px #8e838044 !important;",
      content
    )
  )
}


create_flat_info_card <- function(ns, title, icon, content, style = NULL) {
  # Build the style string safely
  base_style <- "width: 100% !important;
            height: 100% !important;"
  
  final_style <- if (!is.null(style)) {
    paste0(base_style, " ", style)
  } else {
    base_style
  }
  
  bslib::card(
    style = final_style,
    bslib::card_header(
          h1(
            title,
            phosphoricons::ph(icon, weight = "bold", class = "sidebar-icon")
        )
        ),
    bslib::card_body(
      content
    )
  )
}

create_pill_info_card <- function(ns, title, icon, tabs, style = NULL, header_style = NULL, ...) {
  # Build the style string safely
  base_style <- "width: 100% !important;
            border-radius: 10px !important;
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2) !important;
    border: 1px solid #8e8380 !important;"

  final_style <- if (!is.null(style)) {
    paste0(base_style, " ", style)
  } else {
    base_style
  }

  header_base_style <- "color: #F2ECD7; background: linear-gradient(135deg, rgba(255, 255, 255, 0.1) 0%, rgba(255, 255, 255, 0.2) 100%); text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.2);"
  final_header_style <- if (!is.null(header_style)) {
    paste0(header_base_style, " ", header_style)
  } else {
    header_base_style
  }

  # Build the tab panels
  tab_panels <- lapply(tabs, function(tab) {
    if (!is.null(tab$type) && tab$type == "actions") {
      bslib::nav_panel(
        title = tab$title,
        tagList(
          if (!is.null(tab$description)) p(tab$description),
          # Use the same button + progress bar layout as the standard actions card
          if (length(tab$action_buttons) > 0) {
            mapply(
              function(btn, prog_bar, index) {
                if (!is.null(btn)) {
                  # Create container for button and progress bar
                  fluidRow(
                    class = "action-row",
                    style = "min-height: 60px; position: relative; margin-bottom: 10px;",
                    column(
                      class = "d-flex flex-column justify-content-center align-items-center",
                      width = 12,
                      # Action button
                      div(
                        style = "width: 100%; margin-bottom: 5px; display: flex; justify-content: center;",
                        btn
                      ),
                      # Progress bar container (initially hidden)
                      if (!is.null(prog_bar)) {
                        div(
                          id = if (!is.null(tab$ns)) tab$ns(paste0("progress_container_", index)) else paste0("progress_container_", index),
                          class = "progress-bar-hidden",
                          style = "width: 100%; margin-top: 5px;",
                          div(
                            class = "progress custom-progressbar-style",
                            prog_bar
                          )
                        )
                      }
                    )
                  )
                }
              }, tab$action_buttons,
              if (!is.null(tab$progress_bars)) tab$progress_bars else vector("list", length(tab$action_buttons)),
              seq_along(tab$action_buttons),
              SIMPLIFY = FALSE
            )
          }
        )
      )
    } else {
      bslib::nav_panel(
        title = tab$title,
        tab$content
      )
    }
  })

  # Use a standard info card with navs_pill inside the card body for full theming control
  bslib::card(
    style = final_style,
    bslib::card_header(
      style = final_header_style,
      class = "infocard",
      h1(title),
      phosphoricons::ph(icon, weight = "bold")
    ),
    bslib::card_body(
      bslib::navset_card_pill(
        id = paste0(ns("pilltabs")),
        !!!tab_panels,
        ...
      )
    )
  )
}



create_status_line <- function(ns) {
  #' Create a standardized status line component
  #'
  #' This function creates a consistent status line component that looks and operates
  #' the same across all modules in the application.
  #'
  #' @param ns The namespace function for the current module
  #'
  #' @return A div element containing the standardized status line component
  #'
  #' @examples
  #' # Basic usage - will create a status line with id "status_line"
  #' create_status_line(ns)
  #'
  #' # In your server function, create the corresponding output:
  #' # output$status_line <- renderUI({ ... })

  div(
    class = "status-line",
    style = "width: 100%",
    column(
      width = 3,
      div(
        "Status",
        phosphoricons::ph("terminal", weight = "bold"),
        style = "font-weight = 'bold'; color: var(--bs-info) !important;",
        class = "d-flex justify-content-evenly align-items-center"
      )
    ),
    column(
      width = 9,
      style = "padding-left: 0.5em; padding-right: 2em;",
      div(
        uiOutput(ns("status_line")),
        style = "font-size: 1em; text-align: left; position: relative; min-width: 300px;",
        class = "d-flex justify-content-start align-items-center gap-3"
      )
    )
  )
}

create_status_output <- function(ns, status_state, session = NULL) {
  #' Create a standardized status output renderUI function
  #'
  #' This function creates a consistent status output that can be used across
  #' different modules to display status information with consistent styling.
  #'
  #' @param ns The namespace function for the current module
  #' @param status_state A reactive expression or value containing the current status
  #' @param session The session object (optional, for toast animations)
  #'
  #' @return A renderUI function for the status output
  #'
  #' @examples
  #' # In your server function:
  #' output$status_line <- create_status_output(ns, current_status_state, session)

  function() {
    status <- if (is.reactive(status_state)) status_state() else status_state

    if (status == "no_details" || status == "not_started") {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #D3D3D3; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("empty", weight = "bold"),
        tags$span("Not Started")
      )
    } else if (status == "saved" || status == "complete") {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #3B6B35; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("floppy-disk", weight = "bold"),
        tags$span("Complete")
      )
    } else if (status == "editing" || status == "in_progress") {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #3F5E78; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("keyboard", weight = "bold"),
        tags$span("In Progress")
      )
    } else if (status == "error") {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #990000; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("x-circle", weight = "bold"),
        tags$span("Error")
      )
    } else {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #D3D3D3; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("question", weight = "bold"),
        tags$span("Unknown Status")
      )
    }
  }
}

# Usage Examples for create_status_line and create_status_output:
#
# 1. In your UI function, add the status line:
#    create_status_line(ns)
#
# 2. In your server function, create the status output:
#    output$status_line <- create_status_output(ns, current_status_state, session)
#
# 3. The status system will automatically handle:
#    - "no_details" / "not_started" <U+2192> Gray "Not Started"
#    - "saved" / "complete" <U+2192> Green "Complete"
#    - "editing" / "in_progress" <U+2192> Blue "In Progress"
#    - "error" <U+2192> Red "Error"
#    - Unknown status <U+2192> Gray "Unknown Status"
#
# 4. Benefits:
#    - Consistent styling across all modules
#    - Standardized status messages
#    - Automatic toast animations
#    - Easy to maintain and update
#
# 5. Complete Example for a new module:
#    # In UI function:
#    mod_example_ui <- function(id) {
#      ns <- NS(id)
#      tagList(
#        fluidRow(
#          column(width = 8, h1("Example Module")),
#          column(width = 4, create_status_line(ns))
#        )
#      )
#    }
#
#    # In server function:
#    mod_example_server <- function(id) {
#      moduleServer(id, function(input, output, session) {
#        ns <- session$ns
#
#        # Track status state
#        current_status <- reactiveVal("not_started")
#
#        # Create status output
#        output$status_line <- create_status_output(ns, current_status, session)
#
#        # Update status based on actions
#        observeEvent(input$some_action, {
#          current_status("in_progress")
#          # ... do work ...
#          current_status("complete")
#        })
#      })
#    }
