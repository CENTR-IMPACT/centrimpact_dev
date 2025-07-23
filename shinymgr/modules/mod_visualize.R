#!! ModName = mod_visualize
#!! ModDisplayName = Visualize Data
#!! ModDescription = Create visualizations for project data
#!! ModCitation = Price, Jeremy F. (2025). mod_visualize. [Source code].
#!! ModNotes = This module provides functionality to visualize project data.
#!! ModActive = 1
#!! FunctionArg = project_data !! Project data for visualization !! reactive

# Utilities are loaded in global.R

# Load required libraries
#' @importFrom dplyr ungroup filter
#' @importFrom tidyr pivot_longer

# Read color_palette from _brand.yml using yaml package
if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml")
library(yaml)
brand <- yaml::read_yaml("www/_brand.yml")
color_palette <- unname(unlist(brand$default$color_palette))

# Font setup for Google Fonts (Lato and IBM Plex Mono)
if (!requireNamespace("showtext", quietly = TRUE)) install.packages("showtext")
if (!requireNamespace("sysfonts", quietly = TRUE)) install.packages("sysfonts")
library(showtext)
library(sysfonts)
font_add_google("Lato", "lato")
font_add_google("DM Mono", "ibmplexmono")
showtext_auto()

# Function to create indicators plot
create_indicators_plots <- function(indicator_data, color_palette) {
  req(indicator_data)
  numeric_cols <- sapply(indicator_data, is.numeric)
  value_col <- names(indicator_data)[numeric_cols][1]
  if (is.na(value_col)) {
    stop("No numeric column found in indicators data for visualization")
  }
  non_numeric_cols <- !sapply(indicator_data, is.numeric)
  if (any(non_numeric_cols)) {
    indicator_col <- names(indicator_data)[non_numeric_cols][1]
    plot_data <- data.frame(
      indicator = as.character(indicator_data[[indicator_col]]),
      value = as.numeric(indicator_data[[value_col]]),
      stringsAsFactors = FALSE
    )
  } else {
    plot_data <- data.frame(
      indicator = rownames(indicator_data),
      value = as.numeric(indicator_data[[value_col]]),
      stringsAsFactors = FALSE
    )
  }
  base_plot <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(
      x = indicator,
      r = value,
      fill = indicator
    )
  ) +
    centrimpactvis::geom_indicators(
      show.legend = FALSE,
      show_reference_circles = TRUE,
      show_reference_lines = TRUE,
      scale_factor = 30
    ) +
    ggplot2::scale_fill_manual(values = color_palette)
  preview_plot <- base_plot +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )
  main_plot <- base_plot +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 14,
        family = "IBM Plex Mono"
      ),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  return(list(base = base_plot, preview = preview_plot, main = main_plot))
}

# Function to create alignment plots
create_alignment_plots <- function(alignment_data, color_palette) {
  if (is.null(alignment_data)) {
    stop("No alignment analysis results available")
  }
  median_frame_long <- alignment_data %>%
    tidyr::pivot_longer(
      cols = c("partner", "researcher", "overall"),
      names_to = "role",
      values_to = "value"
    )
  median_frame_long$role <- factor(median_frame_long$role,
    levels = c("partner", "researcher", "overall")
  )
  first_group <- "partner"
  last_group <- "overall"
  left_labels <- median_frame_long %>% dplyr::filter(role == first_group)
  right_labels <- median_frame_long %>% dplyr::filter(role == last_group)
  base_plot <- ggplot2::ggplot(
    data = median_frame_long,
    ggplot2::aes(x = role, y = value, group = alignment, color = alignment)
  ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color_palette)
  preview_plot <- base_plot +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )
  main_plot <- base_plot +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    ggrepel::geom_text_repel(
      data = left_labels,
      ggplot2::aes(label = alignment, x = role, y = value),
      family = "lato",
      fontface = "italic",
      size = 5,
      nudge_x = -0.5,
      direction = "y",
      hjust = 1,
      segment.size = 0.25,
      box.padding = 0.5
    ) +
    ggrepel::geom_text_repel(
      data = right_labels,
      ggplot2::aes(label = alignment, x = role, y = value),
      family = "lato",
      fontface = "italic",
      size = 5,
      nudge_x = 0.5,
      direction = "y",
      hjust = 0,
      segment.size = 0.25,
      box.padding = 0.5
    ) +
    ggplot2::geom_text(
      data = median_frame_long,
      ggplot2::aes(label = round(value, 2)),
      size = 4,
      nudge_y = 0.015,
      family = "ibmplexmono"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(30, 30, 30, 30),
      axis.text.y = ggplot2::element_text(
        size = 8,
        family = "ibmplexmono",
        face = "italic",
        color = "#4A4A4A"
      ),
      axis.text.x = ggplot2::element_text(
        size = 10,
        family = "ibmplexmono",
        face = "italic",
        color = "#4A4A4A"
      ),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.25, color = "#E0E0E0")
    )
  return(list(base = base_plot, preview = preview_plot, main = main_plot))
}


# (Optional) Function to create cascade plots - stub for now
create_cascade_plots <- function(cascade_data) {
  # Implement similar to above if cascade plot is needed
  base_plot <- ggplot2::ggplot() # placeholder
  preview_plot <- base_plot + ggplot2::theme_void()
  main_plot <- base_plot + ggplot2::theme_minimal()
  return(list(base = base_plot, preview = preview_plot, main = main_plot))
}


# the ui function
mod_visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Add JavaScript to handle the custom visualization message
    tags$script(HTML(sprintf("
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('runVisualization', function(message) {
          console.log('Received visualization request for:', message);
          // Trigger the corresponding Shiny input with proper namespace
          Shiny.setInputValue('%s-visualize_' + message, Math.random(), {priority: 'event'});
        });
      });
    ", ns("")))),
    bslib::accordion(
      id = ns("visualize_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "visualize_data",
        title = tagList(phosphoricons::ph("compass-tool"), HTML("&nbsp;"), "Visualize Data"),
        fluidRow(
          column(
            width = 12,
            p(
              "This module allows you to visualize your project data. Select the type of visualization you want to perform and run the visualization."
            ),
            tags$fieldset(
              class = "custom-fieldset",
              tags$legend(
                "Select",
                class = "custom-legend"
              ),
              shinyWidgets::radioGroupButtons(
                inputId = ns("visualization_type"),
                label = "VISUALIZATION TYPE",
                choices = list(
                  "All Metrics" = "full",
                  "Indicators" = "indicators",
                  "Alignment" = "alignment",
                  "Dynamics" = "dynamics",
                  "Cascade" = "cascade"
                ),
                selected = NULL,
                direction = "horizontal",
                justified = TRUE
              )
            ),
            tags$fieldset(
              class = "custom-fieldset",
              tags$legend(
                "Visualize",
                class = "custom-legend"
              ),
              div(
                class = "d-flex justify-content-center align-items-center",
                actionButton(
                  inputId = ns("run_visualization"),
                  label = tagList(
                    "Visualize Metrics ",
                    ph("compass-tool", weight = "bold")
                  ),
                  width = "50%",
                  class = "btn btn-primary btn-lg"
                )
              ),
            div(
                class = "d-flex justify-content-center",
                  id = ns("progress_container_visualize"),
                  style = "width: 50%; visibility: hidden;",
                  shinyWidgets::progressBar(
                    id = ns("progress_bar_visualize"),
                    value = 0,
                    total = 100,
                    status = "success",
                    title = "Ready to Visualize",
                    striped = FALSE,
                    display_pct = FALSE
                  )
                )
              )
            )
        )
      ),
      bslib::accordion_panel(
        value = "results",
        title = tagList(ph("flag-checkered", weight = "fill"), HTML("&nbsp;"), "Results"),
        uiOutput(ns("indicators_ui")),
        uiOutput(ns("alignment_ui")),
        uiOutput(ns("dynamics_ui")),
        uiOutput(ns("cascade_ui"))
      ),
      bslib::accordion_panel(
        value = "cascade_panel",
        title = tagList(phosphoricons::ph("waveform"), HTML("&nbsp;"), "Cascade Effects")
        # uiOutput(ns("cascade_ui"))
      )
    )
  )
}

# Add a helper function for the visualize section UI (mirroring score_section_ui from mod_analyze)
visualize_section_ui <- function(
  plot_id, section_name, bgcolor, tooltip_text, ns, overlay_input_id
) {
  # Use a single column layout with vertical stacking
  div(
    style = "display: flex; flex-direction: column; align-items: stretch; gap: 1.5rem; width: 100% !important;",
    # Value box with preview plot (square)
    bslib::value_box(
      style = "\n        padding: 0.25em !important;\n        border-radius: 10px 10px 0 0 !important;\n        border: 1px solid #d4c5b9 !important;\n        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06) !important;\n        margin-bottom: 0;\n        ",
      title = bslib::tooltip(
        tags$div(
          section_name,
          class = "text-uppercase fw-semibold",
          style = "font-size: 0.9rem;"
        ),
        tooltip_text
      ),
      value = div(
        style = "height: 120px; width: 120px; margin: 0 auto; display: flex; align-items: center; justify-content: center; overflow: hidden;",
        plotOutput(ns(paste0(plot_id, "_preview")), height = "120px", width = "120px")
      ),
      showcase = NULL,
      showcase_layout = "top right",
      theme = value_box_theme(bg = bgcolor, fg = "#f5f1e8")
    ),
    # Card below value box
    bslib::card(
      style = "\n        padding:0 !important;\n        border-radius:0 0 8px 8px !important;\n        border: 1px solid #d4c5b9 !important;\n        box-shadow: 0 2px 4px rgba(0,0,0,0.08) !important;\n        background: #f9f5f0;\n        margin-top: 0;\n        ",
      bslib::card_header(
        style = "background-color: #f9f5f0; border-bottom: 1px solid #e8ddd4; padding: 0.5rem 1rem;",
        tags$div(
          class = "d-flex align-items-center justify-content-between mb-0",
          tags$div("Status",
            class = "fw-medium",
            style = "font-size:0.75rem;color: var(--bs-success);text-transform:uppercase;"
          ),
          tags$div("READY FOR REPORT GENERATION",
            class = "fw-medium",
            style = "font-size:1.1rem;color: var(--bs-success);text-transform:uppercase;font-family:var(--bs-font-monospace);"
          ),
          phosphoricons::ph("check-circle", weight = "fill", size = "3x", style = "color: var(--bs-success);")
        )
      ),
      bslib::card_body(
        # Blank body
      ),
      bslib::card_footer(
        style = "\n          background: var(--bs-body-bg);\n          border-top:1px solid #e8ddd4;\n          padding:0.5rem 1rem; text-align:center;",
        actionButton(
          inputId = ns(overlay_input_id),
          label = tagList(
            phosphoricons::ph("table", weight = "fill", style = "margin-right:0.45rem;"),
            "VIEW VISUALIZED DATA"
          ),
          class = "text-uppercase fw-semibold",
          style = paste0("background-color: #8A7A8F; color: var(--bs-body-bg); border: none;\n                  padding:0.44rem 1.1rem;font-size:0.7rem;letter-spacing:0.4px;border-radius:6px;")
        )
      )
    )
  )
}

# Add a helper for a full metric section with fieldset, legend, value box (with icon and tooltip), and right card
visualize_metric_section_ui <- function(
  plot_id, section_name, legend_label, bgcolor, tooltip_text, ns, overlay_input_id, icon_choice
) {
  # Define color mapping based on section_name
  section_colors <- list(
    "indicators" = "#3B6B35",  # Forest green
    "alignment" = "#3F5E78",   # Denim blue
    "dynamics" = "#4E342E",    # Brown
    "cascade" = "#990000"      # Deep red
  )
  
  # Get the appropriate color for this section
  section_color <- section_colors[[tolower(gsub(" ", "_", section_name))]]
  if (is.null(section_color)) section_color <- "#4A4A4A"  # Default to stone gray
  
  tags$fieldset(
    class = "custom-fieldset",
    style = "margin-bottom: 2rem;",
    tags$legend(class = "custom-legend", legend_label),
    layout_columns(
      col_widths = c(5, 7), 
      gap = 0,
      # LEFT: Value box with preview plot and icon
      bslib::value_box(
        style = "
          padding: 0.5em !important;
          border-radius: 10px 0 0 10px !important;
          border: 1px 0 1px 1px !important;
          border-style: solid !important;
          border-color: #d4c5b9 !important;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06) !important;
          max-height: 28vh !important;
          overflow: hidden !important;
          display: flex !important;
          flex-direction: column !important;
          justify-content: space-between !important;
        ",
        title = tags$div(
          section_name,
          class = "text-uppercase fw-semibold",
          style = "font-size: 0.9rem; color: #f5f1e8;"
        ),
        value = plotOutput(ns(paste0(plot_id, "_preview")), height = "90px"),
        showcase = icon_choice,
        showcase_layout = "top right",
        theme = value_box_theme(bg = section_color, fg = "#f5f1e8")
      ),
      # RIGHT: Card with header, body, and footer with action buttons
      bslib::card(
        style = "
          padding: 0 !important;
          border-radius: 0 8px 8px 0 !important;
          width: 100% !important;
          border: 1px 1px 1px 0; 
          border-style: solid; 
          border-color: #d4c5b9 !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08) !important;
          background: #f9f5f0;
          display: flex;
          flex-direction: column;
        ",
        bslib::card_header(
          style = "background-color: #f9f5f0; border-bottom: 1px solid #e8ddd4; padding: 0.5rem 1rem;",
          tags$div(
            class = "d-flex align-items-center justify-content-between mb-0",
            tags$div("READY FOR REPORT GENERATION",
                     class = "fw-medium",
                     style = "font-size:1.1rem;color: var(--bs-success);text-transform:uppercase;font-family:var(--bs-font-monospace);"
            ),
            phosphoricons::ph("check-circle", weight = "fill", size = "3x", style = "color: var(--bs-success);")
          )
        ),
        bslib::card_body(
          tooltip_text
        ),
        bslib::card_footer(
          style = "\n          background: var(--bs-body-bg);\n          border-top:1px solid #e8ddd4;\n          padding:0.5rem 1rem;\n          display: flex;\n          justify-content: space-between;\n          align-items: center;",
          div(
            class = "btn-group",
            downloadButton(
              ns(paste0(plot_id, "_download")),
              label = "Download",
              icon = icon("download"),
              class = "btn-sm btn-outline-primary"
            )
          ),
          actionButton(
            inputId = ns(overlay_input_id),
            label = "Expand",
            icon = icon("expand"),
            class = "btn-sm btn-outline-secondary"
          )
        )
      )
    )
  )
}

# the server function
mod_visualize_server <- function(id, project_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create workflow observers
    create_workflow_observers(project_data, ns, session)

    # Initialize individual reactive values (do NOT create rv_analysis here)
    visualization_type <- reactiveVal(NULL)
    visualization_message <- reactiveVal(NULL)
    current_visualization <- reactiveVal(NULL)
    indicators_visualized <- reactiveVal(FALSE)
    alignment_visualized <- reactiveVal(FALSE)
    dynamics_visualized <- reactiveVal(FALSE)
    cascade_visualized <- reactiveVal(FALSE)
    full_visualized <- reactiveVal(FALSE)

    # DRY helper for plot/UI rendering
    render_visualization <- function(output, ns, plot_id, plot_func, data, legend = NULL, preview_func = NULL) {
      # Always create the preview plot (for all visualizations)
      if (plot_id == "indicators_plot") {
        output[[paste0(plot_id, "_preview")]] <- renderPlot({
          plots <- create_indicators_plots(data, color_palette)
          plots$preview
        }, height = 70, width = "auto")
        # Render the main plot for the overlay
        output[[paste0(plot_id, "_main_overlay")]] <- renderPlot({
          plots <- create_indicators_plots(data, color_palette)
          plots$main
        }, height = 600, width = 600)
      } else {
        output[[paste0(plot_id, "_preview")]] <- renderPlot({
          if (!is.null(preview_func)) {
            preview_func(data)
          } else {
            plot_func(data)
          }
        }, height = 70, width = "auto")
        # Render the main plot for the overlay
        output[[paste0(plot_id, "_main_overlay")]] <- renderPlot({
          plot_func(data)
        }, height = 600, width = 600)
      }

      # Use a reactiveVal to track overlay state per plot_id
      overlay_active <- reactiveVal(FALSE)

      observeEvent(input[[paste0(plot_id, "_show_overlay")]], {
        overlay_active(TRUE)
      })
      observeEvent(input[[paste0(plot_id, "_close_overlay")]], {
        overlay_active(FALSE)
      })

      output[[paste0(plot_id, "_overlay_ui")]] <- renderUI({
        if (isTRUE(overlay_active())) {
          tagList(
            div(class = "overlay-bg", onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns(paste0(plot_id, "_close_overlay")))),
            div(
              class = "overlay-card", style = "background: #edeae2;",
              tags$h4(legend),
              plotOutput(ns(paste0(plot_id, "_main_overlay")), height = "70vh"),
              actionButton(ns(paste0(plot_id, "_close_overlay")), "Close", class = "btn-secondary float-end")
            )
          )
        }
      })

      # Add download handler for the main plot
      output[[paste0(plot_id, "_download")]] <- downloadHandler(
        filename = function() {
          paste0(plot_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
        },
        content = function(file) {
          # Get the plot function and generate the plot
          if (plot_id == "indicators_plot") {
            p <- create_indicators_plots(data, color_palette)$main
          } else if (plot_id == "alignment_plot_main") {
            p <- create_alignment_plots(data, color_palette)$main
          } else if (plot_id == "dynamics_plot") {
            p <- create_dynamics_plot(domain_scores = data, color_palette = color_palette)$main
          } else if (plot_id == "cascade_plot") {
            p <- create_cascade_plot(data, color_palette)$main
          } else {
            p <- plot_func(data)
          }
          
          # Save the plot with high resolution
          ggsave(
            file,
            plot = p,
            device = "png",
            width = 10,
            height = 8,
            units = "in",
            dpi = 300,
            bg = "white"
          )
        }
      )
      
      output[[paste0(plot_id, "_ui")]] <- renderUI({
        tagList(
          tags$fieldset(
            class = "custom-fieldset",
            if (!is.null(legend)) tags$legend(class = "custom-legend", legend),
            fluidRow(
              column(
                width = 3,
                div(
                  style = "display: flex; flex-direction: column; height: 20vh;",
                  bslib::card(
                    style = "background: #edeae2; flex-grow: 1; min-height: 10vh; margin-bottom: 0.5em; padding: 0;",
                    tags$div(
                      plotOutput(
                        ns(paste0(plot_id, "_preview")),
                        width = "100%",
                        height = "100%"
                      ),
                      style = "display: flex; height: 100%; width: auto;"
                    )
                  ),
                  actionButton(
                    inputId = ns(paste0(plot_id, "_show_overlay")),
                    label = "Expand Plot",
                    class = "btn-primary mt-2",
                    style = "flex-shrink: 0;"
                  )
                )
              ),
              column(
                width = 9,
                div(
                  style = "height: 100%; display: flex; flex-direction: column;",
                  bslib::card(
                    style = "height: 100%;",
                    bslib::card_body(
                      tagList(
                        if (plot_id == "indicators_plot") {
                          HTML("<div style='font-size: 1.2em;'>Shows raw counts of key project elements—like partners, hours, outputs, and participants—to quickly convey project scale and activity, useful for funders and administrators.</div>")
                        } else if (plot_id == "alignment_plot_main") {
                          HTML("<div style='font-size: 1.2em;'>Compares how researchers and partners perceive alignment in values, goals, and roles. High alignment means strong collaboration; gaps highlight areas for improvement.</div>")
                        } else {
                          "[Filler text: Add summary, stats, or controls here]"
                        },
                        uiOutput(ns(paste0(plot_id, "_overlay_ui")))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      })
    }

    # Helper function to handle progress updates (moved inside server)
    update_visualization_progress <- function(session, ns, progress, message, type = "info") {
      visualization_type(type)
      visualization_message(message)
      shinyWidgets::updateProgressBar(
        session = session,
        id = ns("progress_bar_visualize"),
        value = progress,
        status = "success",
        title = if (!is.null(message)) message else NULL
      )
    }

    # --- Observe per-metric workflow state for Alignment Clean Data completion and update status/icon ---
    observe({
      wf <- project_data$workflow$alignment
      if (!is.null(wf) && wf$stage == "Clean Data" && wf$status == "complete") {
        update_alignment_status_display("cleaned", session, ns, project_data)
        update_alignment_workflow_icons(project_data, session)
      }
    })

    # Helper to update the single progress bar
    update_progress <- function(value, text = NULL) {
      shinyWidgets::updateProgressBar(
        session = session,
        id = ns("progress_bar_visualize"),
        value = value,
        status = "success",
        title = if (!is.null(text)) text else NULL
      )
    }

    # Create a local environment for helper functions
    helpers <- new.env()

    # Helper function to update visualization status
    helpers$update_visualization_status <- function(type, message, visualized = NULL) {
      visualization_type(type)
      visualization_message(message)
      if (!is.null(visualized)) {
        alignment_visualized(visualized)
      }
    }

    # Helper function to show/hide progress UI
    helpers$toggle_progress_ui <- function(show = TRUE) {
      visibility <- if (show) "visible" else "hidden"
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').style.visibility = '%s';",
        ns("progress_container_visualize"),
        visibility
      ))
      if (!show) update_progress(0)
    }

    # Main function to handle alignment visualization
    helpers$visualize_alignment_data <- function() {
      # Show progress UI with a small delay for smooth transition
      helpers$toggle_progress_ui(TRUE)
      helpers$update_visualization_status("info", "Visualization in progress...")

      # Simulate processing steps
      update_progress(30, "Preparing alignment visualization...")
      Sys.sleep(0.5)
      update_progress(70, "Generating alignment plots...")
      Sys.sleep(0.5)

      # Update workflow status
      update_workflow_step(
        project_data,
        stage = "Visualize Findings",
        status = "complete",
        session = session
      )

      # Finalize
      update_progress(100, "Finalizing...")
      helpers$update_visualization_status("success", "Alignment visualization completed successfully!", TRUE)
      cat("Alignment visualization completed\n")
    }

    # Observer for alignment visualization button
    observeEvent(input$visualize_alignment, {
      cat("Visualize Alignment button clicked\n")

      tryCatch(
        {
          # Show progress UI with a small delay
          shinyjs::delay(100, helpers$toggle_progress_ui(TRUE))

          # Run the visualization
          helpers$visualize_alignment_data()

          # Hide progress UI
          helpers$toggle_progress_ui(FALSE)
        },
        error = function(e) {
          error_msg <- conditionMessage(e)
          cat("ERROR in alignment visualization: ", error_msg, "\n", sep = "")
          helpers$update_visualization_status("danger", paste("Error in alignment visualization:", error_msg))
          helpers$toggle_progress_ui(FALSE)
        }
      )
    })

    # Handle Run Visualization button click
    observeEvent(input$run_visualization, {
      tryCatch(
        {
          cat("Run Visualization button clicked\n")
          
          # Show progress bar
          shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", 
                               ns("progress_container_visualize")))

          # Get the selected visualization type
          vis_type <- input$visualization_type
          cat("Selected visualization type: ", vis_type, "\n", sep = "")

          if (is.null(vis_type) || vis_type == "") {
            cat("WARNING: No visualization type selected\n")
            visualization_type("warning")
            visualization_message("Please select a visualization type first")
            # Hide progress bar if no type selected
            shinyjs::delay(1000, {
              shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", 
                                   ns("progress_container_visualize")))
            })
            return()
          }

          # Store the current type for error handling
          current_visualization(vis_type)

          # Handle visualization based on type
          update_visualization_progress(session, ns, 30, paste("Preparing", vis_type, "visualization..."), "info")

          # Handle each visualization type
          if (vis_type == "indicators") {
            cat("Running indicators visualization\n")
            render_visualization(
              output, ns,
              plot_id = "indicators_plot",
              plot_func = function(data) create_indicators_plots(data, color_palette)$main,
              data = project_data$cleaned_data$indicators,
              legend = "Project Indicators",
              preview_func = function(data) create_indicators_plots(data, color_palette)$preview
            )
            indicators_visualized(TRUE)
          } else if (vis_type == "alignment") {
            cat("Running alignment visualization\n")

            # Check if analysis has been performed
            if (is.null(rv_analysis$alignment_medians)) {
              stop("No alignment analysis results available. Please run alignment analysis first.")
            }

            render_visualization(
              output, ns,
              plot_id = "alignment_plot_main",
              plot_func = function(data) create_alignment_plots(data, color_palette)$main,
              data = rv_analysis$alignment_medians,
              legend = "Project Alignment",
              preview_func = function(data) create_alignment_plots(data, color_palette)$preview
            )
            alignment_visualized(TRUE)

            # Update workflow status
            update_workflow_step(
              project_data,
              stage = "Visualize Findings",
              status = "complete",
              session = session
            )

            # Show success message
            update_visualization_progress(
              session,
              ns,
              100,
              "Alignment visualization completed successfully!",
              "success"
            )
          } else if (vis_type == "dynamics") {
            cat("Running dynamics visualization\n")

            # Check if analysis has been performed
            if (is.null(rv_analysis$dynamics_results)) {
              stop("No dynamics analysis results available. Please run dynamics analysis first.")
            }

            render_visualization(
              output, ns,
              plot_id = "dynamics_plot",
              plot_func = function(data) create_dynamics_plot(
                domain_scores = data,
                dimension_scores = rv_analysis$dimension_scores,
                color_palette = color_palette
              )$main,
              data = rv_analysis$dynamics_results,
              legend = "Project Dynamics",
              preview_func = function(data) create_dynamics_plot(domain_scores = data, color_palette = color_palette)$preview
            )
            dynamics_visualized(TRUE)

            # Update workflow status
            update_workflow_step(
              project_data,
              stage = "Visualize Findings",
              status = "complete",
              session = session
            )

            # Show success message
            update_visualization_progress(
              session,
              ns,
              100,
              "Dynamics visualization completed successfully!",
              "success"
            )
          } else if (vis_type == "cascade") {
            cat("Running cascade visualization\n")
            update_visualization_progress(session, ns, 50, "Preparing cascade visualization...", "info")

            # Check if analysis has been performed
            if (is.null(rv_analysis$cascade_results)) {
              stop("No cascade analysis results available. Please run cascade analysis first.")
            }

            # Render the cascade plot using the new create_cascade_plot
            render_visualization(
              output, ns,
              plot_id = "cascade_plot",
              plot_func = function(data) create_cascade_plot(data, color_palette)$main,
              data = rv_analysis$cascade_results,
              legend = "Cascade Effects",
              preview_func = function(data) create_cascade_plot(data, color_palette)$preview
            )
            cascade_visualized(TRUE)

            # Update workflow status and show success message
            update_workflow_step(
              project_data,
              stage = "Visualize Findings",
              status = "complete",
              session = session
            )

            update_visualization_progress(
              session,
              ns,
              100,
              "Cascade visualization completed successfully!",
              "success"
            )
          } else if (vis_type == "full") {
            cat("Running full visualization (sequential)...\n")
            update_visualization_progress(session, ns, 10, "Preparing full visualization...", "info")
            Sys.sleep(0.2)
            
            # INDICATORS
            cat("[Full] Running indicators visualization\n")
            render_visualization(
              output, ns,
              plot_id = "indicators_plot",
              plot_func = function(data) create_indicators_plots(data, color_palette)$main,
              data = project_data$cleaned_data$indicators,
              legend = "Project Indicators",
              preview_func = function(data) create_indicators_plots(data, color_palette)$preview
            )
            indicators_visualized(TRUE)
            update_visualization_progress(session, ns, 30, "Indicators visualization completed.", "info")
            Sys.sleep(0.2)

            # ALIGNMENT
            cat("[Full] Running alignment visualization\n")
            if (is.null(rv_analysis$alignment_medians)) {
              stop("No alignment analysis results available. Please run alignment analysis first.")
            }
            render_visualization(
              output, ns,
              plot_id = "alignment_plot_main",
              plot_func = function(data) create_alignment_plots(data, color_palette)$main,
              data = rv_analysis$alignment_medians,
              legend = "Project Alignment",
              preview_func = function(data) create_alignment_plots(data, color_palette)$preview
            )
            alignment_visualized(TRUE)
            update_visualization_progress(session, ns, 50, "Alignment visualization completed.", "info")
            Sys.sleep(0.2)

            # DYNAMICS
            cat("[Full] Running dynamics visualization\n")
            if (is.null(rv_analysis$dynamics_results)) {
              stop("No dynamics analysis results available. Please run dynamics analysis first.")
            }
            render_visualization(
              output, ns,
              plot_id = "dynamics_plot",
              plot_func = function(data) create_dynamics_plot(
                domain_scores = data,
                dimension_scores = rv_analysis$dimension_scores,
                color_palette = color_palette
              )$main,
              data = rv_analysis$dynamics_results,
              legend = "Project Dynamics",
              preview_func = function(data) create_dynamics_plot(domain_scores = data, color_palette = color_palette)$preview
            )
            dynamics_visualized(TRUE)
            update_visualization_progress(session, ns, 70, "Dynamics visualization completed.", "info")
            Sys.sleep(0.2)

            # CASCADE
            cat("[Full] Running cascade visualization\n")
            if (is.null(rv_analysis$cascade_results)) {
              stop("No cascade analysis results available. Please run cascade analysis first.")
            }
            render_visualization(
              output, ns,
              plot_id = "cascade_plot",
              plot_func = function(data) create_cascade_plot(data, color_palette)$main,
              data = rv_analysis$cascade_results,
              legend = "Cascade Effects",
              preview_func = function(data) create_cascade_plot(data, color_palette)$preview
            )
            cascade_visualized(TRUE)
            update_visualization_progress(session, ns, 90, "Cascade visualization completed.", "info")
            Sys.sleep(0.2)

            # Finalize
            update_workflow_step(
              project_data,
              stage = "Visualize Findings",
              status = "complete",
              session = session
            )
            update_visualization_progress(
              session,
              ns,
              100,
              "Full visualization completed successfully!",
              "success"
            )
            full_visualized(TRUE)
          } else {
            warning_msg <- paste("Visualization type not implemented yet:", vis_type)
            cat("WARNING: ", warning_msg, "\n", sep = "")
            visualization_type("warning")
            visualization_message(warning_msg)
          }

          # Hide progress bar after completion with a 1-second delay
          shinyjs::delay(1000, {
            shinyjs::runjs(sprintf(
              "document.getElementById('%s').style.visibility = 'hidden';",
              ns("progress_container_visualize")
            ))
          })
          update_progress(0)
        },
        error = function(e) {
          current_type <- current_visualization()
          current_type <- if (is.null(current_type) || current_type == "") "unknown" else current_type
          error_msg <- paste("Error in", current_type, "visualization:", conditionMessage(e))
          cat("ERROR: ", error_msg, "\n", sep = "")
          update_visualization_progress(session, ns, 0, error_msg, "danger")
          
          # Hide progress bar on error after 3 seconds
          shinyjs::delay(3000, {
            shinyjs::runjs(sprintf(
              "document.getElementById('%s').style.visibility = 'hidden';",
              ns("progress_container_visualize")
            ))
          })
          visualization_message(error_msg)
          update_progress(0)
        }
      )
    })

    # Handle visualization type changes
    observeEvent(input$visualization_type, {
      cat("Visualization type changed to: ", input$visualization_type, "\n", sep = "")
    })

    # Visualize Indicators Data
    observeEvent(input$visualize_indicators, {
      cat("Visualize Indicators button clicked\n")
      tryCatch(
        {
          visualization_type("info")
          visualization_message("Visualization in progress...")
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_visualization_container")))
          })
          update_progress(30, "Preparing indicators visualization...")
          Sys.sleep(0.5)
          update_progress(70, "Generating indicators plots...")

          # Generate the indicators plot
          output$indicators_plot <- renderPlot({
            cat("Starting indicators plot generation...\n")

            # Log the structure of the workflow data
            cat("\n=== Workflow Data Structure ===\n")
            str(project_data$cleaned_data$indicators)

            req(project_data$cleaned_data$indicators)
            cat("Indicators data found, proceeding with visualization\n")

            df <- project_data$cleaned_data$indicators
            cat("Data dimensions: ", nrow(df), " rows x ", ncol(df), " columns\n", sep = "")
            cat("Column names: ", paste(names(df), collapse = ", "), "\n", sep = "")

            # Find the first numeric column for values
            numeric_cols <- sapply(df, is.numeric)
            cat("Numeric columns found: ", paste(names(df)[numeric_cols], collapse = ", "), "\n", sep = "")

            value_col <- names(df)[numeric_cols][1]
            if (is.na(value_col)) {
              cat("ERROR: No numeric column found in indicators data\n")
              stop("No numeric column found in indicators data for visualization")
            }
            cat("Using column '", value_col, "' for values\n", sep = "")

            # Find the first non-numeric column for indicators
            non_numeric_cols <- !sapply(df, is.numeric)
            cat("Non-numeric columns: ", if (any(non_numeric_cols)) paste(names(df)[non_numeric_cols], collapse = ", ") else "None", "\n", sep = "")

            if (any(non_numeric_cols)) {
              indicator_col <- names(df)[non_numeric_cols][1]
              cat("Using column '", indicator_col, "' for indicators\n", sep = "")
              plot_data <- data.frame(
                indicator = as.character(df[[indicator_col]]),
                value = as.numeric(df[[value_col]]),
                stringsAsFactors = FALSE
              )
            } else {
              cat("No non-numeric columns found, using row names as indicators\n")
              plot_data <- data.frame(
                indicator = rownames(df),
                value = as.numeric(df[[value_col]]),
                stringsAsFactors = FALSE
              )
            }

            cat("\n=== Plot Data Structure ===\n")
            str(plot_data)

            # Create the plot
            cat("Creating ggplot object...\n")
            base_plot <- ggplot2::ggplot(
              data = plot_data,
              aes(
                x = indicator,
                r = value,
                fill = indicator
              )
            ) +
              centrimpactvis::geom_indicator(show_reference_circles = TRUE) +
              labs(
                title = "Indicator Visualization",
                x = "Indicator",
                y = "Value"
              )

            cat("Plot object created successfully\n")
            return(p)
          })

          indicators_visualized(TRUE)

          # Update the indicators UI to show the plot
          cat("Updating indicators UI to show the plot\n")
          output$indicators_ui <- renderUI({
            cat("Rendering plotOutput for indicators_plot\n")
            tryCatch(
              {
                tags$fieldset(
                  class = "custom-fieldset",
                  plotOutput(ns("indicators_plot"), height = "600px")
                )
              },
              error = function(e) {
                cat("ERROR creating plotOutput: ", conditionMessage(e), "\n", sep = "")
                div(
                  class = "alert alert-danger",
                  "Error creating plot: ", conditionMessage(e)
                )
              }
            )
          })

          update_workflow_step(
            project_data,
            stage = "Visualize Findings",
            status = "complete",
            session = session
          )
          update_progress(100, "Finalizing...")
          cat("Indicators visualization completed\n")
          visualization_type("success")
          visualization_message("Indicators visualization completed successfully!")
          shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_visualization_container")))
          update_progress(0)
        },
        error = function(e) {
          cat("ERROR in indicators visualization: ", conditionMessage(e), "\n", sep = "")
          visualization_type("danger")
          visualization_message("Error in indicators visualization")
          shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_visualization_container")))
          update_progress(0)
        }
      )
    })

    # Visualize Dynamics Data
    observeEvent(input$visualize_dynamics, {
      cat("Visualize Dynamics button clicked\n")
      tryCatch(
        {
          visualization_type("info")
          visualization_message("Visualization in progress...")
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_visualization_container")))
          })
          update_progress(30, "Preparing dynamics visualization...")
          Sys.sleep(0.5)
          update_progress(70, "Generating dynamics plots...")
          Sys.sleep(0.5)
          dynamics_visualized(TRUE)
          update_workflow_step(
            project_data,
            stage = "Visualize Findings",
            status = "complete",
            session = session
          )
          # update_main_data_workflow_icons("Visualize Findings", "complete", session, ns, ns_workflow)
          update_progress(100, "Finalizing...")
          cat("Dynamics visualization completed\n")
          visualization_type("success")
          visualization_message("Dynamics visualization completed successfully!")
          shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_visualization_container")))
          update_progress(0)
        },
        error = function(e) {
          cat("ERROR in dynamics visualization: ", conditionMessage(e), "\n", sep = "")
          visualization_type("danger")
          visualization_message("Error in dynamics visualization")
          shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_visualization_container")))
          update_progress(0)
        }
      )
    })

    # Visualize Cascade Data
    observeEvent(input$visualize_cascade, {
      cat("Visualize Cascade button clicked\n")
      tryCatch(
        {
          visualization_type("info")
          visualization_message("Visualization in progress...")
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_visualization_container")))
          })
          update_progress(30, "Preparing cascade visualization...")
          Sys.sleep(0.5)
          update_progress(70, "Generating cascade plots...")
          Sys.sleep(0.5)
          cascade_visualized(TRUE)
          update_workflow_step(
            project_data,
            stage = "Visualize Findings",
            status = "complete",
            session = session
          )
          # update_main_data_workflow_icons("Visualize Findings", "complete", session, ns, ns_workflow)
          update_progress(100, "Finalizing...")
          cat("Cascade visualization completed\n")
          visualization_type("success")
          visualization_message("Cascade visualization completed successfully!")
          shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_visualization_container")))
          update_progress(0)
        },
        error = function(e) {
          cat("ERROR in cascade visualization: ", conditionMessage(e), "\n", sep = "")
          visualization_type("danger")
          visualization_message("Error in cascade visualization")
          shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_visualization_container")))
          update_progress(0)
        }
      )
    })

    # Run Full Visualization
    observeEvent(input$visualize_full, {
      cat("Run Full Visualization button clicked\n")
      tryCatch(
        {
          visualization_type("info")
          visualization_message("Visualization in progress...")
          shinyjs::delay(100, {
            shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", ns("progress_visualization_container")))
          })
          update_progress(10, "Preparing full visualization...")
          Sys.sleep(0.3)
          update_progress(30, "Generating indicators plots...")
          Sys.sleep(0.3)
          indicators_visualized(TRUE)
          update_progress(50, "Generating alignment plots...")
          Sys.sleep(0.3)
          alignment_visualized(TRUE)
          update_progress(70, "Generating dynamics plots...")
          Sys.sleep(0.3)
          dynamics_visualized(TRUE)
          update_progress(90, "Generating cascade plots...")
          Sys.sleep(0.3)
          cascade_visualized(TRUE)
          update_progress(100, "Finalizing...")
          full_visualized(TRUE)
          update_workflow_step(
            project_data,
            stage = "Visualize Findings",
            status = "complete",
            metric = "alignment",
            session = session
          )
          update_workflow_step(
            project_data,
            stage = "Visualize Findings",
            status = "complete",
            metric = "indicators",
            session = session
          )
          update_workflow_step(
            project_data,
            stage = "Visualize Findings",
            status = "complete",
            metric = "dynamics",
            session = session
          )
          update_workflow_step(
            project_data,
            stage = "Visualize Findings",
            status = "complete",
            metric = "cascade",
            session = session
          )
          update_alignment_workflow_icons(project_data, session, ns)
          # update_main_data_workflow_icons("Visualize Findings", "complete", session, ns, ns_workflow)
          cat("Full visualization completed\n")
          visualization_type("success")
          visualization_message("Full visualization completed successfully!")
          shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_visualization_container")))
          update_progress(0)
        },
        error = function(e) {
          cat("ERROR in full visualization: ", conditionMessage(e), "\n", sep = "")
          visualization_type("danger")
          visualization_message("Error in full visualization")
          shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", ns("progress_visualization_container")))
          update_progress(0)
        }
      )
    })

    # Initialize workflow icons on module load with current state
    observe({
      # Workflow icons are now updated centrally by the main server observer
      cat("Workflow icons initialization skipped - handled centrally\n")
    })

    # Update UI based on visualization status
    output$visualization_status <- renderUI({
      type <- visualization_type()
      message <- visualization_message()
      if (!is.null(type) && !is.null(message)) {
        div(
          class = paste0("alert alert-", type),
          role = "alert",
          message
        )
      }
    })

    output$visualize_status_alert_ui <- NULL # Removed renderUI for visualize_status_alert_ui

    # Update UI rendering for indicators, alignment, and dynamics
    output$indicators_ui <- renderUI({
      if (!isTRUE(indicators_visualized())) {
        tags$div(
          class = "data-placeholder",
          tags$div(
            class = "d-flex align-items-center justify-content-center gap-2",
            phosphoricons::ph("warning-circle", weight = "bold", class = "warning-icon"),
            tags$div(
              tags$strong("No Indicators Visualization Results"),
              tags$br(),
              "Please generate the indicators plots first to view results."
            )
          )
        )
      } else {
        tagList(
          tags$fieldset(
            class = "custom-fieldset",
            tags$legend(class = "custom-legend", "INDICATORS"),
            fluidRow(
              column(
                width = 3,
                div(
                  style = "display: flex; flex-direction: column; height: 20vh;",
                  bslib::card(
                    style = "background: #edeae2; flex-grow: 1; min-height: 10vh; margin-bottom: 0.5em; padding: 0;",
                    tags$div(
                      plotOutput(ns("indicators_plot"), height = "600px")
                    )
                  ),
                  actionButton(
                    inputId = ns("indicators_plot_show_overlay"),
                    label = "Expand Plot",
                    class = "btn-primary mt-2",
                    style = "flex-shrink: 0;"
                  )
                )
              ),
              column(
                width = 9,
                div(
                  style = "height: 100%; display: flex; flex-direction: column;",
                  bslib::card(
                    style = "height: 100%;",
                    bslib::card_body(
                      tagList(
                        HTML("<div style='font-size: 1.2em;'>Shows raw counts of key project elements—like partners, hours, outputs, and participants—to quickly convey project scale and activity, useful for funders and administrators.</div>"),
                        uiOutput(ns("indicators_plot_overlay_ui"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    })
    output$alignment_ui <- renderUI({
      if (!isTRUE(alignment_visualized())) {
        tags$div(
          class = "data-placeholder",
          tags$div(
            class = "d-flex align-items-center justify-content-center gap-2",
            phosphoricons::ph("warning-circle", weight = "bold", class = "warning-icon"),
            tags$div(
              tags$strong("No Alignment Visualization Results"),
              tags$br(),
              "Please generate the alignment plots first to view results."
            )
          )
        )
      } else {
        tagList(
          tags$fieldset(
            class = "custom-fieldset",
            tags$legend(class = "custom-legend", "Alignment"),
            fluidRow(
              column(
                width = 3,
                div(
                  style = "display: flex; flex-direction: column; height: 20vh;",
                  bslib::card(
                    style = "background: #edeae2; flex-grow: 1; min-height: 10vh; margin-bottom: 0.5em; padding: 0;",
                    tags$div(
                      plotOutput(ns("alignment_plot_main"), height = "600px")
                    )
                  ),
                  actionButton(
                    inputId = ns("alignment_plot_main_show_overlay"),
                    label = "Expand Plot",
                    class = "btn-primary mt-2",
                    style = "flex-shrink: 0;"
                  )
                )
              ),
              column(
                width = 9,
                div(
                  style = "height: 100%; display: flex; flex-direction: column;",
                  bslib::card(
                    style = "height: 100%;",
                    bslib::card_body(
                      tagList(
                        HTML("<div style='font-size: 1.2em;'>Compares how researchers and partners perceive alignment in values, goals, and roles. High alignment means strong collaboration; gaps highlight areas for improvement.</div>"),
                        uiOutput(ns("alignment_plot_main_overlay_ui"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    })
    output$dynamics_ui <- renderUI({
      if (!isTRUE(dynamics_visualized())) {
        tags$div(
          class = "data-placeholder",
          tags$div(
            class = "d-flex align-items-center justify-content-center gap-2",
            phosphoricons::ph("warning-circle", weight = "bold", class = "warning-icon"),
            tags$div(
              tags$strong("No Dynamics Visualization Results"),
              tags$br(),
              "Please generate the dynamics plots first to view results."
            )
          )
        )
      } else {
        tagList(
          tags$fieldset(
            class = "custom-fieldset",
            tags$legend(class = "custom-legend", "Dynamics"),
            fluidRow(
              column(
                width = 3,
                div(
                  style = "display: flex; flex-direction: column; height: 20vh;",
                  bslib::card(
                    style = "background: #edeae2; flex-grow: 1; min-height: 10vh; margin-bottom: 0.5em; padding: 0;",
                    tags$div(
                      plotOutput(ns("dynamics_plot"), height = "600px", width = "600px")
                    )
                  ),
                  actionButton(
                    inputId = ns("dynamics_plot_show_overlay"),
                    label = "Expand Plot",
                    class = "btn-primary mt-2",
                    style = "flex-shrink: 0;"
                  )
                )
              ),
              column(
                width = 9,
                div(
                  style = "height: 100%; display: flex; flex-direction: column;",
                  bslib::card(
                    style = "height: 100%;",
                    bslib::card_body(
                      tagList(
                        HTML("<div style='font-size: 1.2em;'>Visualizes project dynamics across core domains, helping identify strengths and areas needing balance or integration.</div>"),
                        uiOutput(ns("dynamics_plot_overlay_ui"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    })


    # Cascade UI
    output$cascade_ui <- renderUI({
      if (!isTRUE(cascade_visualized())) {
        return(
          tags$div(
            class = "data-placeholder",
            tags$div(
              class = "d-flex align-items-center justify-content-center gap-2",
              phosphoricons::ph("warning-circle", weight = "bold", class = "warning-icon"),
              tags$div(
                tags$strong("No Cascade Visualization Results"),
                tags$br(),
                "Please generate the cascade plots first to view results."
              )
            )
          )
        )
      } else {
        tagList(
          tags$fieldset(
            class = "custom-fieldset",
            tags$legend(class = "custom-legend", "Cascade Effects"),
            fluidRow(
              column(
                width = 3,
                div(
                  style = "display: flex; flex-direction: column; height: 20vh;",
                  bslib::card(
                    style = "background: #edeae2; flex-grow: 1; min-height: 10vh; margin-bottom: 0.5em; padding: 0;",
                    tags$div(
                      plotOutput(ns("cascade_plot"), height = "600px")
                    )
                  ),
                  actionButton(
                    inputId = ns("cascade_plot_show_overlay"),
                    label = "Expand Plot",
                    class = "btn-primary mt-2",
                    style = "flex-shrink: 0;"
                  )
                )
              ),
              column(
                width = 9,
                div(
                  style = "height: 100%; display: flex; flex-direction: column;",
                  bslib::card(
                    style = "height: 100%;",
                    bslib::card_body(
                      tagList(
                        HTML("<div style='font-size: 1.2em;'>Shows how project effects cascade through network layers, revealing reach, flow, and equity across the community.</div>"),
                        uiOutput(ns("cascade_plot_overlay_ui"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    })

    # Always render the full dynamics plot overlay for the Results panel
    output$dynamics_plot_main_overlay <- renderPlot({
      message("==== [DEBUG] dimension_scores in mod_visualize before plotting ====")
      print(str(rv_analysis$dimension_scores))
      print(head(rv_analysis$dimension_scores))
      req(rv_analysis$dynamics_results, rv_analysis$dimension_scores)
      plots <- create_dynamics_plot(
        domain_scores = rv_analysis$dynamics_results,
        dimension_scores = rv_analysis$dimension_scores,
        color_palette = color_palette
      )
      plots$main
    }, height = 600)

    # Return a list of reactive values
    list(
      alignment_visualized = alignment_visualized,
      indicators_visualized = indicators_visualized,
      dynamics_visualized = dynamics_visualized,
      cascade_visualized = cascade_visualized,
      full_visualized = full_visualized
    )
  }) # Close moduleServer
} # Close module function
