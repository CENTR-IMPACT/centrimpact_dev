library(bslib)
library(thematic)
library(shiny)
library(shinyjs)
thematic::thematic_shiny()

# Define a custom theme for the application
custom_theme <- bslib::bs_theme(
  version = 5,
  base_font = bslib::font_google("Lato", wght = c(400, 600)),
  heading_font = bslib::font_google("Lora", wght = c(400, 700)),
  code_font = bslib::font_google("DM Mono", wght = c(300, 400, 600, 700)),
  bg = "#EDEAE2", # Main app background (lightest warm neutral)
  fg = "#2A2A2A", # Default text color (dark soft grey)

  primary = "#5C6470", 
  secondary = "#7A734D",
  info = "#5A6C75",
  success = "#5D7359", 
  warning = "#C5A259", 
  danger = "#8F524E", 

  "sidebar-bg" = "#5C5A58", # **Crucial: Darker, rich warm grey/brown for sidebar background**
  "sidebar-fg" = "#F5F1E8", # **Crucial: Creamy white for text/icons on dark sidebar**

  "navbar-bg" = "#5C5A58", # Rich brown (darkest, strong anchor)
  "navbar-fg" = "#F5F1E8", # Creamy white for navbar text/links

  "card-bg" = "#E2D8C3", # For accordions, cards (slightly darker than main bg)
  "accordion-bg" = "#E2D8C3", # Explicitly set accordion panel bg if different from card-bg
  "accordion-active-bg" = "#B8A989", # Darker highlight for active accordion header
  "input-bg" = "#FDF5E6", # A very light cream for input fields, stands out from card-bg
  "border-color" = "#C8BFA6" # A general border color for consistency
)


ui <- tagList(
  shinyjs::useShinyjs(),
  bslib::page_navbar(
    id = "main_navbar",
    theme = custom_theme,
    title = "CEnTR*IMPACT Toolkit",
    header = tags$head(
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Jersey+20&display=swap",
        rel = "stylesheet"
      ),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Share+Tech+Mono&display=swap",
        rel = "stylesheet"
      ),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Doto:wght@100..900&display=swap>",
        rel = "stylesheet"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://fontlibrary.org//face/fantasque-sans-mono",
        type = "text/css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://fontlibrary.org//face/grundschrift",
        type = "text/css"
      ),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "custom.css"
      ),
      tags$script(HTML("
  Shiny.addCustomMessageHandler('highlightWorkflowIcon', function(message) {
    // Validate message and required properties
    if (!message || typeof message !== 'object') {
      console.warn('Invalid message received for highlightWorkflowIcon');
      return;
    }

    // Find the workflow icon span by ID
    const iconSpan = document.getElementById(message.icon_id);
    if (iconSpan) {
      // Find the icon element (could be <i> or <svg>)
      let iconEl = iconSpan.querySelector('i, svg');
      if (iconEl) {
        // Apply color to the icon element
        iconEl.style.color = message.color;

        // If it's an SVG, also set the fill attribute
        if (iconEl.tagName.toLowerCase() === 'svg') {
          iconEl.style.fill = message.color;
        }
      } else {
        // Icon element not found, send warning to R
        Shiny.setInputValue('js_warning', {
          type: 'icon_not_found',
          element: message.icon_id || 'unknown',
          timestamp: Date.now()
        });
      }
    } else {
      // Icon span not found, send warning to R
      Shiny.setInputValue('js_warning', {
        type: 'workflow_icon_not_found',
        element: message.icon_id || 'unknown',
        timestamp: Date.now()
      });
    }
  });

"))
    ),
    sidebar = bslib::sidebar(
      position = "left",
      style = "background-color: #333333; color: #F5F1E8; height: 100%;",
      fluidRow(
        div(
          class = "d-flex align-items-center justify-content-center gap-2",
          ph("cards-three", weight = "light", class = "sidebar-icon"),
          "Status",
          #         style = "width: 85%; font-size: 1em !important; color: #4a4a4a; font-weight: light; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.2);
          #          border-top: solid 1px #d3d3d366; border-bottom: solid 1px #d3d3d366; padding: 0.25em; margin: 0.25em auto 0.25em auto !important;"
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            style = "height: 0.9em !important;",
            class = "d-flex align-items-center justify-content-center",
            uiOutput("project_title_text")
          )
        )
      ),
      fluidRow(
        div(
          style = "color: #D3D3D366; font-size: 0.8em;",
          class = "d-flex align-items-center justify-content-evenly gap-2",
          span(id = "workflow-setup", uiOutput("workflow_setup_icon")),
          span(id = "workflow-upload", uiOutput("workflow_upload_icon")),
          span(id = "workflow-clean", uiOutput("workflow_clean_icon")),
          span(id = "workflow-analyze", uiOutput("workflow_analyze_icon")),
          span(id = "workflow-visualize", uiOutput("workflow_visualize_icon")),
          span(id = "workflow-generate", uiOutput("workflow_generate_icon"))
        )
      ),
      fluidRow(
        div(
          class = "d-flex align-items-center justify-content-center gap-2 infocard",
          bsicons::bs_icon("clipboard2-data", class = "sidebar-icon"),
          "Scores",
          #          style = "width: 85%; font-size: 1em !important; color: #4a4a4a; font-weight: light; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.2);
          #          border-top: solid 1px #d3d3d366; border-bottom: solid 1px #d3d3d366; padding: 0.25em; margin: 1.25em auto 0.25em auto !important;"
        )
      ),
      fluidRow(
        div(
          class = "d-flex align-items-center justify-content-center gap-2 infocard",
          ph("toolbox", weight = "light", class = "sidebar-icon"),
          "Tools"
        )
      ),
      fluidRow(
        column(
          width = 10,
          actionButton(
            inputId = "data_entry_mode",
            class = "d-flex align-items-center justify-content-center gap-1 action-button action-clean",
            label = "Data Mode",
            style = "width: 100%; font-size: 1em;"
          )
        ),
        column(
          width = 2,
          class = "d-flex align-items-center justify-content-center",
          uiOutput("data_entry_indicator")
        )
      ),
      fluidRow(
        column(
          width = 10,
          actionButton(
            inputId = "save_snapshot",
            class = "d-flex align-items-center justify-content-center gap-1 action-button action-cascade",
            label = "Save Snapshot",
            style = "width: 100%; font-size: 1em;"
          )
        ),
        column(
          width = 2,
          class = "d-flex align-items-center justify-content-center",
          uiOutput("snapshot_indicator")
        )
      ),
    fluidRow(
      column(
        width = 10,
        actionButton(
          inputId = "load_snapshot",
          class = "d-flex align-items-center justify-content-center gap-1 action-button action-cascade",
          label = "Load Snapshot",
          style = "width: 100%; font-size: 1em;"
        )
      ),
      column(
        width = 2,
        class = "d-flex align-items-center justify-content-center",
        uiOutput("snapshot_load_indicator")
      )
    )
  ),
  
    # Navigation panels (comma-separated)
    bslib::nav_panel(
      title = "Home",
      icon = ph("map-pin-area", weight = "bold"),
      mod_home_ui("home_1"),
      class = "main_content"
    ),
    bslib::nav_panel(
      title = "Project Setup",
      icon = ph("toolbox", weight = "fill"),
      mod_setup_ui("setup_1"),
      class = "main_content"
    ),
    bslib::nav_panel(
      title = "Upload Data",
      icon = ph("pencil-ruler", weight = "fill"),
      mod_load_clean_ui("load_clean_1"),
      class = "main_content"
    ),
    # bslib::nav_panel(
    #   title = "Enter Data",
    #   icon = ph("pencil-ruler", weight = "fill"),
    #   mod_enter_data_ui("enter_data_1"),
    #   style = "margin: 0.5em 1.25em 1.25em 0.5em !important;",
    #   class = "neumorphic"
    # ),
    bslib::nav_panel(
      title = "Analyze Data",
      icon = ph("calculator", weight = "fill"),
      mod_analyze_ui("analyze_data_1"),
      style = "margin: 0.5em 1.25em 1.25em 0.5em !important;",
      class = "neumorphic"
    ),
    bslib::nav_panel(
      title = "Visualize Data",
      icon = ph("compass-tool", weight = "fill"),
      mod_visualize_ui("visualize_1"),
      style = "margin: 0.5em 1.25em 1.25em 0.5em !important;",
      class = "neumorphic"
    ),
    bslib::nav_panel(
      title = "Generate Report",
      icon = ph("pen-nib", weight = "fill"),
      mod_generate_ui("generate_1"),
      style = "margin: 0.5em 1.25em 1.25em 0.5em !important;",
      class = "neumorphic"
    ),
    # Right-aligned navigation items
    bslib::nav_spacer(),
    bslib::nav_menu(
      title = "About",
      icon = ph("info", weight = "bold"),
      align = "right",
      bslib::nav_item(
        style = "font-family: var(--bs-font-code); font-size: 1em; padding: 15px !important;",
        tags$p("Hello!")
      ),
      bslib::nav_item(
        style = "font-family: var(--bs-font-code); font-size: 0.75em; padding: 15px !important;",
        tags$a(
          shiny::icon("github"),
          "GitHub",
          href = "https://github.com/yourusername/centrimpactgolem",
          target = "_blank"
        )
      )
    )
  )
)
