library(bslib)
library(thematic)
library(shiny)
library(shinyjs)
thematic::thematic_shiny()

# Define a custom theme for the application
custom_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "bootstrap",
  base_font = bslib::font_google("Lato", wght = c(400, 700)),
  code_font = bslib::font_google("IBM Plex Mono", wght = c(400, 700)),
  heading_font = bslib::font_google("Lora", wght = c(400, 700)),
  enable_gradients = TRUE,
  enable_shadows = TRUE,
  enable_transitions = TRUE,
  # navbar_bg = "transparent", # "#3F5E78",
  "sidebar-emphasis-bg" = "#d2bfa3",
  fg = "#4A4A4A",
  bg = "#f8f4ef",
  primary = "#3B6B35",
  secondary = "#d2bfa3", # "#3F5E78",
  success = "#4B7F52",
  info = "#3F51B5",
  warning = "#E18B4E",
  danger = "#990000",
  light = "#f0e5d7",
  dark = "#4A4A4A",

  # Dark mode settings
  bg_select = "#4A4A4A",
  fg_select = "#F2ECD7",

  # Sidebar-specific
  "sidebar-bg" = "#f0e5d7",
  "sidebar-fg" = "#4A4A4A"
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
        rel="stylesheet"
          ),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Doto:wght@100..900&display=swap>",
        rel = "stylesheet"
      ),
      tags$link(rel="stylesheet",
                href="https://fontlibrary.org//face/fantasque-sans-mono",
                type="text/css"),
      tags$link(rel="stylesheet",
                href="https://fontlibrary.org//face/grundschrift",
                type="text/css"),
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "custom.css"),
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
    footer = tags$footer(
      class = "d-flex align-items-center justify-content-start gap-2",
      style = "background-color: rgba(255, 255, 255, 0.1); 
      padding: 0.25em;
      backdrop-filter: blur(10px);
      box-shadow: 0 4px 8px #00000022;
      border-radius: 12px;
      font-family: 'Share Tech Mono', var(--bs-code-font) !important;
      font-size: 0.7em;
      text-transform: uppercase;
      color: #4A4A4A;",
      fluidRow(
        column(
          width = 3,
          class = "d-flex align-items-center justify-content-evenly gap-2",
          style = "border-right: solid 1px #4a4a4a44; padding-right: 2em; padding-left: 2em;",
      img(
        src = "centr-logo.png",
        class = "header-logo",
        style = "max-height: 5vh; width: auto; display: block;"
      ),
      div(
        "Powered by the CEnTR*IMPACT Libraries for R"
        ),
      icon("r-project", class = "fa-2x", style = "color: #276DC2;")
        ),
      column(
        width = 9,
        class = "d-flex align-items-center justify-content-center",
        style = "text-align: center;",
      div(
        "Funded in part by the Coaltion for Urban and Metropolitan Universities (CUMU), 
        the Collaboratory, and the Indiana University Office of the Vice President for Research."
      )
        )
      )
    ),
    sidebar = bslib::sidebar(
      position = "left",
      fluidRow(
        div(
          class = "d-flex align-items-center justify-content-center gap-2 infocard",
          ph("cards-three", weight = "light", class = "sidebar-icon"),
          "Status",
          style = "width: 85%; font-size: 1em !important; color: #4a4a4a; font-weight: light; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.2);
          border-top: solid 1px #d3d3d366; border-bottom: solid 1px #d3d3d366; padding: 0.25em; margin: 0.25em auto 0.25em auto !important;"
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
          style = "color: #D3D3D366;",
          class = "d-flex align-items-center justify-content-evenly gap-2",
          ph("plant", weight = "light", class = "sidebar-icon", id = "workflow-setup"),
          ph("upload", weight = "light", class = "sidebar-icon", id = "workflow-upload"),
          ph("broom", weight = "light", class = "sidebar-icon", id = "workflow-clean"),
          ph("calculator", weight = "light", class = "sidebar-icon", id = "workflow-analyze"),
          ph("blueprint", weight = "light", class = "sidebar-icon", id = "workflow-visualize"),
          ph("newspaper-clipping", weight = "light", class = "sidebar-icon", id = "workflow-generate")
        )
      ),
      fluidRow(
        div(
          class = "d-flex align-items-center justify-content-center gap-2 infocard",
          bsicons::bs_icon("clipboard2-data", class = "sidebar-icon"),
          "Scores",
          style = "width: 85%; font-size: 1em !important; color: #4a4a4a; font-weight: light; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.2);
          border-top: solid 1px #d3d3d366; border-bottom: solid 1px #d3d3d366; padding: 0.25em; margin: 1.25em auto 0.25em auto !important;"
        )
      ),
      fluidRow(
        div(
          class = "d-flex align-items-center justify-content-center gap-2 infocard",
          ph("toolbox", weight = "light", class = "sidebar-icon"),
          "Tools",
          style = "width: 85%; font-size: 1em !important; color: #4a4a4a; font-weight: light; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.2);
          border-top: solid 1px #d3d3d366; border-bottom: solid 1px #d3d3d366; padding: 0.25em; margin: 1.25em auto 0.25em auto !important;"
        )
      ),
      fluidRow(
        column(
          width = 10,
          actionButton(
            inputId = "overview_mode",
            class = "d-flex align-items-center justify-content-center gap-1 action-button action-dynamics",
            label = "Overview Mode",
            style = "width: 100%; font-size: 1em;"
          )
        ),
        column(
          width = 2,
          class = "d-flex align-items-center justify-content-center",
          uiOutput("overview_indicator")
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
      )
    ),

    # Navigation panels (comma-separated)
    bslib::nav_panel(
      title = "Home",
      icon = ph("map-pin-area", weight = "bold"),
      mod_home_ui("home_1"),
      style = "margin: 0.5em 1.25em 1.25em 0.5em !important;",
      class = "neumorphic"
    ),
    bslib::nav_panel(
      title = "Project Setup",
      icon = ph("toolbox", weight = "fill"),
      mod_setup_ui("setup_1"),
      style = "margin: 0.5em 1.25em 1.25em 0.5em !important;",
      class = "neumorphic"
    ),
    bslib::nav_panel(
      title = "Upload Data",
      icon = ph("pencil-ruler", weight = "fill"),
      mod_load_clean_ui("load_clean_1"),
      style = "margin: 0.5em 1.25em 1.25em 0.5em !important;",
      class = "neumorphic"
    ),
        bslib::nav_panel(
      title = "Enter Data",
      icon = ph("pencil-ruler", weight = "fill"),
      mod_enter_data_ui("enter_data_1"),
      style = "margin: 0.5em 1.25em 1.25em 0.5em !important;",
      class = "neumorphic"
    ),
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
