library(bslib)
library(thematic)
thematic::thematic_shiny()

# Define a custom theme for the application
custom_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "bootstrap",
  base_font = bslib::font_google("Lato"),
  code_font = bslib::font_google("IBM Plex Mono"),
  heading_font = bslib::font_google("Lora"),
  enable_gradients = TRUE,
  enable_shadows = TRUE,
  enable_transitions = TRUE,
  navbar_bg = "#3F5E78",
  "sidebar-emphasis-bg" = "#d2bfa3",
  fg = "#4A4A4A",
  bg = "#f8f4ef",
  primary = "#3B6B35",
  secondary = "#3F5E78",
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

jersey_font <- bslib::font_google("Jersey 20")

ui <- bslib::page_navbar(
  id = "main_navbar",
  theme = custom_theme,
  title = "CEnTR*IMPACT",
  header = tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Jersey+20&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
    body {
      background-color: #e5dac3 !important; /*#f0e5d7 !important*/
    }
    .workflow-icon i {
      color: #D3D3D3;
    }
    .textarea {
      font-family: var(--bs-font-code);
      font-size: 14px;
    }
    .project-info-text {
      color: #D3D3D3;
    }
    .no-score {
      color: #D3D3D3;
      font-family: 'Jersey 20';
    }
    .has-score {
      color: var(--bs-body-color);
      font-family: 'Jersey 20';
    }
    .score-icon {
      display: inline-block;
    }
    .no-score + .column .score-icon i {
      color: #D3D3D3;
    }
    .has-score + .column .score-icon i {
      color: var(--bs-body-color);
    }
      text-transform: uppercase;
      font-weight: 700;
      text-align: center;
      font-family: var(--bs-font-code), sans-serif;
      font-size: 0.75em;
    }
    .project-info-date{
      color: #D3D3D3;
      text-align: right;
      font-weight: 700;
      font-family: var(--bs-font-code);
      font-size: .75em;
    }
    font-size: 2rem;
/* Light mode color */
:root {
  --monochrome-icon-color: #6c757d;
}

/* Dark mode color */
[data-bs-theme='dark'] {
  --monochrome-icon-color: #adb5bd;  /* Lighter gray for dark mode */
}

.monochrome-toggle .pretty .state i {
  color: var(--monochrome-icon-color) !important;
  font-size: 1.5em !important;
  position: relative !important;
  top: 0.75em !important;
  display: inline-block !important;
  left: 50% !important;
  transform: translateX(-50%) !important;
}

.monochrome-toggle .pretty input:checked ~ .state i,
.monochrome-toggle .pretty input:not(:checked) ~ .state i {
  color: var(--monochrome-icon-color) !important;
  font-size: 1.25em !important;
  position: relative !important;
  top: 0.5em !important;
  left: 50% !important;
  transform: translateX(-50%) !important;
}

/* Remove all borders and backgrounds */
.monochrome-toggle .pretty.p-toggle .state label:after,
.monochrome-toggle .pretty input:checked ~ .state label:after {
  background: none !important;
  border: none !important;
  box-shadow: none !important;
}

/* Remove focus outline */
.monochrome-toggle .pretty input:focus ~ .state label:after {
  outline: none !important;
  box-shadow: none !important;
}
  }")),
    tags$script(HTML("
  Shiny.addCustomMessageHandler('highlightWorkflowIcon', function(message) {
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
      }
    }
  });
  
  // Update project info text color
  Shiny.addCustomMessageHandler('project-info-text', function(message) {
    const el = document.getElementById('pi_text');
    if (el) {
      el.style.color = message.color || '#D3D3D3';
    }
  });
  
  // Update project info date color
  Shiny.addCustomMessageHandler('project-info-date', function(message) {
    const el = document.getElementById('pi_date');
    if (el) {
      el.style.color = message.color || '#D3D3D3';
    }
  });
"))
  ),
  sidebar = bslib::sidebar(
    position = "left",
    img(
      src = "centr_logo.png",
      class = "header-logo",
      style = "display: block;"
    ),
    # Project Info Card
    bslib::card(
      bslib::card_header(
        class = "d-flex align-items-center justify-content-center gap-2 project-info-text",
        style = "font-family: var(--bs-font-code); font-size: 1em; font-weight: bold;",
        id = "pi_text",
        ph("tree-structure", weight = "bold"),
        uiOutput(outputId = "project_info_text")
        #"Project Info"
      ),
      card_body(
      fluidRow(
        column(
          width = 2,
          class = "d-flex flex-wrap justify-content-center",
          style = "text-align: center; font-family: var(--bs-font-code); font-size: 0.75em;",
          uiOutput("report_output_icons")
        ),
        fluidRow(
          style = "display: flex; align-items: center;",
          column(
            width = 2,
            uiOutput("alignment_icon"),
            style = "font-size: 1.5em;"
          ),
          column(
            width = 10,
            align = "right",
            uiOutput("alignment_score_display"),
            style = "font-size: 1.5em; font-weight: 700;"
          )
        ),
        fluidRow(
          style = "display: flex; align-items: center;",
          column(
            width = 2,
            uiOutput("dynamics_icon"),
            style = "font-size: 1.5em;"
          ),
          column(
            width = 10,
            align = "right",
            uiOutput("dynamics_score_display"),
            style = "font-size: 1.5em; font-weight: 700;"
          )
        ),
        fluidRow(
          style = "display: flex; align-items: center;",
          column(
            width = 2,
            uiOutput("cascade_icon"),
            style = "font-size: 1.5em;"
          ),
          column(
            width = 10,
            align = "right",
            uiOutput("cascade_score_display"),
            style = "font-size: 1.5em; font-weight: 700;"
          )
        )
      )
      ),
      bslib::card_footer(
        class = "d-flex align-items-center justify-content-between project-info-footer",
        span(
          class = "d-flex gap-1 align-items-center justify-content-between w-25",
          uiOutput("report_output_icons")
          ),
        span(
        class = "d-flex gap-1 align-items-center justify-content-end project-info-date w-75",
        style = "font-family: 'Jersey 20'; margin: 0px; font-size: 1.25em;",
        id = "pi_date",
        ph("calendar-dots", weight = "bold"),
        uiOutput(outputId = "project_info_date")
        )
      )
    ),
    bslib::card(
      bslib::card_header(
        class = "d-flex align-items-center justify-content-center gap-2",
        ph("toolbox", weight = "bold"),
        "Tools"
      ),
      bslib::card_body(
        fluidRow(
          column(
            width = 2,
            class = "d-flex align-items-center justify-content-center",
            uiOutput("theme_indicator")
          ),
          column(
            width = 10,
            actionButton(
              inputId = "toggle_theme",
              class = "d-flex align-items-center justify-content-center gap-1",
              label = "Set Dark Mode",
              style = "width: 100%; font-size: 1em;"
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            class = "d-flex align-items-center justify-content-center",
            uiOutput("overview_indicator")
          ),
          column(
            width = 10,
            actionButton(
              inputId = "overview_mode",
              class = "d-flex align-items-center justify-content-center gap-1",
              label = "Skip Overviews",
              style = "width: 100%; font-size: 1em;"
            )
          )
        )
        ),
        fluidRow(
          column(
            width = 2,
            class = "d-flex align-items-center justify-content-center",
            uiOutput("snapshot_indicator")
          ),
          column(
            width = 10,
            actionButton(
              inputId = "save_snapshot",
              class = "d-flex align-items-center justify-content-center gap-1",
              label = "Save Snapshot",
              style = "width: 100%; font-size: 1em;"
            )
          )
        )
      )
  ),

  # Navigation panels (comma-separated)
  bslib::nav_panel(
    title = "Home",
    icon = ph("lighthouse", weight = "bold"),
    mod_home_ui("home_1"),
  ),
  bslib::nav_panel(
    title = "Project Setup",
    icon = ph("plant", weight = "bold"),
    mod_setup_ui("setup_1")
  ),
  bslib::nav_panel(
    title = "Upload Data",
    icon = ph("upload", weight = "bold"),
    mod_load_clean_ui("load_clean_1")
  ),
  bslib::nav_panel(
    title = "Analyze Data",
    icon = ph("calculator", weight = "bold"),
    mod_analyze_ui("analyze_data_1")
  ),
  bslib::nav_panel(
    title = "Visualize Data",
    icon = ph("blueprint", weight = "bold"),
    mod_visualize_ui("visualize_1")
  ),
  bslib::nav_panel(
    title = "Generate Report",
    icon = ph("newspaper-clipping", weight = "bold"),
    mod_generate_ui("generate_1")
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
