library(bslib)

# Define a custom theme for the application
custom_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "bootstrap",
  base_font = bslib::font_google("Lato"),
  code_font = bslib::font_google("IBM Plex Mono"),
  heading_font = bslib::font_google("Lora"),
  enable_gradients = TRUE,
  navbar_bg = "#3F5E78",
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

doto_font <- bslib::font_google("Doto")

ui <- bslib::page_navbar(
  id = "main_navbar",
  theme = custom_theme,
  title = "CEnTR*IMPACT",
  header = tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Doto:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
    body {
      background-color: #f0e5d7 !important;
    }
    .workflow-icon i {
      color: #D3D3D3;
    }
    .project-info-text {
      color: #D3D3D3;
      text-transform: uppercase;
      font-weight: 700;
      text-align: center;
      font-family: var(--bs-font-sans), sans-serif;
      font-size: 0.75em;
    }
    .project-info-date{
      color: #D3D3D3;
      font-family: Doto !important;
      text-align: right;
      font-weight: 700;
      font-family: var(--bs-font-code);
      font-size: 1em;
    }")),
    tags$script(HTML("
  Shiny.addCustomMessageHandler('highlightWorkflowIcon', function(message) {
    // Update only the specified icon
    const spanEl = document.getElementById(message.icon_id);
    if (spanEl) {
      const iconEl = spanEl.querySelector('i');
      if (iconEl) {
        iconEl.style.color = message.color;
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
    position = "right",
    bslib::card(
      bslib::card_header(
        class = "d-flex align-items-center justify-content-center gap-2",
        icon("diagram-project"),
        "Project Info"
      ),
      card_body(
        div(
          class = "project-info-text",
          id = "pi_text",
          uiOutput(outputId = "project_info_text")
      ),
      div(
        class = "project-info-date",
        id = "pi_date",
        uiOutput(outputId = "project_info_date")
      )
    )
    ),
    bslib::card(
      bslib::card_header(
        div(
          class = "d-flex align-items-center justify-content-center gap-2",
          icon("list-check"),
          "Workflow Progress"
        )
      ),
      card_body(
        div(
          class = "d-flex justify-content-center gap-3 workflow-icons",
          span(
            class = "workflow-icon",
            icon("seedling"),
            id = "project_setup_icon"
          ),
          span(
            class = "workflow-icon",
            icon("clipboard-list"),
            id = "load_data_icon"
          ),
          span(
            class = "workflow-icon",
            icon("soap"),
            id = "clean_data_icon"
          ),
          span(
            class = "workflow-icon",
            icon("calculator"),
            id = "analyze_data_icon"
          ),
          span(
            class = "workflow-icon",
            icon("magnifying-glass-chart"),
            id = "visualize_data_icon"
          ),
          span(
            class = "workflow-icon",
            icon("file-lines"),
            id = "generate_report_icon"
          )
        ),
        div(
          shinyWidgets::progressBar(id = "pb_workflow", value = 16.667, status = "secondary", title = "Project Not Initiated"),
          style = "color: #D3D3D3; text-transform: uppercase; font-family: var(--bs-font-code); font-size: 0.75em;"
        )
      )
    ),
    bslib::card(
      bslib::card_header(
        div(
          class = "d-flex align-items-center justify-content-center gap-2",
          icon("chart-line"),
          "Project Scores"
        )
      ),
      card_body(
        fluidRow(
          style = "display: flex; align-items: center;",
          column(
            width = 2,
            icon("people-arrows"),
            style = "font-size: 1.5em; color: #D3D3D3;"
          ),
          column(
            width = 10,
            align = "right",
            "-.--",
            style = "font-family: Doto; font-size: 1.5 em; font-weight: 700; color: #D3D3D3;"
          )
        ),
        fluidRow(
          style = "display: flex; align-items: center;",
          column(
            width = 2,
            icon("bars-progress"),
            style = "font-size: 1.5em; color: #D3D3D3;"
          ),
          column(
            width = 10,
            align = "right",
            "-.--",
            style = "font-family: Doto; font-size: 1.5 em; font-weight: 700; color: #D3D3D3;"
          )
        ),
        fluidRow(
          style = "display: flex; align-items: center;",
          column(
            width = 2,
            icon("tower-broadcast"),
            style = "font-size: 1.5em; color: #D3D3D3;"
          ),
          column(
            width = 10,
            align = "right",
            "-.--",
            style = "font-family: Doto; font-size: 1.5 em; font-weight: 700; color: #D3D3D3;"
          )
        )
      )
    )
  ),

  # Navigation panels (comma-separated)
  bslib::nav_panel(
    title = "Home",
    icon = icon("house"),
    mod_home_ui("home_1")
  ),
  bslib::nav_panel(
    title = "Project Setup",
    icon = icon("folder-plus"),
    mod_setup_ui("setup_1")
  ),
  bslib::nav_panel(
    title = "Load Data",
    icon = icon("database")
    # mod_load_clean_data_ui("load_clean_data_1")
  ),
  bslib::nav_panel(
    title = "Analyze Data",
    icon = icon("chart-bar")
    # mod_analyze_data_ui("analyze_data_1")
  ),
  bslib::nav_panel(
    title = "Visualize Data",
    icon = icon("chart-line")
    # mod_visualize_data_ui("visualize_data_1")
  ),
  bslib::nav_panel(
    title = "Generate Report",
    icon = icon("file-lines")
    # mod_generate_report_ui("generate_report_1")
  ),
  footer = fluidRow(
    class = "text-center",
    style = "background-color: #f8f4ef; color: #8A7A8F; padding: 10px; font-size: 0.75em;",
    tags$p("Supported in part by viewers like you.")
  ),
  # Right-aligned navigation items
  # bslib::nav_spacer(),
  # bslib::nav_menu(
  #   title = "Links",
  #   align = "right",
  #   bslib::nav_item(
  #     tags$a(
  #       shiny::icon("github"),
  #       "GitHub",
  #       href = "https://github.com/yourusername/centrimpactgolem",
  #       target = "_blank"
  #     )
  #   )
  # )
)
