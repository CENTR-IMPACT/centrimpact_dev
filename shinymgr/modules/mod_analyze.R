#!! ModName = mod_analyze
# !! ModDisplayName = Enter your module shiny display name here.
# !! ModDescription = Enter your module description here.
# !! ModCitation = Price, Jeremy F.  (2025). mod_analyze. [Source code].
# !! ModNotes = Enter your module notes here.
# !! ModActive = 1/0
# !! FunctionArg = argName1 !! argDescription !! argClass
# !! FunctionArg = argName2 !! argDescription !! argClass
# !! FunctionReturn = returnName1 !! returnDescription !! returnClass
# !! FunctionReturn = returnName2 !! returnDescription !! returnClass


# the ui function
mod_analyze_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      id = ns("analyze_tabs"), # Add an ID to the navset for switching
      bslib::nav_panel(
        "Overview",
        icon = icon("compass"),
        fluidRow(
          column(
            width = 8,
            h4(
              "Analyze Data",
              class = "analyze"
            ),
            p(
              "This module allows you to analyze your project data. Select the type of analysis you want to perform and run the analysis.",
              style = "font-family: var(--font-sans) !important; padding: 10px; margin-bottom: 20px;"
            )
          ),
          column(
            width = 4,
            img(
              src = "analyze.png",
              alt = "Analyze Data Image",
              style = "max-width: 100%; height: auto; margin-top: 20px; padding-left: 10%; padding-right: 10%;"
            )
          ),
        )
      ),
      bslib::nav_panel(
        "Analyze Data",
        icon = icon("toolbox"),
        fluidRow(
          column(
            width = 12,
            h4(
              tagList(
                "Run Full Analysis ",
                icon("people-arrows", style = "margin-left: 10px;"),
                icon("arrows-rotate", style = "margin-left: 10px;"),
                icon("tower-broadcast", style = "margin-left: 10px;")
              ),
              class = "analyze"
            ),
            hr(),
          )
        ),
        fluidRow(
          column(
            width = 7,
            style = "margin-bottom: 3em;",
            div(
              # style = "gap: 10px;",
              p("Load the main project data set containing all research team generated data, including:"),
              tags$ul(
                tags$li("Project indicators and metrics"),
                tags$li("Cascade effects analysis")
              ),
            ),
            div(
              style = "display: flex;",
              actionButton(ns("analyze_full"), "Run Full Analysis",
                class = "btn-analyze"
              )
            )
          ),
          column(
            width = 5,
            div(
              id = "full_analysis_console",
              style = "display: flex; color: #8ba086; background-color: #4A4A4A; gap: 10px; font-family: var(--font-mono) !important; width: 100%; height: 100%; margin-top: 15px; text-transform: uppercase; padding: 15px; border-radius: 5px; overflow-y: auto;",
              p("Analysis Progress:", style = "font-family: var(--font-mono) !important; color: #8ba086;"),
              p("Hello World!", style = "font-family: var(--font-mono) !important; color: #8ba086;"),
              p(
                id = ns("full_analysis_progress_text"),
                style = "margin: 0; padding: 0.25em; color: #8ba086; font-family: var(--font-mono) !important;"
              )
              # textOutput(ns("full_analysis_progress"), inline = TRUE)
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            h4(
              tagList(
                "Run Individual Analyses ",
                icon("people-arrows", style = "margin-left: 10px;"),
                icon("arrows-rotate", style = "margin-left: 10px;"),
                icon("tower-broadcast", style = "margin-left: 10px;")
              ),
              class = "analyze"
            ),
            hr()
          )
        ),
        fluidRow(
          column(
            width = 9,
            div(
              p("Load the alignment data set containing shared alignment assessments between:"),
              tags$ul(
                tags$li("Research team members"),
                tags$li("Community partners"),
                tags$li("Stakeholders")
              )
            )
          ),
          column(
            width = 3,
            div(
              style = "display: flex; width: 100%; margin-bottom: 1em;",
              actionButton(ns("analyze_alignment"), "Analyze Alignment",
                class = "btn-analyze", style = "width: 100%;"
              )
            ),
            div(
              style = "display: flex; width: 100%; margin-bottom: 1em;",
              actionButton(ns("analyze_dynamics"), "Analyze Project Dynamics",
                class = "btn-analyze", style = "width: 100%;"
              )
            ),
            div(
              style = "display: flex; width: 100%; margin-bottom: 1em;",
              actionButton(ns("analyze_cascade"), "Analyze Cascade Effects",
                class = "btn-analyze", style = "width: 100%;"
              )
            )
          )
        )
      ),
      # Data View Tabs
      bslib::nav_panel("Indicators",
        value = "indicators_panel", icon = icon("gauge-high"), # uiOutput(ns("indicators_ui")
      ),
      bslib::nav_panel("Alignment",
        value = "alignment_panel", icon = icon("people-arrows"), # uiOutput(ns("alignment_ui"
      ),
      bslib::nav_panel("Dynamics",
        value = "dynamics_panel", icon = icon("arrows-rotate"), # uiOutput(ns("dynamics_ui"
      ),
      bslib::nav_panel("Cascade Effects",
        value = "cascade_panel", icon = icon("tower-broadcast")
      )
    )
  )
}


# the server function
mod_analyze_server <- function(id, argName1, argName2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      reactiveValues(
        returnName1 = reactive(returnName1()),
        returnName2 = reactive(returnName2())
      )
    )
  })
}
