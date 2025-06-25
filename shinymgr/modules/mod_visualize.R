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
    bslib::navset_card_tab(
      id = ns("visualize_tabs"), # Add an ID to the navset for switching
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
        h1(
          "Visualize Data",
          style = "border-bottom: 1px solid #4A4A4A;"
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
            )
          ),
          column(
            width = 5,
            fluidRow(
              style = "margin-right: 1em;",
              column(
                width = 12,
                # style = "margin-bottom: 1em;",
                bslib::card(
                  bslib::card_header(
                    style = "background-color: #d2bfa3;",
                    actionButton(ns("visualize_all"),
                      label = tagList(
                        phosphoricons::ph("blueprint", weight = "bold"),
                        "Visualize All Metrics"
                      ),
                      class = "btn-primary btn-lg ",
                      style = "width: 100%;"
                    )
                  )
                )
              ),
              style = "background-color: #f0e5d7; padding-top: 10px; border: solid #d2bfa3 1px;",
              class = "shadow rounded",
              bslib::card_body(
                fluidRow(
                  class = "align-items-center",
                  column(width = 1),
                  column(
                    width = 1,
                    uiOutput(ns("v_indicators_icon"))
                  ),
                  column(
                    width = 1,
                    span(
                      ph("gauge", weight = "bold")
                    )
                  ),
                  column(
                    width = 5,
                    style = "font-weight: bold; font-size: 1.2em;",
                    span("Indicators")
                  ),
                  column(
                    width = 4,
                    div(
                      class = "d-flex justify-content-end",
                      style = "width: 100%;",
                      actionButton(ns("visualize_indicators"),
                        label = tagList(
                          phosphoricons::ph("gauge"),
                          "Visualize"
                        ),
                        class = "btn-analyze"
                      )
                    )
                  )
                ),
                fluidRow(
                  class = "align-items-center",
                  column(width = 1),
                  column(
                    width = 1,
                    uiOutput(ns("v_alignment_icon"))
                  ),
                  column(
                    width = 1,
                    span(
                      ph("flower-lotus", weight = "bold")
                    )
                  ),
                  column(
                    width = 5,
                    style = "font-weight: bold; font-size: 1.2em;",
                    span("Alignment")
                  ),
                  column(
                    width = 4,
                    div(
                      class = "d-flex justify-content-end",
                      style = "width: 100%;",
                      actionButton(ns("visualize_alignment"),
                        label = tagList(
                          phosphoricons::ph("flower-lotus"),
                          "Visualize"
                        ),
                        class = "btn-analyze"
                      )
                    )
                  )
                ),
                fluidRow(
                  class = "align-items-center",
                  column(width = 1),
                  column(
                    width = 1,
                    uiOutput(ns("v_dynamics_icon"))
                  ),
                  column(
                    width = 1,
                    span(
                      ph("pulse", weight = "bold")
                    )
                  ),
                  column(
                    width = 5,
                    style = "font-weight: bold; font-size: 1.2em;",
                    span("Project Dynamics")
                  ),
                  column(
                    width = 4,
                    div(
                      class = "d-flex justify-content-end",
                      style = "width: 100%;",
                      actionButton(ns("visualize_dynamics"),
                        label = tagList(
                          phosphoricons::ph("pulse"),
                          "Visualize"
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  class = "align-items-center",
                  column(width = 1),
                  column(
                    width = 1,
                    uiOutput(ns("v_cascade_icon"))
                  ),
                  column(
                    width = 1,
                    span(
                      ph("waveform", weight = "bold")
                    )
                  ),
                  column(
                    width = 5,
                    style = "font-weight: bold; font-size: 1.2em;",
                    span("Cascade Effects")
                  ),
                  column(
                    width = 4,
                    div(
                      class = "d-flex justify-content-end",
                      style = "width: 100%;",
                      actionButton(ns("visualize_cascade"),
                        label = tagList(
                          phosphoricons::ph("waveform"),
                          "Visualize"
                        )
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
        #           height = "50.625vw") # 90% × 56.25% = ~16:9
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
mod_visualize_server <- function(id, argName1, argName2) {
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
