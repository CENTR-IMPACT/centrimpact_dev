#!! ModName = mod_generate
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Price, Jeremy F.  (2025). mod_generate. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = argName1 !! argDescription !! argClass
#!! FunctionArg = argName2 !! argDescription !! argClass
#!! FunctionReturn = returnName1 !! returnDescription !! returnClass
#!! FunctionReturn = returnName2 !! returnDescription !! returnClass


# the ui function
mod_generate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      id = ns("generate_tabs"), # Add an ID to the navset for switching
      bslib::nav_panel(
        "Overview",
        icon = icon("compass"),
        fluidRow(
          column(
            width = 8,
            h4(
              "Generate Report Template",
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
              src = "report.png",
              alt = "Generate Data Image",
              style = "max-width: 100%; height: auto; margin-top: 20px; padding-left: 10%; padding-right: 10%;"
            )
          ),
        )
      ),
      bslib::nav_panel(
        "Generate Report",
        icon = icon("file-alt"),
        fluidRow(
        )
      )
    )
  )
}


# the server function
mod_generate_server <- function(id, argName1, argName2) {
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
