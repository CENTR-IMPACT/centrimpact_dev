#!! ModName = mod_home
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Price, Jeremy.  (2025). mod_home. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = argName1 !! argDescription !! argClass
#!! FunctionArg = argName2 !! argDescription !! argClass
#!! FunctionReturn = returnName1 !! returnDescription !! returnClass
#!! FunctionReturn = returnName2 !! returnDescription !! returnClass


# the ui function
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      fluidRow(
        column(
          width = 2,
          style = "text-align: center;",
          img(
            src = "centr-logo.png",
            class = "header-logo",
            style = "max-height: 180px; width: auto; margin: 0 auto 20px; display: block;"
          )
        ),
        column(
          width = 10,
            h2("CEnTR*IMPACT Toolkit"),
            p("Welcome to the CEnTR*IMPACT Toolkit. Use the tabs above to navigate through the workflow."),
            style = "font-family: 'Lato', sans-serif; margin-bottom: 20px;"
        )
      )
    )
  )
}


# the server function
mod_home_server <- function(id, argName1, argName2) {
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
