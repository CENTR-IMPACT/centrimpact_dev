#!! ModName = mod_home
# !! ModDisplayName = Enter your module shiny display name here.
# !! ModDescription = Enter your module description here.
# !! ModCitation = Price, Jeremy.  (2025). mod_home. [Source code].
# !! ModNotes = Enter your module notes here.
# !! ModActive = 1/0
# !! FunctionArg = argName1 !! argDescription !! argClass
# !! FunctionArg = argName2 !! argDescription !! argClass
# !! FunctionReturn = returnName1 !! returnDescription !! returnClass
# !! FunctionReturn = returnName2 !! returnDescription !! returnClass


# the ui function
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
        img(
          src = "centr_logo.png",
          class = "header-logo",
          style = "max-width: 80%; height: auto; max-height: 120px; margin: 0 auto 20px; display: block;"
      ),
        fluidRow(
          column(
            width = 6,
            create_flat_info_card(
              ns,
              title = "Welcome",
              icon = "hand-waving",
              content = tagList(
                p("Welcome to the CEnTR*IMPACT Toolkit. Use the tabs above to navigate through the workflow.")
              )
            )
          ),
          column(
            width = 6,
            create_flat_info_card(
              ns,
              title = "Workflow",
              icon = "map-pin-area",
              content = img(
                src = "blueprint-wf.png",
                alt = "CEnTR*IMPACT Logo",
                style = "max-width: 100%; border-radius: 18px !important; box-shadow: 4px 4px 8px rgba(0, 0, 0, 0.2); height: auto;"
              )
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
