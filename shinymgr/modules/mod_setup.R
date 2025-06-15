#!! ModName = mod_setup
#!! ModDisplayName = Project Setup
#!! ModDescription = This module allows you to set up your project by selecting a directory, entering project details, and creating the necessary project structure. You can also edit author information and project configuration at any time.
#!! ModCitation = Price, Jeremy.  (2025). mod_setup. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = argName1 !! argDescription !! argClass
#!! FunctionArg = argName2 !! argDescription !! argClass
#!! FunctionReturn = returnName1 !! returnDescription !! returnClass
#!! FunctionReturn = returnName2 !! returnDescription !! returnClass


# the ui function
mod_setup_ui <- function(id) {
  ns <- NS(id)
tagList(
  bslib::navset_card_tab(
    id = ns("setup_navbar"),
    bslib::nav_panel(
      value = "overview",
      tagList(icon("compass"), HTML("&nbsp;"), "Overview"),  # Tab label
      fluidRow(
        column(
          width = 8,
          h1("Project Setup"),
          p("This module allows you to set up your project by selecting a directory, entering project details, and creating the necessary project structure. You can also edit author information and project configuration at any time."),
          p("Please follow the steps in the tabs below to complete the project setup.")
        ),
        column(
          width = 4,
          img(
            src = "setup.png",
            alt = "Project Setup Image",
            style = "max-width: 100%; height: auto; margin-top: 20px; padding-left: 10%; padding-right: 10%;"
          )
        )
      )
    ),
    bslib::nav_panel(
      value = "project details",
      tagList(icon("clipboard-list"), HTML("&nbsp;"), "Project Details"),  # Tab label
      # Section ----
      fluidRow(
        column(width = 2),
        column(
          width = 8,
          fluidRow(
            width = 12,
          h1(
            tagList(
              "Enter Project Information ",
              icon("diagram-project", style = "margin-left: 10px;")
            )
          ),
          p("Enter the appropriate information for your project. This will be used to generate the project structure and configuration files. You will have the opportunity to update this information at any time by editing the YAML files that are created."),
          fluidRow(
            column(
              width = 6,
              textInput(ns("project_title"), "Project Title", width = "100%")
            ),
            column(
              width = 6,
              dateInput(ns("report_date"),
                      "Report Date",
                      value = NULL,
                      width = "100%",
                      format = "yyyy-mm-dd",
                      startview = "month"
            )
            )
            ),
          fluidRow(
            textAreaInput(ns("project_description"), "Project Description",
                          placeholder = "Enter a brief description of the project",
                          rows = 3, resize = "vertical", width = "100%"
            )
          ),
          fluidRow(
            checkboxGroupInput("report-output", "Select output types:",
                               choiceNames =
                                 list("Website", "PDF", "Word"),
                               choiceValues =
                                 list("html", "pdf", "docx")
            ),
            textOutput("txt")
          )
        )
        )
      )
    ),
  bslib::nav_panel(
    #value = "author_information",
    tagList(icon("user-pen"), HTML("&nbsp;"), "Author Information"),
    # Section ----
    fluidRow(
      # Section ----
      column(
        width = 12,
        div(
          h4(
            tagList(
              "Edit Author Information ",
              icon("users-between-lines", style = "margin-left: 10px;")
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          style = "gap: 10px;",
          p("You may edit the author information. This information will be used in the project configuration and can be updated at any time. The author information is stored in a YAML file called `authors.yml` located in the `config` directory of your project."),
        )
      ),
      column(
        width = 6,
        fluidRow(
          column(
            width = 12,
            style = "margin-bottom: 15px;",
            div(
              downloadButton("download_authors", "Download Authors File",
                             icon = icon("file-download"),
                             class = "btn-setup")
              )
        ),
        textAreaInput(
          ns("authors_yml_editor"),
          label = NULL,
          width = "100%",
          height = "300px",
          resize = "vertical"
        )
      )
    )
  )
  ),
    bslib::nav_panel(
      #value = "project_configuration",
tagList(icon("sliders"), HTML("&nbsp;"), "Branding"),
      # Section ----
      fluidRow(
        # Section ----
        column(
          width = 12,
          div(
            h4(
              tagList(
                  "Edit Project Configuration ",
                  icon("palette", style = "margin-left: 10px;")
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            div(
              style = "gap: 10px;",
              p("You may edit the project configuration. This information will be used in the project configuration and can be updated at any time. The project configuration is stored in a YAML file called `config.yml` located in the `config` directory of your project."),
            )
          ),
          column(
            width = 6,
            fluidRow(
              column(
                width = 12,
                style = "margin-bottom: 15px;",
                div(
                  class = "yaml-buttons",
                  actionButton(ns("load_config_btn"), "Load",
                               icon = icon("file-import"),
                               class = "btn-setup"
                  ),
                  actionButton(ns("save_config_btn"), "Save",
                               icon = icon("save"),
                               class = "btn-setup"
                  )
                )
              )
            ),
            textAreaInput(
              ns("config_yml_editor"),
              label = NULL,
              width = "100%",
              height = "300px",
              resize = "vertical"
            )
          )
        )
      )
)
)
}


# the server function
mod_setup_server <- function(id, ns_authors, ns_project, ns_workflow) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    logger::log_info("Initiating project setup module")
    
    # Track the previously selected tab
    previous_tab <- reactiveVal(NULL)
    
    # Observe tab changes
    observeEvent(input$setup_navbar, {
      current_tab <- input$setup_navbar
      logger::log_trace("Tab changed to:", current_tab)
      
      # Check if we're switching to the Project Details tab
      if (current_tab == "project details") {
        logger::log_trace("Project Details tab selected - updating workflow")
        update_workflow_step(
          ns_workflow,
          step = "setup",
          stage = "in progress",
          session = session
        )
      }
      
      # Update the previous tab
      previous_tab(current_tab)
    }, ignoreInit = TRUE)
    
    # Also check on initialization in case we start on this tab
    observe({
      req(input$setup_navbar)
      if (is.null(previous_tab()) && input$setup_navbar == "project details") {
        logger::log_trace("Initialized on Project Details tab - updating workflow")
        update_workflow_step(
          ns_workflow,
          step = "setup",
          stage = "in progress",
          session = session
        )
      }
    })
    
    
    update_workflow_step(
      ns_workflow,
      step = "setup",
      stage = "init",
      session = session
    )
    
    debounced_title <- debounce(reactive(input$project_title), millis = 750)
    
    observeEvent(debounced_title(), {
      ns_project$project_title <- debounced_title()
      logger::log_info("Project title updated:", ns_project$project_title)
    })
    
    observe({
      updateDateInput(session, "report_date", value = ns_project$project_report_date)
      updateTextAreaInput(session, "project_description", value = ns_project$project_description)
    })
    
    
    observe({
      ns_project$project_title <- input$project_title
      ns_project$project_report_date <- input$report_date
      ns_project$project_description <- input$project_description
    })
    
    output$download_authors <- downloadHandler(
      filename = function() "authors.yml",
      content = function(file) {
        file.copy("www/authors.yml", file)
      }
    )
    
    return(
      reactiveValues(
        returnName1 = reactive(returnName1()),
        returnName2 = reactive(returnName2())
      )
    )
  })
}
