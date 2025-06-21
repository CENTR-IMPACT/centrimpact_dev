#!! ModName = mod_setup
# Load required libraries
library(phosphoricons)

#!! ModName = mod_setup
# !! ModDisplayName = Project Setup
# !! ModDescription = This module allows you to set up your project by selecting a directory, entering project details, and creating the necessary project structure. You can also edit author information and project configuration at any time.
# !! ModCitation = Price, Jeremy.  (2025). mod_setup. [Source code].
# !! ModNotes = Enter your module notes here.
# !! ModActive = 1/0
# !! FunctionArg = argName1 !! argDescription !! argClass
# !! FunctionArg = argName2 !! argDescription !! argClass
# !! FunctionReturn = returnName1 !! returnDescription !! returnClass
# !! FunctionReturn = returnName2 !! returnDescription !! returnClass


# the ui function
mod_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      id = ns("setup_navbar"),
      bslib::nav_panel(
        value = "overview",
        tagList(ph("lighthouse"), HTML("&nbsp;"), "Overview"), # Tab label
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
        tagList(ph("clipboard-text"), HTML("&nbsp;"), "Project Details"), # Tab label
        # Section ----
        fluidRow(
          column(width = 2),
          column(
            width = 8,
            fluidRow(
              width = 12,
              h1(
                  "Enter Project Information "
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
                checkboxGroupInput(ns("report_formats"), "Select output types:",
                  choiceNames =
                    list("Website", "PDF", "Word", "LaTeX", "Typst"),
                  choiceValues =
                    list("html", "pdf", "docx", "latex", "typst")
                )
              )
            )
          )
        )
      ),
      bslib::nav_panel(
        # value = "author_information",
        tagList(icon("users"), HTML("&nbsp;"), "Author Information"),
        fluidRow(
          h4(
            tagList(
              "Edit Author Information ",
              icon("users-between-lines", style = "margin-left: 10px;")
            )
          ),
          p("You may edit the author information. This information will be used in the project configuration and can be updated at any time. The author information is stored in a YAML file called `authors.yml` located in the `config` directory of your project."),
        ),
        fluidRow(
          column(width = 1),
          column(
            width = 5,
            shinyWidgets::textInputIcon(
              inputId = ns("author_name"),
              label = "Author Name",
              placeholder = "Enter author name",
              icon = icon("user"),
              width = "100%"
            ),
            shinyWidgets::textInputIcon(
              inputId = ns("author_affiliation"),
              label = "Author Affiliation",
              placeholder = "Enter author affiliation",
              icon = icon("building-columns"),
              width = "100%"
            ),
            shinyWidgets::textInputIcon(
              inputId = ns("author_orcid"),
              label = "Author ORCID",
              placeholder = "XXXX-XXXX-XXXX-XXXX",
              icon = icon("orcid"),
              width = "100%"
            ),
            shinyWidgets::textInputIcon(
              inputId = ns("author_email"),
              label = "Author Email",
              placeholder = "Enter author email",
              icon = icon("at"),
              width = "100%"
            ),
            shinyWidgets::textInputIcon(
              inputId = ns("author_url"),
              label = "Author URL",
              placeholder = "https://",
              icon = icon("link"),
              width = "100%"
            ),
            actionButton(ns("add_author_btn"), "Add Author", icon = icon("user-plus"))
          ),
          column(
            width = 5,
            style = "text-align: center; margin-top: 20px;",
            shinyAce::aceEditor(
              outputId = ns("author_yaml_code"),
              mode = "yaml",
              theme = "github",
              height = "300px",
              fontSize = 14,
              debounce = 750,
              autoScrollEditorIntoView = TRUE,
              highlightActiveLine = TRUE
            ),
            downloadButton(
              outputId = ns("download_authors"),
              label = "Download authors.yml",
              class = "btn-setup",
              icon = icon("file-download")
            )
          )
        )
      ),
      bslib::nav_panel(
        # value = "project_configuration",
        tagList(ph("palette"), HTML("&nbsp;"), "Branding"),
        # Section ----
        fluidRow(
          # Section ----
          column(
            width = 12,
            div(
              h4(
                tagList(
                  "Edit Project Branding ",
                  ph("palette", style = "margin-left: 10px;")
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
            shinyAce::aceEditor(
              outputId = ns("branding_yaml_code"),
              mode = "yaml",
              theme = "github",
              height = "300px",
              fontSize = 14,
              debounce = 750,
              autoScrollEditorIntoView = TRUE,
              highlightActiveLine = TRUE
            ),
            downloadButton(
              outputId = ns("download_branding"),
              label = "Download _brand.yml",
              class = "btn-setup",
              icon = icon("file-download")
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

    authors <- reactiveValues(list = list())
    
    # Load _brand.yml into the branding editor on initialization
    observe({
      req(input$setup_navbar)
      logger::log_info("Current tab:", input$setup_navbar)
      
      # The tab value is the text content of the tab, not the value attribute
      if (grepl("Branding", input$setup_navbar, ignore.case = TRUE)) {
        tryCatch({
          # Use file.path for cross-platform compatibility
          brand_file <- file.path("www", "_brand.yml")
          logger::log_info("Attempting to load file:", brand_file)
          
          if (file.exists(brand_file)) {
            brand_content <- readLines(brand_file, warn = FALSE)
            logger::log_info("File content length:", length(brand_content), "lines")
            
            shinyAce::updateAceEditor(
              session = session,
              editorId = "branding_yaml_code",
              value = paste(brand_content, collapse = "\n"),
              mode = "yaml"
            )
            logger::log_info("Successfully loaded _brand.yml into the editor")
          } else {
            logger::log_warn("File not found:", brand_file)
          }
        }, error = function(e) {
          logger::log_error("Error in loading _brand.yml: ", e$message)
          logger::log_error("Working directory: ", getwd())
        })
      }
    })
    
    observeEvent(input$add_author_btn, {
      logger::log_info("Adding new author with name:", input$author_name)
      # Save current author fields
      new_author <- list(
        name = input$author_name,
        affiliation = input$author_affiliation,
        orcid = input$author_orcid,
        email = input$author_email,
        url = input$author_url
      )
      authors$list <- append(authors$list, list(new_author))
      
      # Clear form fields
      updateTextInput(session, "author_name", value = "")
      updateTextInput(session, "author_affiliation", value = "")
      updateTextInput(session, "author_orcid", value = "")
      updateTextInput(session, "author_email", value = "")
      updateTextInput(session, "author_url", value = "")
      
      # Build YAML from all authors
      yaml_lines <- lapply(authors$list, function(author) {
        paste0(
          "- name: ", author$name, "\n",
          "  affiliations: ", author$affiliation, "\n",
          "  orcid: ", author$orcid, "\n",
          "  email: ", author$email, "\n",
          "  url: ", author$url
        )
      })
      yaml_text <- paste(yaml_lines, collapse = "\n")
      logger::log_info("Updating authors YAML with ", length(authors$list), " author(s)")
      shinyAce::updateAceEditor(session, "author_yaml_code", value = yaml_text, mode = "yaml")
      
      # Update workflow step to mark Project Setup as complete
      logger::log_info("Marking Project Setup as complete")
      update_workflow_step(
        ns_workflow,
        step = "Project Setup",
        stage = "complete",
        session = session
      )
    })
    
    # Also check on initialization in case we start on this tab
    observe({
      req(input$setup_navbar)
      current_tab <- trimws(tolower(input$setup_navbar))
      logger::log_trace("Checking initialization - current tab:", current_tab, "previous tab:", previous_tab())

      if (is.null(previous_tab()) && grepl("project details", current_tab, ignore.case = TRUE)) {
        logger::log_trace("Initialized on Project Details tab - updating workflow")
        update_workflow_step(
          ns_workflow,
          step = "Project Setup", # Changed from "setup" to match expected values
          stage = "in progress",
          session = session
        )
      }
    })


    update_workflow_step(
      ns_workflow,
      step = "Project Setup",
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
      ns_project$report_output <- input$report_output
    })

    output$download_authors <- downloadHandler(
      filename = function() "authors.yml",
      content = function(file) {
        # Get the YAML content from the Ace editor
        yaml_content <- input$author_yaml_code
        # Write the content to the file
        writeLines(yaml_content, file)
      },
      contentType = "text/yaml"
    )
    
    # Download handler for branding YAML
    output$download_branding <- downloadHandler(
      filename = function() "_brand.yml",
      content = function(file) {
        # Get the YAML content from the Ace editor
        yaml_content <- input$branding_yaml_code
        # Write the content to the file
        writeLines(yaml_content, file)
        
        # Update workflow step to complete
        update_workflow_step(
          ns_workflow,
          step = "Project Setup",
          stage = "complete",
          session = session
        )
      },
      contentType = "text/yaml"
    )

    output$report_output_icons <- renderUI({
      # Map each format to a Font Awesome icon (or generic if not available)
      icons_map <- list(
        "html" = icon("globe", class = "far-1x"),
        "pdf" = icon("file-pdf", class = "far-1x"),
        "word" = icon("file-word", class = "far-1x", lib = "font-awesome"),
        "latex" = icon("file-code", class = "far-1x"),
        "typst" = icon("file-invoice", class = "far-1x") # Use a generic code/file icon for Typst
      )

      selected <- input$report_formats
      if (is.null(selected) || length(selected) == 0) {
        return(NULL)
      }

      # Create a span for each selected icon+label
      spans <- lapply(selected, function(choice) {
        tags$span(
          icons_map[[choice]],
          tags$span(choice, style = "margin-right: 15px; margin-left: 5px;")
        )
      })
      tagList(spans)
    })

    # Create a reactive value to store the icons UI
    report_icons_ui <- reactive({
      req(input$report_formats)
      # Map each format to a Font Awesome icon (or generic if not available)
      icons_map <- list(
        "html" = icon("globe"),
        "pdf" = icon("file-pdf"),
        "docx" = icon("file-word"),
        "latex" = icon("file-code"),
        "typst" = icon("file-invoice")
      )

      selected <- input$report_formats
      if (is.null(selected) || length(selected) == 0) {
        return(NULL)
      }

      # Create a span for each selected icon+label
      spans <- lapply(selected, function(choice) {
        tags$span(
          icons_map[[choice]]
        )
      })
      tagList(spans)
    })

    # Local output for the module UI
    output$report_output_icons <- renderUI({
      report_icons_ui()
    })

    # Return the reactive UI for use in the main app
    return(
      reactiveValues(
        returnName1 = reactive(returnName1()),
        returnName2 = reactive(returnName2()),
        report_icons_ui = report_icons_ui
      )
    )
  })
}
