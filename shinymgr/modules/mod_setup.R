#!! ModName = mod_setup
#!! ModDisplayName = Project Setup
#!! ModDescription = Set up project information and configuration
#!! ModCitation = Price, Jeremy F. (2025). mod_setup. [Source code].
#!! ModNotes = This module provides functionality to set up project information.
#!! ModActive = 1
#!! FunctionArg = project_data !! Project data for setup !! reactive
#!! FunctionArg = ns_workflow !! Namespace for workflow updates !! character

# Load required libraries
#' @importFrom phosphoricons ph
#' @importFrom shinyAce updateAceEditor
#' @importFrom logger log_info log_warn log_error log_trace

# Utilities are loaded in global.R
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom shinyToastify showToast
#' @importFrom shinyvalidate InputValidator sv_required sv_between sv_in_set



# Generic function to update workflow state and UI for any metric
# update_workflow_state <- function(ns_workflow, session, stage, status, ns) {
#   logger::log_info("update_workflow_state called with parameters:")
#   logger::log_info("Stage: {stage}, Status: {status}")

#   update_workflow_step(
#     ns_workflow,
#     stage = stage,
#     status = status,
#     session = session,
#     ns = ns
#   )

#   logger::log_info("Workflow step updated successfully.")

#   # Workflow step updated - no need for metric-specific handling
#   logger::log_info("Workflow state updated successfully.")
# }

# !! ModName = mod_setup
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

    # shinyToastify::useShinyToastify(),
    # shiny::useBusyIndicators(),
    # waiter::useWaiter(),
    tags$head(
      tags$style(HTML(paste0(
        "#", ns("details_status_message, "),
        "#", ns("author_status_message"), " {
          display: block;
          padding-left: 0.5em;
          z-index: 10;
          width: 100%;
          font-family: 'Share Tech Mono' !important;
        }"
      )))
    ),
    bslib::accordion(
      id = ns("setup_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "overview",
        title = tagList(ph("lighthouse"), HTML("&nbsp;"), "Overview"),
        fluidRow(
          column(
            width = 8,
            p("This module allows you to set up your project by selecting a directory, entering project details, and creating the necessary project structure. You can also edit author information and project configuration at any time."),
            p("Please follow the steps in the accordions below to complete the project setup.")
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
      bslib::accordion_panel(
        value = "project_details",
        title = tagList(ph("clipboard-text"), HTML("&nbsp;"), "Project Details"),
        div(
          class = "status-line",
          style = "display: flex; align-items: center; gap: 0.5em; margin-bottom: 1.25em;",
          div(
            style = "color: #3F5E78;",
            id = ns("details_status_icon"),
            ph("check-circle", weight = "bold", class = "sidebar-icon")
          ),
          span(
            style = "color: #3F5E78;",
            id = ns("details_status_message"),
            "Ready"
          )
        ),
        fluidRow(
          column(
            width = 6,
            style = "padding-right: 1em;",
            textInput(
              ns("project_title"),
              "Project Title",
              placeholder = "Enter project title",
              width = "100%"
            )
          ),
          column(
            width = 6,
            style = "padding-left: 1em;",
            dateInput(
              ns("report_date"),
              "Report Date",
              value = Sys.Date(),
              width = "100%",
              format = "yyyy-mm-dd",
              startview = "month"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            textAreaInput(
              ns("project_description"),
              "Project Description",
              placeholder = "Enter a concise description of your project",
              rows = 3,
              resize = "vertical",
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            style = "padding-right: 1em;",
            checkboxGroupInput(
              ns("report_formats"),
              "Report Formats",
              width = "100%",
              inline = TRUE,
              choiceNames = list("Website", "PDF", "Word", "LaTeX", "Typst"),
              choiceValues = list("html", "pdf", "docx", "latex", "typst")
            )
          ),
          column(
            width = 6,
            style = "padding-left: 1em;",
            selectizeInput(
              inputId = ns("project_keywords"),
              label = "Project Keywords",
              width = "100%",
              choices = NULL,
              multiple = TRUE,
              options = list(
                create = TRUE,
                delimiter = ",",
                placeholder = "Enter keywords separated by commas"
              )
            )
          )
        ),
        fluidRow(
          div(
            class = "d-flex justify-content-evenly align-items-center",
            actionButton(
              ns("initiate_project"),
              tagList(
                ph("floppy-disk", weight = "bold"),
                HTML("&nbsp;"),
                "Save Project Details"
              ),
              class = "btn-lrg btn btn-primary"
            ),
            actionButton(
              ns("reset_form"),
              tagList(
                ph("arrow-counter-clockwise"),
                HTML("&nbsp;"),
                "Reset Form"
              ),
              class = "btn btn-secondary"
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "author_information",
        title = tagList(ph("user-list"), HTML("&nbsp;"), "Author Information"),
        fluidRow(
          column(
            width = 12,
            p("You may edit the author information. This information will be used in the project configuration and can be updated at any time. The author information is stored in a YAML file called `authors.yml` located in the `config` directory of your project.")
          )
        ),
        fluidRow(
          div(
            class = "status-line",
            style = "display: flex; align-items: center; gap: 0.5em; margin-bottom: 1.25em;",
            div(
              style = "color: #3F5E78;",
              id = ns("author_status_icon"),
              ph("check-circle", weight = "bold", class = "sidebar-icon")
            ),
            span(
              style = "color: #3F5E78;",
              id = ns("author_status_message"),
              "Ready"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            shinyWidgets::textInputIcon(
              inputId = ns("author_name"),
              label = tagList("Author Name", ph("asterisk", weight = "bold")),
              placeholder = "Enter author name",
              icon = icon("user"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            style = "padding-right: 1em;",
            shinyWidgets::textInputIcon(
              inputId = ns("author_affiliation"),
              label = "Author Affiliation",
              placeholder = "Enter author affiliation",
              icon = icon("building-columns"),
              width = "100%"
            )
          ),
          column(
            width = 6,
            style = "padding-left: 1em;",
            shinyWidgets::textInputIcon(
              inputId = ns("author_orcid"),
              label = "Author ORCID",
              placeholder = "XXXX-XXXX-XXXX-XXXX",
              icon = icon("orcid"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            style = "padding-right: 1em;",
            shinyWidgets::textInputIcon(
              inputId = ns("author_email"),
              label = "Author Email",
              placeholder = "Enter author email",
              icon = icon("at"),
              width = "100%"
            )
          ),
          column(
            width = 6,
            style = "padding-left: 1em;",
            shinyWidgets::textInputIcon(
              inputId = ns("author_url"),
              label = "Author URL",
              placeholder = "https://",
              icon = icon("link"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            style = "margin-top: 2em; text-align: center;",
            div(
              class = "d-flex justify-content-evenly align-items-center",
              actionButton(
                ns("add_author_btn"),
                tagList(
                  ph("user-circle-plus"),
                  HTML("&nbsp;"),
                  "Add Author"
                ),
                class = "btn-lrg btn btn-primary"
              ),
              actionButton(
                ns("download_authors"),
                tagList(
                  ph("file-arrow-down"),
                  HTML("&nbsp;"),
                  "Download authors.yml"
                ),
                class = "btn-lrg btn btn-primary"
              ),
              actionButton(
                ns("reset_author_form"),
                tagList(
                  ph("arrow-counter-clockwise"),
                  HTML("&nbsp;"),
                  "Reset Form"
                ),
                class = "btn-lrg btn btn-secondary"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            style = "margin-top: 2em;",
            h4("YAML Preview"),
            shinyAce::aceEditor(
              outputId = ns("author_yaml_code"),
              mode = "yaml",
              theme = "github",
              height = "300px",
              fontSize = 14,
              debounce = 750,
              autoScrollEditorIntoView = TRUE,
              highlightActiveLine = TRUE
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "branding",
        title = tagList(ph("palette"), HTML("&nbsp;"), "Branding"),
        fluidRow(
          column(
            width = 12,
            h4(tagList("Edit Project Branding ", ph("palette", style = "margin-left: 10px;")))
          )
        ),
        fluidRow(
          column(
            width = 6,
            p("You may edit the project configuration. This information will be used in the project configuration and can be updated at any time. The project configuration is stored in a YAML file called `config.yml` located in the `config` directory of your project.")
          ),
          column(
            width = 6,
            downloadButton(
              outputId = ns("download_branding"),
              label = "Download _brand.yml",
              class = "btn-setup",
              icon = icon("file-download")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            shinyAce::aceEditor(
              outputId = ns("branding_yaml_code"),
              mode = "yaml",
              theme = "github",
              height = "300px",
              fontSize = 14,
              debounce = 750,
              autoScrollEditorIntoView = TRUE,
              highlightActiveLine = TRUE
            )
          )
        )
      )
    )
  )
}


# the server function
mod_setup_server <- function(id, project_data, ns_workflow) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    logger::log_info("Initiating project setup module")
    
    # Initialize input validation
    iv <- InputValidator$new()

    # Validate Project Title - required and minimum length
    iv$add_rule("project_title", sv_required())

    iv$add_rule("author_name", sv_required())

    iv$add_rule("author_email", sv_optional())
    iv$add_rule("author_email", sv_email())

    iv$add_rule("author_url", sv_optional())
    iv$add_rule("author_url", sv_url())

    # Validate Report Date - required and must be a valid date
    iv$add_rule("report_date", sv_required())
    iv$add_rule("report_date", function(value) {
      if (is.null(value) || is.na(value)) {
        "Report date is required"
      } else {
        tryCatch(
          {
            date_val <- as.Date(value)
            if (is.na(date_val)) {
              "Please enter a valid date"
            } else if (date_val < as.Date("1900-01-01")) {
              "Date cannot be before 1900"
            } else if (date_val > as.Date("2100-12-31")) {
              "Date cannot be after 2100"
            }
          },
          error = function(e) {
            "Please enter a valid date"
          }
        )
      }
    })

    # Validate Report Formats - at least one format must be selected
    iv$add_rule("report_formats", sv_required())
    # iv$add_rule("report_formats", function(value) {
    #   if (is.null(value) || length(value) == 0) {
    #     "Please select at least one report format"
    #   }
    # })

    # Enable validation
    iv$enable()

    # Helper to robustly check for blank/empty/NA values
    is_blank <- function(x) {
      # Handle vectors by checking if all elements are blank
      if (length(x) > 1) {
        return(all(sapply(x, is_blank)))
      }

      if (is.null(x)) {
        return(TRUE)
      }
      if (is.na(x)) {
        return(TRUE)
      }
      if (is.character(x)) {
        if (length(x) == 0) {
          return(TRUE)
        }
        if (all(is.na(x))) {
          return(TRUE)
        }
        # Safely handle trimws by checking length first
        if (length(x) > 0 && all(nchar(trimws(x)) == 0)) {
          return(TRUE)
        }
      }
      if (is.list(x) && length(x) == 0) {
        return(TRUE)
      }
      if (is.atomic(x) && length(x) == 0) {
        return(TRUE)
      }
      FALSE
    }

    # Track editing and saved state for status line
    editing_active <- reactiveVal(FALSE)
    author_editing_active <- reactiveVal(FALSE) # Separate tracking for author editing
    details_saved <- reactiveVal(FALSE)
    last_edit_time <- reactiveVal(NULL)
    last_author_edit_time <- reactiveVal(NULL) # Separate edit time for author fields
    status_changed <- reactiveVal(FALSE)

    # Track the previously selected tab
    # Tab navigation is now handled by the main server logic

    authors <- reactiveValues(list = list())



    # Workflow status reactive for status_line
    workflow_status <- reactive({
      # Defensive: check if ns_workflow and ns_workflow$status exist
      if (is.null(ns_workflow) || is.null(ns_workflow$status)) {
        return("not_started")
      }
      statuses <- reactiveValuesToList(ns_workflow$status)
      if (length(statuses) == 0) {
        return("not_started")
      }
      if (any(unlist(statuses) == "error")) {
        "error"
      } else if (all(unlist(statuses) == "complete")) {
        "complete"
      } else if (any(unlist(statuses) == "in progress" | unlist(statuses) == "in_progress")) {
        "in_progress"
      } else {
        "not_started"
      }
    })



    # Reactive expression to determine current status
    current_status <- reactive({
      # Safely check if inputs exist before calling is_blank
      title_blank <- is.null(input$project_title) || is_blank(input$project_title)
      desc_blank <- is.null(input$project_description) || is_blank(input$project_description)
      formats_blank <- is.null(input$report_formats) || is_blank(input$report_formats)
      keywords_blank <- is.null(input$project_keywords) || is_blank(input$project_keywords)
      # Special handling for date values
      date_blank <- tryCatch(
        {
          is.null(input$report_date) || is_blank(input$report_date)
        },
        error = function(e) {
          logger::log_warn("Error checking date blank status: {conditionMessage(e)}")
          TRUE # Assume blank if there's an error
        }
      )

      if (
        !details_saved() && !editing_active() &&
          title_blank &&
          desc_blank &&
          formats_blank &&
          keywords_blank &&
          date_blank
      ) {
        "no_details"
      } else if (details_saved()) {
        "saved"
      } else if (editing_active()) {
        "editing"
      } else {
        "unknown"
      }
    })

    ready_icon <- as.character(phosphoricons::ph("check-circle", weight = "regular", class = "sidebar-icon"))
    ready_icon <- paste0("<span style = 'color: #3F5E78;'>", ready_icon, "</span>")
    ready_message <- paste0("<span style = 'color: #3F5E78;'>Ready</span>")

    observeEvent(input$initiate_project, {
      logger::log_info("Save Project Details button clicked")

      # Use shinyvalidate to check all validation rules
      if (!iv$is_valid()) {
        logger::log_warn("Form validation failed - validation errors will be shown automatically")
        return()
      }

      logger::log_info("Form validation passed - all fields are valid")

      # Log the current state for debugging
      logger::log_info("=== After Save ===")
      # Store project info in project_data
      project_data$project_info$title <- input$project_title
      project_data$project_info$description <- input$project_description
      project_data$project_info$report_formats <- input$report_formats
      project_data$project_info$report_keywords <- input$project_keywords
      project_data$project_info$report_date <- tryCatch(
        as.character(as.Date(input$report_date)),
        error = function(e) as.character(Sys.Date())
      )
      
      logger::log_info("Project title: ", project_data$project_info$title)
      logger::log_info("Report date: ", project_data$project_info$report_date, " (class: ", class(project_data$project_info$report_date), ")")
      logger::log_info("Report formats: ", paste(project_data$project_info$report_formats, collapse = ", "))

      #   # Show progress bar
      #   session$sendCustomMessage("show-progress", list(
      #     id = ns("progress_container_1"),
      #     show = TRUE
      #   ))

      # Show success message
      saved_icon <- as.character(phosphoricons::ph("cloud-check", weight = "light", class = "sidebar-icon"))
      saved_icon <- paste0("<span style = 'color: #3B6B35;'>", saved_icon, "</span>")
      saved_message <- paste0("<span style = 'color: #3B6B35;'>Details Saved</span>")
      shinyjs::html("details_status_icon", saved_icon)
      shinyjs::html("details_status_message", saved_message)



      # Hide after 4 seconds
      shinyjs::delay(4000, {
        shinyjs::html("details_status_message", ready_message)
        shinyjs::html("details_status_icon", ready_icon)
      })
    })


    # Load _brand.yml into the branding editor on initialization
    observe({
      req(input$setup_accordion)
      logger::log_info(skip_formatter("Current accordion panel: {input$setup_accordion}"))

      # Check if the branding accordion panel is open
      if ("branding" %in% input$setup_accordion) {
        tryCatch(
          {
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
          },
          error = function(e) {
            logger::log_error("Error in loading _brand.yml: ", e$message)
            logger::log_error("Working directory: ", getwd())
          }
        )
      }
    })

    # Update editing state for project title and set ns_project$project_title
    observeEvent(input$project_title,
      {
        if (!is.null(input$project_title) && nchar(trimws(input$project_title)) > 0) {
          trigger_editing()
        }
        project_data$project_info$title <- input$project_title
      },
      ignoreInit = TRUE
    )

    # Update editing state for report date
    observeEvent(input$report_date,
      {
        # Safely handle report date
        tryCatch(
          {
            if (!is.null(input$report_date)) {
              # Ensure we have a valid date value
              date_val <- as.Date(input$report_date)
              if (!is.na(date_val)) {
                trigger_editing()
              }
            }
          },
          error = function(e) {
            logger::log_warn("Error processing report date: {conditionMessage(e)}")
          }
        )
      },
      ignoreInit = TRUE
    )

    # Helper to trigger editing state and update last edit time
    trigger_editing <- function() {
      details_saved(FALSE)
      logger::log_info("Setting editing_active to TRUE in trigger_editing()")
      editing_active(TRUE)
      edit_icon <- as.character(phosphoricons::ph("pencil-line", weight = "regular", class = "sidebar-icon"))
      edit_icon <- paste0("<span style = 'color: #6E6272;'>", edit_icon, "</span>")
      edit_message <- paste0("<span style = 'color: #6E6272;'>Editing</span>")
      shinyjs::html("details_status_icon", edit_icon)
      shinyjs::html("details_status_message", edit_message)
      last_edit_time(Sys.time())
    }

    # Helper to trigger author editing state (no autosave message)
    trigger_author_editing <- function() {
      logger::log_info("Setting author_editing_active to TRUE in trigger_author_editing()")
      author_editing_active(TRUE)
      edit_icon <- as.character(phosphoricons::ph("pencil-line", weight = "regular", class = "sidebar-icon"))
      edit_icon <- paste0("<span style = 'color: #6E6272;'>", edit_icon, "</span>")
      edit_message <- paste0("<span style = 'color: #6E6272;'>Editing</span>")
      shinyjs::html("author_status_icon", edit_icon)
      shinyjs::html("author_status_message", edit_message)
      last_author_edit_time(Sys.time())
    }

    # Observer to handle editing timeout (5 seconds)
    observe({
      if (editing_active()) {
        invalidateLater(1000, session)
        if (!is.null(last_edit_time()) && as.numeric(difftime(Sys.time(), last_edit_time(), units = "secs")) > 5) {
          logger::log_info("Setting editing_active to FALSE due to timeout in observe")
          editing_active(FALSE)
          shinyjs::html("details_status_message", ready_message)
          shinyjs::html("details_status_icon", ready_icon)
        }
      }
    })

    # Observer to handle author editing timeout (5 seconds) - no autosave message
    observe({
      if (author_editing_active()) {
        invalidateLater(1000, session)
        if (!is.null(last_author_edit_time()) && as.numeric(difftime(Sys.time(), last_author_edit_time(), units = "secs")) > 5) {
          logger::log_info("Setting author_editing_active to FALSE due to timeout")
          author_editing_active(FALSE)
          shinyjs::html("author_status_message", ready_message)
          shinyjs::html("author_status_icon", ready_icon)
        }
      }
    })

    # Observer to show autosave message when editing becomes inactive
    observeEvent(editing_active(), {
      if (!editing_active()) {
        # Only show autosave message if author editing is also not active
        if (!author_editing_active()) {
          save_icon <- as.character(phosphoricons::ph("floppy-disk", weight = "light", class = "sidebar-icon"))
          save_icon <- paste0("<span style = 'color: #4B7F52;'>", save_icon, "</span>")
          save_message <- paste0("<span style = 'color: #4B7F52;'>Autosaved</span>")

          logger::log_info("Autosave observer triggered: editing_active is FALSE, showing autosave message")

          shinyjs::html("details_status_icon", save_icon)
          shinyjs::html("details_status_message", save_message)

          # Hide after 2 seconds
          shinyjs::delay(2000, {
            shinyjs::html("details_status_message", ready_message)
            shinyjs::html("details_status_icon", ready_icon)
          })
        }
      } else {
        logger::log_info("autosave observer triggered: editing_active is TRUE, not showing autosave message")
        edit_icon <- as.character(phosphoricons::ph("pencil-line", weight = "regular", class = "sidebar-icon"))
        edit_icon <- paste0("<span style = 'color: #6E6272;'>", edit_icon, "</span>")
        edit_message <- paste0("<span style = 'color: #6E6272;'>Editing</span>")
        shinyjs::html("details_status_icon", edit_icon)
        shinyjs::html("details_status_message", edit_message)
      }
    })



    # Observe all relevant inputs for editing state
    observeEvent(input$project_title,
      {
        if (!is.null(input$project_title) && nchar(trimws(input$project_title)) > 0) {
          trigger_editing()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$project_description,
      {
        if (!is.null(input$project_description) && nchar(trimws(input$project_description)) > 0) {
          trigger_editing()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$report_formats,
      {
        if (!is.null(input$report_formats) && length(input$report_formats) > 0) {
          trigger_editing()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$project_keywords,
      {
        if (!is.null(input$project_keywords) && length(input$project_keywords) > 0) {
          trigger_editing()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$report_date,
      {
        # Safely handle report date
        tryCatch(
          {
            if (!is.null(input$report_date)) {
              # Ensure we have a valid date value
              date_val <- as.Date(input$report_date)
              if (!is.na(date_val)) {
                trigger_editing()
              }
            }
          },
          error = function(e) {
            logger::log_warn("Error processing report date: {conditionMessage(e)}")
          }
        )
      },
      ignoreInit = TRUE
    )

    # Author information editing observers
    observeEvent(input$author_name,
      {
        if (!is.null(input$author_name) && nchar(trimws(input$author_name)) > 0) {
          trigger_author_editing()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$author_affiliation,
      {
        if (!is.null(input$author_affiliation) && nchar(trimws(input$author_affiliation)) > 0) {
          trigger_author_editing()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$author_orcid,
      {
        if (!is.null(input$author_orcid) && nchar(trimws(input$author_orcid)) > 0) {
          trigger_author_editing()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$author_email,
      {
        if (!is.null(input$author_email) && nchar(trimws(input$author_email)) > 0) {
          trigger_author_editing()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$author_url,
      {
        if (!is.null(input$author_url) && nchar(trimws(input$author_url)) > 0) {
          trigger_author_editing()
        }
      },
      ignoreInit = TRUE
    )



    # Reset form and status on Reset Form button
    observeEvent(input$reset_form,
      {
        details_saved(FALSE)
        logger::log_info("Setting editing_active to FALSE in reset logic")
        editing_active(FALSE)
        last_edit_time(NULL)
        updateTextInput(session, "project_title", value = "")
        updateTextAreaInput(session, "project_description", value = "")
        updateCheckboxGroupInput(session, "report_formats", selected = character(0))
        updateSelectizeInput(session, "project_keywords", selected = character(0))
        updateDateInput(session, "report_date", value = Sys.Date())
      },
      ignoreInit = TRUE
    )

    observeEvent(input$add_author_btn, {
      # Safely get author input values with NULL checks and trim whitespace
      author_name <- if (!is.null(input$author_name)) trimws(input$author_name) else ""
      author_affiliation <- if (!is.null(input$author_affiliation)) trimws(input$author_affiliation) else ""
      author_orcid <- if (!is.null(input$author_orcid)) trimws(input$author_orcid) else ""
      author_email <- if (!is.null(input$author_email)) trimws(input$author_email) else ""
      author_url <- if (!is.null(input$author_url)) trimws(input$author_url) else ""

      # Require author name
      if (author_name == "") {
        logger::log_warn("Author name is required but was not provided")
        error_icon <- as.character(phosphoricons::ph("warning-octagon", weight = "fill", class = "sidebar-icon"))
        error_icon <- paste0("<span style = 'color: #990000;'>", error_icon, "</span>")
        error_message <- paste0("<span style = 'color: #990000;'>Name Required</span>")
        shinyjs::html("author_status_icon", error_icon)
        shinyjs::html("author_status_message", error_message)
        # Hide after 2 seconds
        shinyjs::delay(2000, {
          shinyjs::html("author_status_message", ready_message)
          shinyjs::html("author_status_icon", ready_icon)
        })
        return()
      }

      logger::log_info("Adding new author with name:", author_name)
      # Save only non-empty author fields
      new_author <- list(name = author_name)
      if (author_affiliation != "") new_author$affiliation <- author_affiliation
      if (author_orcid != "") new_author$orcid <- author_orcid
      if (author_email != "") new_author$email <- author_email
      if (author_url != "") new_author$url <- author_url
      authors$list <- append(authors$list, list(new_author))

      # Clear form fields
      updateTextInput(session, "author_name", value = "")
      updateTextInput(session, "author_affiliation", value = "")
      updateTextInput(session, "author_orcid", value = "")
      updateTextInput(session, "author_email", value = "")
      updateTextInput(session, "author_url", value = "")

      # Build YAML from all authors, only including non-empty fields and always using 'affiliations' in YAML
      yaml_lines <- lapply(authors$list, function(author) {
        lines <- c(paste0("- name: ", author$name))
        if (!is.null(author$affiliation) && author$affiliation != "") {
          lines <- c(lines, paste0("  affiliations: ", author$affiliation))
        }
        if (!is.null(author$orcid) && author$orcid != "") {
          lines <- c(lines, paste0("  orcid: ", author$orcid))
        }
        if (!is.null(author$email) && author$email != "") {
          lines <- c(lines, paste0("  email: ", author$email))
        }
        if (!is.null(author$url) && author$url != "") {
          lines <- c(lines, paste0("  url: ", author$url))
        }
        paste(lines, collapse = "\n")
      })
      yaml_text <- paste(yaml_lines, collapse = "\n")
      logger::log_info("Updating authors YAML with ", length(authors$list), " author(s)")
      shinyAce::updateAceEditor(session, "author_yaml_code", value = yaml_text, mode = "yaml")

      # Authors updated - no need to mark Project Setup as complete automatically
      logger::log_info("Authors YAML updated successfully")
      # Show success message
      success_icon <- as.character(phosphoricons::ph("user-check", weight = "light", class = "sidebar-icon"))
      success_icon <- paste0("<span style = 'color: #3B6B35;'>", success_icon, "</span>")
      success_message <- paste0("<span style = 'color: #3B6B35;'>Author Added</span>")
      shinyjs::html("author_status_icon", success_icon)
      shinyjs::html("author_status_message", success_message)
      # Hide after 2 seconds
      shinyjs::delay(2000, {
        shinyjs::html("author_status_message", ready_message)
        shinyjs::html("author_status_icon", ready_icon)
      })
      author_editing_active(FALSE) # Reset author editing state
    })

    # Tab navigation is now handled by the main server logic


    logger::log_info("Project Setup module initialized - workflow status will be set when tab is accessed")

    # Workflow UI is now initialized centrally by the main server
    # This initialization was causing conflicts with the centralized workflow management

    # Remove the automatic title update on input change

    observe({
      updateDateInput(session, "report_date", value = project_data$project_info$report_date)
      updateTextAreaInput(session, "project_description", value = project_data$project_info$description)
    })

    # Reset details_saved to FALSE when any input changes (except the save button)
    # Also set project_data$project_info$report_date reactively
    observeEvent(list(input$project_title, input$project_description, input$report_formats, input$project_keywords, input$report_date),
      {
        details_saved(FALSE)
        project_data$project_info$report_date <- tryCatch(
          {
            if (!is.null(input$report_date) && !is.na(input$report_date)) {
              as.character(as.Date(input$report_date))
            } else {
              ""
            }
          },
          error = function(e) {
            ""
          }
        )
      },
      ignoreInit = TRUE
    )
    # This observer has been moved and combined with form validation above

    # Create a reactive value to trigger UI updates
    trigger_ui_update <- reactiveVal(0)

    # Observer for the initiate project button
    # observeEvent(input$initiate_project, {
    #   logger::log_info("=== Initiate Project Button Clicked ===")
    #   logger::log_info("Input report_date: ", input$report_date)
    #
    #   logger::log_info("About to update project title...")
    #   # Update the project title
    #   project_data$project_info$title <- if (!is.null(input$project_title)) input$project_title else ""
    #   logger::log_info("Project title updated successfully")
    #
    #   logger::log_info("About to update project report date...")
    #   # Update the project report date
    #   project_data$project_info$report_date <- tryCatch(
    #     {
    #       if (!is.null(input$report_date) && !is.na(input$report_date)) {
    #         date_val <- as.character(as.Date(input$report_date))
    #         logger::log_info("Setting report date to: ", date_val)
    #         date_val
    #       } else {
    #         logger::log_info("No report date set - using empty string")
    #         ""
    #       }
    #     },
    #     error = function(e) {
    #       logger::log_error("Error setting report date: ", conditionMessage(e))
    #       ""
    #     }
    #   )
    #   logger::log_info("Project report date updated successfully")
    #
    #   logger::log_info("About to update report formats...")
    #   # Update report formats
    #   project_data$project_info$report_formats <- if (!is.null(input$report_formats)) input$report_formats else character(0)
    #   logger::log_info("Report formats updated successfully")
    #
    #   # Log the current state for debugging
    #   logger::log_info("=== After Save ===")
    #   logger::log_info("Project title: ", project_data$project_info$title)
    #   logger::log_info("Report date: ", project_data$project_info$report_date, " (class: ", class(project_data$project_info$report_date), ")")
    #   logger::log_info("Report formats: ", paste(project_data$project_info$report_formats, collapse = ", "))
    #
    #   logger::log_info("About to increment trigger_ui_update...")
    #   # Increment the trigger to force UI updates
    #   current_value <- trigger_ui_update()
    #   logger::log_info("Got current trigger_ui_update value: ", current_value)
    #   trigger_ui_update(current_value + 1)
    #   logger::log_info("Set new trigger_ui_update value successfully")
    #   logger::log_info("Trigger UI update count: ", trigger_ui_update())
    #
    #   # Force reactive updates with a small delay to ensure UI has time to update
    #   session$onFlushed(function() {
    #     invalidateLater(100, session)
    #   }, once = TRUE)
    #
    #   logger::log_info("About to set details_saved...")
    #   # Set details_saved to TRUE when the button is clicked
    #   details_saved(TRUE)
    #   logger::log_info("details_saved set successfully")
    #   logger::log_info("About to set editing_active...")
    #   logger::log_info("Setting editing_active to FALSE after details_saved set")
    #   editing_active(FALSE)
    #   logger::log_info("editing_active set successfully")
    #
    #   # Additional UI update trigger
    #   session$sendCustomMessage("force-update", list())
    #
    #   # Show progress bar
    #   session$sendCustomMessage("show-progress", list(
    #     id = ns("progress_container_1"),
    #     show = TRUE
    #   ))
    #
    #   logger::log_info("About to check required fields...")
    #   # Check if all required fields are filled when user clicks the button
    #   has_title_val <- !is.null(input$project_title) && nchar(trimws(input$project_title)) > 0
    #   logger::log_info("Title validation complete: ", has_title_val)
    #   has_date_val <- tryCatch(
    #     {
    #       !is.null(input$report_date) && !is.na(as.Date(input$report_date))
    #     },
    #     error = function(e) {
    #       logger::log_warn("Error checking date validity: {conditionMessage(e)}")
    #       FALSE # Assume invalid if there's an error
    #     }
    #   )
    #   logger::log_info("Date validation complete: ", has_date_val)
    #   has_formats_val <- !is.null(input$report_formats) && length(input$report_formats) > 0
    #   logger::log_info("Formats validation complete: ", has_formats_val)
    #
    #   logger::log_info("About to check if all fields are valid...")
    #   if (has_title_val && has_date_val && has_formats_val) {
    #     logger::log_info("All project setup fields completed - marking Project Setup as complete")
    #     logger::log_info("About to call update_workflow_step...")
    #     # update_workflow_step(
    #     #   ns_workflow,
    #     #   stage = "Project Setup",
    #     #   status = "complete",
    #     #   session = session,
    #     #   ns = ns
    #     # )
    #     logger::log_info("update_workflow_step completed successfully")
    #   } else {
    #     logger::log_warn("Cannot complete project setup - missing required fields")
    #   }
    #
    #   # Show save confirmation
    #   shinyToastify::showToast(
    #     session = session,
    #     input = input,
    #     text = "Project Details Saved",
    #     position = "top-center",
    #     transition = "slide",
    #     className = "status-line"
    #   )
    # })
    # })

    output$download_authors <- downloadHandler(
      filename = function() "authors.yml",
      content = function(file) {
        # Get the YAML content from the Ace editor with NULL check
        yaml_content <- if (!is.null(input$author_yaml_code)) input$author_yaml_code else ""
        # Write the content to the file
        writeLines(yaml_content, file)
      },
      contentType = "text/yaml"
    )

    # Download handler for branding YAML
    output$download_branding <- downloadHandler(
      filename = function() "_brand.yml",
      content = function(file) {
        # Get the YAML content from the Ace editor with NULL check
        yaml_content <- if (!is.null(input$branding_yaml_code)) input$branding_yaml_code else ""
        # Write the content to the file
        writeLines(yaml_content, file)

        # Update workflow step to complete
        logger::log_info("Calling update_workflow_state for Project Setup completion via branding YAML.")
        # update_workflow_state(ns_workflow = ns_workflow, session = session, stage = "Project Setup", status = "complete", ns = ns)
      },
      contentType = "text/yaml"
    )

    # Reactive value to store report formats
    report_formats <- reactiveVal(character(0))

    # Update report formats when save is clicked
    observeEvent(input$initiate_project, {
      if (!is.null(input$report_formats)) {
        report_formats(input$report_formats)
      }
    })

    # Create a reactive value to store the icons UI
    report_icons_ui <- reactive({
      # Use the stored report formats instead of direct input
      formats <- report_formats()
      if (length(formats) == 0) {
        return(NULL)
      }

      # Map each format to a Font Awesome icon
      icons_map <- list(
        "html" = ph("file-html", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "pdf" = ph("file-pdf", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "docx" = ph("file-doc", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "pptx" = ph("file-ppt", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "latex" = ph("file-tsx", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "typst" = ph("file-ts", weight = "light", class = "sidebar-icon", style = "color: #ffffff;")
      )

      # Create a span for each selected icon
      spans <- lapply(formats, function(choice) {
        if (choice %in% names(icons_map)) {
          tags$span(icons_map[[choice]])
        }
      })
      tagList(Filter(Negate(is.null), spans))
    })

    # Create reactives to return project data
    project_title <- reactive({
      if (!is.null(input$project_title) && nchar(trimws(input$project_title)) > 0) {
        input$project_title
      } else {
        ""
      }
    })

    project_description <- reactive({
      if (!is.null(input$project_description) && nchar(trimws(input$project_description)) > 0) {
        input$project_description
      } else {
        ""
      }
    })
    
    project_keywords <- reactive({
      if (!is.null(input$project_keywords) && length(input$project_keywords) > 0) {
        input$project_keywords
      } else {
        character(0)
      }
    })

    project_report_formats <- reactive({
      if (!is.null(input$report_formats) && length(input$report_formats) > 0) {
        input$report_formats
      } else {
        character(0)
      }
    })

    # Author information reactives
    author_name <- reactive({
      if (!is.null(input$author_name) && nchar(trimws(input$author_name)) > 0) {
        input$author_name
      } else {
        ""
      }
    })

    author_affiliation <- reactive({
      if (!is.null(input$author_affiliation) && nchar(trimws(input$author_affiliation)) > 0) {
        input$author_affiliation
      } else {
        ""
      }
    })

    author_orcid <- reactive({
      if (!is.null(input$author_orcid) && nchar(trimws(input$author_orcid)) > 0) {
        input$author_orcid
      } else {
        ""
      }
    })

    author_email <- reactive({
      if (!is.null(input$author_email) && nchar(trimws(input$author_email)) > 0) {
        input$author_email
      } else {
        ""
      }
    })

    author_url <- reactive({
      if (!is.null(input$author_url) && nchar(trimws(input$author_url)) > 0) {
        input$author_url
      } else {
        ""
      }
    })

    # Combined author information
    author_info <- reactive({
      list(
        name = author_name(),
        affiliation = author_affiliation(),
        orcid = author_orcid(),
        email = author_email(),
        url = author_url()
      )
    })

    # Combined project information
    project_info <- reactive({
      list(
        title = project_title(),
        description = project_description(),
        report_date = project_report_date(),
        keywords = project_keywords(),
        report_formats = project_report_formats(),
        authors = author_info()
      )
    })

    # Observer to show toast when triggered
    observeEvent(input$show_toast, {
      logger::log_info("Showing toast notification")
      shinyToastify::showToast(
        session = session,
        input = input,
        text = "Project Details Saved",
        position = "top-center",
        transition = "slide",
        className = "status-line"
      )
    })

    # Return the reactive UI and data for use in the main app
    return(
      list(
        icons = report_icons_ui,
        # Individual project data
        title = reactive({
          if (is.reactive(project_title)) project_title() else project_title
        }),
        description = reactive({
          if (is.reactive(project_description)) project_description() else project_description
        }),
        report_date = reactive({
          if (shiny::is.reactivevalues(project_data) && 
              !is.null(project_data$project_info) && 
              !is.null(project_data$project_info$report_date)) {
            project_data$project_info$report_date
          } else {
            NULL
          }
        }),
        keywords = reactive({
          if (is.reactive(project_keywords)) project_keywords() else project_keywords
        }),
        report_formats = reactive({
          if (is.reactive(project_report_formats)) project_report_formats() else project_report_formats
        }),
        # Individual author data
        author_name = reactive({
          if (is.reactive(author_name)) author_name() else author_name
        }),
        author_affiliation = reactive({
          if (is.reactive(author_affiliation)) author_affiliation() else author_affiliation
        }),
        author_orcid = reactive({
          if (is.reactive(author_orcid)) author_orcid() else author_orcid
        }),
        author_email = reactive({
          if (is.reactive(author_email)) author_email() else author_email
        }),
        author_url = reactive({
          if (is.reactive(author_url)) author_url() else author_url
        }),
        # Combined data
        author_info = reactive({
          if (is.reactive(author_info)) author_info() else author_info
        }),
        project_info = reactive({
          if (is.reactive(project_info)) project_info() else project_info
        })
      )
    )
  })
}
