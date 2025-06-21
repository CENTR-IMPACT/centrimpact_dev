source("global.R")

server <- function(input, output, session) {
  
  # call database module
  output$my_db_output <- renderUI({
    my_db_ui("my_db")
  })
  
  # make reactive connection object
  con <- reactiveVal({
    DBI::dbConnect(
      drv = RSQLite::SQLite(), 
      dbname = paste0(
        shinyMgrPath, 
        "/database/shinymgr.sqlite")
      )
  })
  
  # call server functions
  shiny::isolate({
    my_db_server("my_db", con)
    lapply(
      X = DBI::dbListTables(conn = con()), 
      FUN = function(X) {
        table_server(X, con)
      }
    ) 
  })

  # control the reactive db connection object 
  observeEvent(
    eventExpr = {
      input$dev_tool_tabs
      input$tabs
    }, 
    handlerExpr = {
      if (input$tabs == "DevTools" & input$dev_tool_tabs == "shinymgr_db") {
        # connecting to database
        con(
          DBI::dbConnect(
            drv = RSQLite::SQLite(), 
            dbname = paste0(shinyMgrPath, "/database/shinymgr.sqlite"))
          )

      } else {
        if (DBI::dbIsValid(con())) {
          # disconnecting
          DBI::dbDisconnect(con())
        } # end disconnecting if still connected
      } # end not being on database tab
    } # end handler expr
  ) # end observe event
  
  # disconnect from the database when the session is ended
  session$onSessionEnded(function(){
    shiny::isolate({
      # session ended
      if (DBI::dbIsValid(con())) {
        # disconnecting
        DBI::dbDisconnect(con())
      } # end disconnecting if still connected
    })
  })
  
  # also disconnect if session stops
  onStop(function(){
    shiny::isolate({
      # session stopped
      if (DBI::dbIsValid(con())) {
        # disconnecting
        DBI::dbDisconnect(con())
      } # end disconnecting if still connected
    })
  })
  
  # call the  new_analyses module ui -----------------------------
  output$new_analysis <- renderUI({
    new_analysis_ui("new_analysis")
  })
  
  new_analysis_server(
    id = "new_analysis", 
    tabSelect = reactive({input$tabs}), 
    shinyMgrPath = shinyMgrPath
    )
  
  # call the new_report module ui -----------------------------
  output$new_report <- renderUI({
    new_report_ui("new_report")
  })
  
  new_report_server(
    id = "new_report"
    )
  
  # call the buildApp module ui -----------------------------
  output$build_app <- renderUI({
    app_builder_ui("app_builder")
  })

  reset_builder <- app_builder_server(
    'app_builder',
    shinyMgrPath = shinyMgrPath
  )

  observeEvent(reset_builder$reset, {
    if (reset_builder$reset()) {
      # remove and re-insert builder tab
      output$build_app <- renderUI({
        app_builder_ui("app_builder")
      })
    }
  })
  
  # call the add_report module ui and server ----------
  output$add_report_output <- renderUI({
    add_report_ui("add_report")
  })
  
  add_report_server(
    id = "add_report", 
    shinyMgrPath = shinyMgrPath
  )
  
  #call the query module ui and server -----------
  output$query_output <- renderUI({
    queries_ui("queries")
  })
  queries_server(
    id = "queries",
    shinyMgrPath = shinyMgrPath
  )
  
  # Define Reactive Variables
  ns_workflow <- reactiveValues(
    workflow_step = "pre-init",
    workflow_status = "not started"
  )
  ns_project <- reactiveValues(
    project_title = NULL,
    project_description = NULL,
    project_report_date = NULL,
    project_report_outputs = list(
      pdf = NULL,
      html = NULL,
      word = NULL
    )
  )
  ns_authors <- reactiveValues(
    author_list = data.frame(
      author_name = character(),
      author_email = character(),
      author_orcid = character(),
      author_institution = character(),
      author_role = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Render project info text in sidebar
  output$project_info_text <- renderUI({
    if (!is.null(ns_project$project_title) && ns_project$project_title != "") {
      session$sendCustomMessage("project-info-text", list(color = "#4A4A4A"))
      ns_project$project_title
    } else {
      session$sendCustomMessage("project-info-text", list(color = "#D3D3D3"))
      "Project Not Initiated"
    }
  })
  
  output$project_info_date <- renderUI({
    if (!is.null(ns_project$project_report_date)) {
      session$sendCustomMessage("project-info-date", list(color = "#4A4A4A"))
      ns_project$project_report_date
    } else {
      session$sendCustomMessage("project-info-date", list(color = "#D3D3D3"))
      "----------"
    }
  })

  output$workflow_step <- renderUI({
    step_text <- if (is.null(ns_workflow$workflow_step) || ns_workflow$workflow_step == "") {
      "Project not initiated"
    } else {
      ns_workflow$workflow_step
    }
    
    # Apply consistent styling
    tags$div(
      style = "color: #4A4A4A; text-transform: uppercase; font-family: var(--bs-font-code); font-size: 0.75em; text-align: center; margin-top: 8px;",
      step_text
    )
  })
  
  output$workflow_stage <- renderUI({
    stage_text <- if (is.null(ns_workflow$stage) || ns_workflow$stage == "") {
      "Status: Not Started"
    } else {
      ns_workflow$stage
    }
    
    # Style to match workflow_step but with a different color
    tags$div(
      style = "color: #888888; text-transform: uppercase; font-family: var(--bs-font-sans-serif); font-size: 0.7em; text-align: center; margin-top: 2px;",
      stage_text
    )
  })
  
  
    # Track which modules have been initialized
  setup_initialized <- reactiveVal(FALSE)
  load_clean_initialized <- reactiveVal(FALSE)
  
  # Track workflow updates to prevent multiple updates
  workflow_updates <- reactiveValues(
    project_setup_initiated = FALSE,
    load_data_initiated = FALSE,
    clean_data_initiated = FALSE
  )
  
  # Reactive value to store the report icons UI
  report_icons <- reactiveVal(NULL)
  
  # Single consolidated observer for main_navbar changes
  observeEvent(input$main_navbar, {
    req(input$main_navbar)
    
    logger::log_info("Main tab changed to:", input$main_navbar)
    
    # Handle Project Setup tab
    if (input$main_navbar == "Project Setup") {
      logger::log_info("Project Setup tab selected")
      
      # Initialize Project Setup module if not already done
      if (!setup_initialized()) {
        logger::log_info("Initializing Project Setup module")
        # Store the module's server function output
        setup_module <- mod_setup_server(
          id = "setup_1",
          ns_authors = ns_authors,
          ns_project = ns_project,
          ns_workflow = ns_workflow
        )
        
        # Update the report_icons reactive value when it changes
        observe({
          if (!is.null(setup_module$report_icons_ui())) {
            report_icons(setup_module$report_icons_ui())
          }
        })
        
        # Mark setup as initialized
        setup_initialized(TRUE)
      }
      
      # Update workflow step if not already done
      if (!workflow_updates$project_setup_initiated) {
        logger::log_info("Updating Project Setup workflow to initiated")
        update_workflow_step(
          ns_workflow,
          step = "Project Setup",
          stage = "initiated",
          session = session
        )
        workflow_updates$project_setup_initiated <- TRUE
      }
    }
    
    # Handle Upload Data tab
    if (input$main_navbar == "Upload Data") {
      logger::log_info("Upload Data tab selected")
      
      # Initialize Load Data module if not already done
      if (!load_clean_initialized()) {
        tryCatch({
          logger::log_info("Initializing Load Data module...")
          # Initialize the load_clean module with ns_workflow
          load_clean_module <- mod_load_clean_server(
            id = "load_clean_1",
            ns_workflow = ns_workflow
          )
          load_clean_initialized(TRUE)
          logger::log_info("Load Data module initialized successfully")
        }, error = function(e) {
          logger::log_error("Failed to initialize Load Data module: {conditionMessage(e)}")
          showNotification("Failed to initialize Load Data module. Please check the logs.", 
                          type = "error", duration = 10)
        })
      }
      
      # Update workflow steps if not already done
      if (!workflow_updates$load_data_initiated) {
        logger::log_info("Updating Upload Data workflow to initiated")
        update_workflow_step(
          ns_workflow,
          step = "Upload Data",
          stage = "initiated",
          session = session
        )
        workflow_updates$load_data_initiated <- TRUE
      }
      
      if (!workflow_updates$clean_data_initiated) {
        logger::log_info("Updating Clean Data workflow to initiated")
        update_workflow_step(
          ns_workflow,
          step = "Clean Data",
          stage = "initiated",
          session = session
        )
        workflow_updates$clean_data_initiated <- TRUE
      }
    }
  }, ignoreInit = TRUE)
  
  # Render the report icons in the main app
  output$report_output_icons <- renderUI({
    req(report_icons())
    report_icons()
  })
  
  # Observer for skip_overview toggle to switch tabs
  observeEvent(input$skip_overview, {
    # Set the target tab based on the toggle state
    target_tab <- if (isTRUE(input$skip_overview)) {
      list(
        "setup_1-setup_navbar" = "project details",
        "load_clean_1-load_clean_tabs" = "load_clean_panel",
        "analyze_data_1-analyze_tabs" = "Analyze Data",
        "visualize_1-visualize_tabs" = "Visualize Data",
        "generate_1-generate_tabs" = "Generate Report"
      )
    } else {
      list(
        "setup_1-setup_navbar" = "overview",
        "load_clean_1-load_clean_tabs" = "Overview",
        "analyze_data_1-analyze_tabs" = "Overview",
        "visualize_1-visualize_tabs" = "Overview",
        "generate_1-generate_tabs" = "Overview"
      )
    }
    
    # Update each navset if it exists
    for (nav_id in names(target_tab)) {
      # Extract module ID and tab ID
      ids <- strsplit(nav_id, "-")[[1]]
      module_id <- ids[1]
      tab_id <- ids[2]
      
      # Construct the namespaced input ID
      ns_tab_id <- paste0(module_id, "-", tab_id)
      
      # Check if the tab exists and is different from target
      if (!is.null(input[[ns_tab_id]]) && input[[ns_tab_id]] != target_tab[[nav_id]]) {
        # Update the tab
        updateTabsetPanel(
          session = session,
          inputId = ns_tab_id,
          selected = target_tab[[nav_id]]
        )
      }
    }
  })
  
} # end of server function
