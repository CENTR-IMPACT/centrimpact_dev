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
  
  # Render dynamics score display
  output$dynamics_score_display <- renderUI({
    has_score <- !is.null(modules$analyze$dynamics_score) && !is.null(modules$analyze$dynamics_score())
    score_value <- if (has_score) format(round(as.numeric(modules$analyze$dynamics_score()), 2), nsmall = 2) else "-.--"
    
    div(
      class = if (has_score) "has-score" else "no-score",
      style = if (!has_score) "color: #D3D3D3;" else "",
      div(
        class = "score-value",
        style = "font-family: 'Jersey 20'; font-weight: 700;",
        score_value
      )
    )
  })
  
  # Render cascade score display
  output$cascade_score_display <- renderUI({
    has_score <- !is.null(modules$analyze$cascade_score) && !is.null(modules$analyze$cascade_score())
    score_value <- if (has_score) format(round(as.numeric(modules$analyze$cascade_score()), 2), nsmall = 2) else "-.--"
    
    div(
      class = if (has_score) "has-score" else "no-score",
      style = if (!has_score) "color: #D3D3D3;" else "",
      div(
        class = "score-value",
        style = "font-family: 'Jersey 20'; font-weight: 700;",
        score_value
      )
    )
  })
  
  # Render alignment icon with conditional color
  output$alignment_icon <- renderUI({
    has_score <- !is.null(modules$analyze$alignment_score) && !is.null(modules$analyze$alignment_score())
    icon_style <- if (has_score) "font-size: 1em;" else "font-size: 1em; color: #D3D3D3;"
    ph("flower-lotus", weight = "regular", style = icon_style)
  })
  
  # Render dynamics icon with conditional color
  output$dynamics_icon <- renderUI({
    has_score <- !is.null(modules$analyze$dynamics_score) && !is.null(modules$analyze$dynamics_score())
    icon_style <- if (has_score) "font-size: 1em;" else "font-size: 1em; color: #D3D3D3;"
    ph("pulse", weight = "regular", style = icon_style)
  })
  
  # Render cascade icon with conditional color
  output$cascade_icon <- renderUI({
    has_score <- !is.null(modules$analyze$cascade_score) && !is.null(modules$analyze$cascade_score())
    icon_style <- if (has_score) "font-size: 1em;" else "font-size: 1em; color: #D3D3D3;"
    ph("waveform", weight = "regular", style = icon_style)
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
  
  # Initialize the workflow object
  ns_workflow <- reactiveValues(
    stage = "Not Started",
    alignment_data = NULL,
    dynamics_data = NULL,
    cascade_data = NULL,
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
  
  # Render alignment score display with default value
  output$alignment_score_display <- renderUI({
    has_score <- !is.null(modules$analyze$alignment_score) && !is.null(modules$analyze$alignment_score())
    score_value <- if (has_score) format(round(as.numeric(modules$analyze$alignment_score()), 2), nsmall = 2) else "-.--"
    
    div(
      class = if (has_score) "has-score" else "no-score",
      div(
        class = "score-value",
        score_value
      )
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
  analyze_initialized <- reactiveVal(FALSE)
  
  # Store module instances
  modules <- reactiveValues(
    load_clean = NULL,
    analyze = NULL
  )
  
  # Initialize the load_clean module first
  modules$load_clean <- mod_load_clean_server(
    id = "load_clean_1",
    ns_workflow = ns_workflow
  )
  
  # Initialize the analyze module with access to ns_workflow
  modules$analyze <- mod_analyze_server(
    id = "analyze_data_1",
    ns_workflow = ns_workflow
  )
  
  # Log the initialization
  message("Modules initialized successfully")
  analyze_initialized(TRUE)
  load_clean_initialized(TRUE)
  
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
          modules$load_clean <- mod_load_clean_server(
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
      
      # Handle Analyze Data tab
      if (input$main_navbar == "Analyze Data") {
        message("Analyze Data tab selected")
        logger::log_info("Analyze Data tab selected")
        
        # Just log the current status, module is already initialized at the top level
        logger::log_info("Analyze module initialization status: ", analyze_initialized())
        
        # Log the current alignment data status
        if (!is.null(modules$load_clean) && !is.null(modules$load_clean$rv$alignment)) {
          logger::log_info("Alignment data is available in load_clean module")
        } else {
          logger::log_warn("No alignment data available in load_clean module")
        }
      }
    }
  }, ignoreInit = TRUE)
  
  # Render the report icons in the main app
  output$report_output_icons <- renderUI({
    req(report_icons())
    report_icons()
  })
  
  # Observer for skip_overview toggle to switch tabs
  # Track the overview mode state
  overview_mode <- reactiveVal(FALSE)
  theme_mode <- reactiveVal("light")
  snapshot_mode <- reactiveVal(FALSE)
  
  # Toggle dark mode when the theme button is clicked
  observeEvent(input$toggle_theme, {
    if(theme_mode() == "light") {
      theme_mode("dark")
    } else {
      theme_mode("light")
    }
    bslib::toggle_dark_mode(mode = theme_mode(), session = session)
  })

  
  # Render the theme indicator icon
  output$theme_indicator <- renderUI({
    if (theme_mode() == "dark") {
      ph("moon-stars", weight = "bold")
    } else {
      ph("sun", weight = "bold")
    }
  })
  
  # Render the overview indicator icon
  output$overview_indicator <- renderUI({
    if (isTRUE(overview_mode())) {
      ph("rocket-launch", weight = "bold")
    } else {
      ph("lighthouse", weight = "bold")
    }
  })
  
  output$snapshot_indicator <- renderUI({
    if (isTRUE(snapshot_mode())) {
      ph("disk", weight = "bold")
    } else {
      ph("camera", weight = "bold")
    }
  })
  
  observeEvent(input$overview_mode, {
    # Toggle the overview mode state
    overview_mode(!overview_mode())
    
    # Set the target tab based on the overview mode state
    target_tab <- if (isTRUE(overview_mode())) {
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
