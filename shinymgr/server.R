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
        "/database/shinymgr.sqlite"
      )
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

  observeEvent(session$clientData$url_port, {
    nav_hide(id = "main_navbar", target = "Enter Data")
  }, once = TRUE)
  
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
            dbname = paste0(shinyMgrPath, "/database/shinymgr.sqlite")
          )
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
  session$onSessionEnded(function() {
    shiny::isolate({
      # session ended
      if (DBI::dbIsValid(con())) {
        # disconnecting
        DBI::dbDisconnect(con())
      } # end disconnecting if still connected
    })
  })

  # also disconnect if session stops
  onStop(function() {
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
    tabSelect = reactive({
      input$tabs
    }),
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
    # Check if modules and analyze module exist before accessing
    if (is.null(modules$analyze) || is.null(modules$analyze$dynamics_score)) {
      has_score <- FALSE
      score_value <- "-.--"
    } else {
      has_score <- !is.null(modules$analyze$dynamics_score)
      score_value <- if (has_score) format(round(as.numeric(modules$analyze$dynamics_score), 2), nsmall = 2) else "-.--"
    }

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
    # Check if modules and analyze module exist before accessing
    if (is.null(modules$analyze) || is.null(modules$analyze$cascade_score)) {
      has_score <- FALSE
      score_value <- "-.--"
    } else {
      has_score <- !is.null(modules$analyze$cascade_score)
      score_value <- if (has_score) format(round(as.numeric(modules$analyze$cascade_score), 2), nsmall = 2) else "-.--"
    }

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
    # Check if modules and analyze module exist before accessing
    if (is.null(modules$analyze) || is.null(modules$analyze$alignment_score)) {
      has_score <- FALSE
    } else {
      has_score <- !is.null(modules$analyze$alignment_score)
    }
    icon_style <- if (has_score) "font-size: 1em;" else "font-size: 1em; color: #D3D3D3;"
    ph("flower-lotus", weight = "regular", style = icon_style)
  })

  # Render dynamics icon with conditional color
  output$dynamics_icon <- renderUI({
    # Check if modules and analyze module exist before accessing
    if (is.null(modules$analyze) || is.null(modules$analyze$dynamics_score)) {
      has_score <- FALSE
    } else {
      has_score <- !is.null(modules$analyze$dynamics_score)
    }
    icon_style <- if (has_score) "font-size: 1em;" else "font-size: 1em; color: #D3D3D3;"
    ph("pulse", weight = "regular", style = icon_style)
  })

  # Render cascade icon with conditional color
  output$cascade_icon <- renderUI({
    # Check if modules and analyze module exist before accessing
    if (is.null(modules$analyze) || is.null(modules$analyze$cascade_score)) {
      has_score <- FALSE
    } else {
      has_score <- !is.null(modules$analyze$cascade_score)
    }
    icon_style <- if (has_score) "font-size: 1em;" else "font-size: 1em; color: #D3D3D3;"
    ph("waveform", weight = "regular", style = icon_style)
  })

  reset_builder <- app_builder_server(
    "app_builder",
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

  # call the query module ui and server -----------
  output$query_output <- renderUI({
    queries_ui("queries")
  })
  queries_server(
    id = "queries",
    shinyMgrPath = shinyMgrPath
  )

  # Initialize reactive values for app state
  data_entry_mode <- reactiveVal(FALSE) # FALSE = manual entry, TRUE = upload
  snapshot_mode <- reactiveVal(FALSE) # Add this if not already present

  # Initialize the workflow object with safe defaults
  ns_workflow <- reactiveValues(
    stage = "Project Setup",
    status = "not started",
    alignment_data = NULL,
    dynamics_data = NULL,
    cascade_data = NULL
  )

  # Log the initial workflow state for debugging
  logger::log_info("ns_workflow contains:")
  logger::log_info("- stage\n- status\n- alignment_data\n- dynamics_data\n- cascade_data")

  # Workflow icons are now initialized directly by update_workflow_step function
  # This initialization observer was causing reactive context errors
  # Removed to prevent conflicts with centralized workflow management

  # Workflow icons are now updated directly by update_workflow_step function
  # This observer was causing circular dependencies and reactive context errors
  # Removed to prevent conflicts with centralized workflow management
  ns_project <- reactiveValues(
    project_title = NULL,
    project_description = NULL,
    project_report_date = NULL,
    project_report_outputs = list(
      pdf = NULL,
      html = NULL,
      word = NULL
    ),
    # Add workflow variables from mod_load_clean.R
    main_data_raw = NULL,
    main_cleaned = NULL,
    alignment_data_raw = NULL,
    alignment_data = NULL,
    dynamics_data = NULL,
    cascade_data = NULL,
    params = NULL,
    edges = NULL,
    nodes = NULL,
    yaml_content = NULL,
    file_name = NULL,
    rv_status = NULL
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

  # Render project title in sidebar
  output$project_title_text <- renderUI({
    req(ns_project$project_title)
    div(ns_project$project_title, class = "title-style")
  })

  # Render project info text in sidebar
  # Note: These are now handled in the setup module with proper namespacing

  output$workflow_step <- renderUI({
    step_text <- if (is.null(ns_workflow$workflow_step) || ns_workflow$workflow_step == "") {
      "Project not started"
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
    # Check if modules and analyze module exist before accessing
    if (is.null(modules$analyze) || is.null(modules$analyze$alignment_score)) {
      has_score <- FALSE
      score_value <- "-.--"
    } else {
      has_score <- !is.null(modules$analyze$alignment_score)
      score_value <- if (has_score) format(round(as.numeric(modules$analyze$alignment_score), 2), nsmall = 2) else "-.--"
    }

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


  # Initialize project data structure
  project_data <- reactiveValues(
    # Project Information
    project_info = list(
      title = NULL,                   # character(1)
      description = NULL,             # character(1)
      authors = list(),               # list of author information
      report_formats = character(0),  # character vector of selected formats
      report_date = NULL,             # Date or character(1)
      report_keywords = NULL          # character vector
    ),
    
    # Cleaned Data
    cleaned_data = list(
      indicators = NULL,
      alignment = NULL,
      dynamics = NULL,
      cascade = NULL
    ),
    
    # Analysis Results
    analysis = list(
      alignment = NULL,
      dynamics = NULL,
      cascade = NULL
    ),
    
    # Visualization Objects (including ggplot2 objects)
    visualization = list(
      indicators = NULL,  # ggplot2 object
      alignment = NULL,   # ggplot2 object
      dynamics = NULL,    # ggplot2 object
      cascade = NULL      # ggplot2 object
    ),
    
    # Status Tracking
    status = list(
      setup_complete = FALSE,
      data_loaded = FALSE,
      analysis_complete = FALSE,
      visualization_complete = FALSE
    )
  )
  
  # Store module instances - initialize after setup module to avoid dependency issues
  modules <- reactiveValues(
    load_clean = NULL,
    analyze = NULL,
    visualize = NULL
  )

  # Track which modules have been initialized
  load_clean_initialized <- reactiveVal(FALSE)
  analyze_initialized <- reactiveVal(FALSE)

  # Track workflow updates to prevent multiple updates
  workflow_updates <- reactiveValues(
    project_setup_initiated = FALSE,
    load_data_initiated = FALSE
  )

  # Reactive value to store the report icons UI (unused - can be removed)
  # report_icons <- reactiveVal(NULL)

  # --- Project Setup Module: Call unconditionally at top-level ---
  setup_module <- mod_setup_server(
    id = "setup_1",
    project_data = project_data,
    ns_workflow = ns_workflow
  )


  # Wire up the returned reactives to sidebar outputs
  output$report_output_icons <- renderUI({
    req(setup_module$icons())
    setup_module$icons()
  })

  # Initialize other modules after setup module to avoid dependency issues
  observe({
    req(setup_module)
    if (!load_clean_initialized()) {
      logger::log_info("Initializing load_clean module...")
      modules$load_clean <- mod_load_clean_server(
        id = "load_clean_1",
        project_data = project_data
      )
      load_clean_initialized(TRUE)
      logger::log_info("load_clean module initialized successfully")
    }
  })

  observe({
    req(setup_module)
    if (!analyze_initialized()) {
      logger::log_info("Initializing analyze module...")
      modules$analyze <- mod_analyze_server(
        id = "analyze_data_1",
        project_data = project_data
      )
      analyze_initialized(TRUE)
      logger::log_info("analyze module initialized successfully")
    }
  })

  observe({
    req(setup_module)
    if (is.null(modules$visualize)) {
      logger::log_info("Initializing visualize module...")
      modules$visualize <- mod_visualize_server(
        id = "visualize_1",
        project_data = project_data
      )
      logger::log_info("visualize module initialized successfully")
    }
  })

  # Initialize generate module
  observe({
    req(setup_module)
    if (is.null(modules$generate)) {
      logger::log_info("Initializing generate module...")
      modules$generate <- mod_generate_server(
        id = "generate_1",
        project_data = project_data
      )
      logger::log_info("generate module initialized successfully")
    }
  })

  # Single consolidated observer for main_navbar changes
  observeEvent(input$main_navbar,
    {
      req(input$main_navbar)
      logger::log_info("Main tab changed to:", input$main_navbar)

      # Handle Project Setup tab
      if (input$main_navbar == "Project Setup") {
        # Update workflow step if not already done
        if (!workflow_updates$project_setup_initiated) {
          logger::log_info("Updating Project Setup workflow to in progress")
          update_workflow_step(
            ns_workflow,
            stage = "Project Setup",
            status = "in progress",
            session = session,
            ns = NULL
          )
          workflow_updates$project_setup_initiated <- TRUE
        }
      }

      # Handle Upload Data tab
      if (input$main_navbar == "Upload Data") {
        logger::log_info("Upload Data tab selected")

        # Update workflow steps if not already done
        if (!workflow_updates$load_data_initiated) {
          logger::log_info("Updating Upload Data workflow to in progress")
          update_workflow_step(
            ns_workflow,
            stage = "Upload Data",
            status = "in progress",
            session = session,
            ns = NULL
          )
          workflow_updates$load_data_initiated <- TRUE
        }
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
    },
    ignoreInit = TRUE
  )

  # Render the report icons in the main app
  # (Handled above with setup_module return values)

  # Observer for skip_overview toggle to switch tabs
  # Track the overview mode state
  overview_mode <- reactiveVal(FALSE)
  theme_mode <- reactiveVal("light")

  # Toggle dark mode when the theme button is clicked
  observeEvent(input$toggle_theme, {
    if (theme_mode() == "light") {
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
      ph("rocket-launch", weight = "light", class = "indicator-icon")
    } else {
      ph("lighthouse", weight = "light", class = "indicator-icon")
    }
  })

  # Render the data entry mode indicator icon
  output$data_entry_indicator <- renderUI({
    if (isTRUE(data_entry_mode())) {
      bsicons::bs_icon("input-cursor-text", class = "indicator-icon")
    } else {
      bsicons::bs_icon("cloud-plus", class = "indicator-icon")
    }
    # data_entry_mode(!data_entry_mode())
  })

  # Render the snapshot indicator icon
  output$snapshot_indicator <- renderUI({
    if (isTRUE(snapshot_mode())) {
      bsicons::bs_icon("bookmark-check", class = "indicator-icon")
    } else {
      ph("camera", weight = "light", class = "indicator-icon")
    }
  })

  output$snapshot_load_indicator <- renderUI({
      ph("camera-plus", weight = "light", class = "indicator-icon")
  })
  
  
  observeEvent(input$data_entry_mode, {
    # Toggle the data entry mode state
    data_entry_mode(!data_entry_mode())

    if (isTRUE(data_entry_mode())) {
      entry_mode <- "manual"
      nav_show(id = "main_navbar", target = "Enter Data")
      nav_hide(id = "main_navbar", target = "Upload Data")
    } else {
      entry_mode <- "upload"
      nav_hide(id = "main_navbar", target = "Enter Data")
      nav_show(id = "main_navbar", target = "Upload Data")
    }

    # Log the current data entry mode state
    logger::log_info("Data entry mode toggled to: ", entry_mode)
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

  # Initialize the main workflow UI on startup
  # Note: Workflow UI updates are now handled within individual modules
  # since the main server doesn't have access to module namespaces

  # Handle JavaScript warnings and log them properly
  observeEvent(input$js_warning, {
    req(input$js_warning)
    warning_data <- input$js_warning

    switch(warning_data$type,
      "element_not_found" = {
        logger::log_warn("JavaScript: Element '{warning_data$element}' not found in DOM")
      },
      "icon_not_found" = {
        logger::log_warn("JavaScript: Icon element not found in '{warning_data$element}'")
      },
      "workflow_icon_not_found" = {
        logger::log_warn("JavaScript: Workflow icon span '{warning_data$element}' not found in DOM")
      },
      {
        logger::log_warn("JavaScript: Unknown warning type '{warning_data$type}' for element '{warning_data$element}'")
      }
    )
  })

  # --- Workflow Icon Outputs ---
  output$workflow_setup_icon <- renderUI({
    ph("plant", weight = "light", class = "sidebar-icon")
  })
  output$workflow_upload_icon <- renderUI({
    is_complete <- !is.null(ns_workflow$upload_status) && ns_workflow$upload_status == "complete"
    weight <- if (is_complete) "fill" else "light"
    color <- if (is_complete) "#4CAF50" else "#d3d3d366"
    ph("upload", weight = weight, class = "sidebar-icon", style = paste0("color: ", color, ";"))
  })
  output$workflow_clean_icon <- renderUI({
    is_complete <- !is.null(ns_workflow$clean_status) && ns_workflow$clean_status == "complete" &&
      !is.null(ns_workflow$upload_status) && ns_workflow$upload_status == "complete"
    weight <- if (is_complete) "fill" else "light"
    color <- if (is_complete) "#4CAF50" else "#d3d3d366"
    ph("broom", weight = weight, class = "sidebar-icon", style = paste0("color: ", color, ";"))
  })
  output$workflow_analyze_icon <- renderUI({
    ph("calculator", weight = "light", class = "sidebar-icon")
  })
  output$workflow_visualize_icon <- renderUI({
    ph("blueprint", weight = "light", class = "sidebar-icon")
  })
  output$workflow_generate_icon <- renderUI({
    ph("newspaper-clipping", weight = "light", class = "sidebar-icon")
  })
} # end of server function
