ui <- shinydashboard::dashboardPage(
  title = "shinymgr: Developers Portal",
  shinydashboard::dashboardHeader(
    title = tags$div(
      tags$img(src = "shinymgr-hexsticker.png", height = "50px", width = "50px"),
      "shinymgr"
    )
  ),
  shinydashboard::dashboardSidebar(
    hr(),
    tags$h3("Developer Portal", style = "text-align:center"),
    hr(),
    # https://fontawesome.com/icons?d=gallery&m=free
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem(
        text = "Analysis (beta)", 
        tabName = "NewAnalysis", 
        icon = icon("chart-pie")
        ),
      shinydashboard::menuItem(
        text = "Reports (beta)", 
        tabName = "GenerateReports", 
        icon = icon("file-alt")
        ),
      shinydashboard::menuItem(
        text = "Developer Tools", 
        tabName = "DevTools", 
        icon = icon("wrench")
        )
    ) # end sidebarMenu
  ), # end dashboardSidebar

  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      
      # New Analysis
      shinydashboard::tabItem(
        tabName = "NewAnalysis",
        uiOutput("new_analysis")
      ), 
      
      # Generate Reports
      shinydashboard::tabItem(
        tabName = "GenerateReports",
        uiOutput("new_report")
      ), 

      # developer Tools
      shinydashboard::tabItem(
        tabName = "DevTools",
        tabsetPanel(
          id = "dev_tool_tabs",
          
          # builder goes in first tab
          tabPanel(
            title = "Build App",
            value = "build_tab",
            uiOutput("build_app")
          ),
          
          # database tab
          tabPanel(
           title = "Database",
           value = "shinymgr_db",
           uiOutput("my_db_output")
          ),
          
          # queries
          tabPanel(
            title = "Queries",
            value = "query_db",
            uiOutput("query_output")
          ),
          
          # tab for adding reports
          tabPanel(
            title = "Add Report",
            value = "add_report_tab",
            uiOutput("add_report_output")
          )
        ) # end tabsetPanel
      ) # end of builder page
    ) # end of tabItems
  ) # end of dashboardBody
) # end dashboard page
