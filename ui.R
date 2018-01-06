dashboardPage(
  skin = "yellow",
  
  dashboardHeader(title = "Portfolio Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Snapshot", icon = icon("dashboard"), tabName = "snapshot"),
      menuItem("Metric Explorer", tabName = "metric_explorer", icon = icon("map")),
      menuItem("About", tabName = "about", icon = icon("plus")),
      hr(),
      div(
        style = "padding: 10px; width = 200px;",
        strong(p(style = "color:white;", "Lawrence Kurniawan Wong")),
        p(style = "color:white;", "lawrencekurniawan@gmail.com")
      )
    )
  ),
  
  dashboardBody(
      #########
      # SNAPSHOT
      ######### 
    tabItems(
      tabItem(tabName = "snapshot",
              
              fluidRow(
                tabBox(title = "Snapshot", 
                       side = "right",
                       width = 12,
                       tabPanel("Options",
                             fluidRow(
                               column(width = 4, uiOutput('snap_select_metric')),
                               column(width = 4, uiOutput('snap_select_instrument')),
                               column(width = 4, uiOutput('snap_select_date')))
                        ),
                        tabPanel("Download"
                                 , p("Click the button below to get the raw data from currently shown information.")
                                 , p("If you get an error, please make sure you have clicked 'Go' button in the 'Options' tab.")
                                 , downloadButton("snapshot_download", "Download")
                        ),
                        tabPanel("About",
                                 p(paste("Last updated:", with_tz(file.mtime(paste0(savepath, 'portfolio.rds')), tzone = "Asia/Jakarta"), "GMT+7"))
                        )
                )
              ),
              
              fluidRow(
                box(title = "Portfolio Snapshot", width = 12, collapsible = TRUE, collapsed = FALSE, 
                    div(style = 'overflow-x: scroll', DT::dataTableOutput("snap_table"))
                )
              )
      ),
      
      #######
      ####### METRIC EXPLORER
      #######
      
      tabItem(tabName = "metric_explorer",
              
              fluidRow(
                tabBox(title = "Metric Explorer", 
                       side = "right",
                       width = 12,
                       tabPanel("Options",
                                fluidRow(
                                  column(width = 4, uiOutput('me_select_metric')),
                                  column(width = 4, uiOutput('me_select_instrument')),
                                  column(width = 4, uiOutput('me_select_date'))
                                )
                       ),
                       tabPanel("Download"
                                , p("Click the button below to get the raw data from currently shown information in Table View.")
                                , p("If you get an error, please make sure you have clicked 'Go' button in the 'Options' tab.")
                                , downloadButton("metric_explorer_download", "Download")
                       ),
                       tabPanel("About",
                                p(paste("Last updated:", with_tz(file.mtime(paste0(savepath, 'portfolio.rds')), tzone = "Asia/Jakarta"), "GMT+7"))
                       )
                )
              ),
              fluidRow(
                box(title = "Chart View"
                    , width = 12
                    , collapsible = T
                    , collapsed = F
                    , highchartOutput("chart_maker"))
              ),
              fluidRow(
                box(title = "Table View"
                    , width = 12
                    , collapsible = T
                    , collapsed = F
                    , div(style = 'overflow-x: scroll', 
                          DT::dataTableOutput("me_table")))
              )
      ),
      
      #######
      ####### ABOUT
      #######
  
      tabItem(tabName = "about",
              
              h2("About"),
              fluidRow(
                box(width = 12
                    , p("This dashboard is a simplified version of the actual dashboard I made at work and only contains dummy data. The aim of this Shiny dashboard is to help non-technical people get the data they need, quickly."
                        , br()
                        , a("My Github (all of my projects are in private repositories)", href = "https://github.com/lawrencekurniawan")
                        , br()
                        , "I've been coding in R for work since March 2017 but have only really used Github since November 2017."
                      )
                )
              )
      )
    )
  )
  )
