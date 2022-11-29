hidden(
  fluidRow(
  id = "main",

  column(
    12,
    div(
      dashboardPage(
        dashboardHeader(title = "DataDash"),
        dashboardSidebar(
         
          sidebarMenu(
            menuItem("File Upload", tabName = "data_upload", icon=icon("fa-solid fa-file"), selected=TRUE),
            menuItem("Dashboard", tabName = "dashboard", icon=icon("line-chart")),
            menuItem("Source Code",  icon = icon("file-text-o"),
                     menuSubItem("global.R", tabName = "global", icon = icon("angle-right")),
                     menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                     menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
                     menuSubItem("main.R", tabName = "mainUI", icon = icon("angle-right")),
                     menuSubItem("dataInput.R", tabName = "dataInput", icon = icon("angle-right")),
                     menuSubItem("columnsInput.R", tabName = "columnsInput", icon = icon("angle-right")),
                     menuSubItem("columnTypesInput.R", tabName = "columnTypesInput", icon = icon("angle-right"))
            ),
            menuItem("About", tabName = "about", icon = icon("question"))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem("data_upload",
                    # Boxes need to be put in a row (or column)
                    fluidRow(
                      box( width = 12,
                        title = "Choose Excel/CSV File to Upload",
                        actionButton('upload', 'Upload') ),
                      
                       mainPanel(width=16 ) 
                    )
            ),
          ### Dashboard          
            tabItem("dashboard",
                    
                    ############
                    fluidRow(
                      column(width = 2,
                             fileInput("uploadFile", "Choose CSV/XLSX file")
                      ),
                      column(width = 1,
                             textInput(inputId = "range_begin", label="Begin Cell", value="A1")
                      ),column(width = 1,
                             textInput(inputId = "range_end", label="End Cell", value="G30")
                      ),
                      column(width = 3,
                             radioButtons("select_filter","Filter", c("Filter by Column-Row" = "ColRow", 
                                                          "Filter by row index" = "RowIndex"), inline=T),
                              sliderInput("row_index", "Row Slider",
                                         min = 1, max = 100, value = c(1, 50))
                      ),
                      column(width = 2, columnsInputUI("xcolumn")), #    uiOutput('xcolumns')),
                      column(width = 2, columnsInputUI("ycolumn"))
                    ),
                    fluidRow(
                      column(width=12, actionButton("showColumnTypes", "Show/Hide Column Types"),br(),
                      box(id = "myBox", uiOutput('columns'), width = 12))
                    ),
                   
                    # Show a plot of the generated distribution
                    tabsetPanel(
                      tabPanel(title = "All data",
                               DTOutput('excelDataTable')  
                    ), tabPanel(title = "GT data",
                                gt_output(outputId = 'gtDataTable')
                    ), tabPanel(title = "Summary data",
                              box(withSpinner(verbatimTextOutput("test1")), width=6),
                              box(withSpinner(verbatimTextOutput("test2")), width=6)
                   ), tabPanel(title = "Model data",
                              box(
                                withSpinner(verbatimTextOutput("Model")),
                                width = 6,
                                title = "Model Summary"
                              ),
                              box(
                                withSpinner(verbatimTextOutput("Model_new")),
                                width = 6,
                                title = "New Summary"
                              ),
                              box(withSpinner(uiOutput("results")), width = 6, title = "Results"),
                              box(withSpinner(verbatimTextOutput("test3")), width=6)
                    ), tabPanel(
                      "Plot",
                   
                      box(withSpinner(plotOutput("plot")), width = 6, title = "Plot"),
                     # box(withSpinner(plotOutput("scatterplot")), width = 6, title = "scatterPlot"),
                     #  box(withSpinner(plotOutput("plot1")), width = 6, title = "Plot1"),
                      #box(withSpinner(uiOutput("results")), width = 6, title = "Results"),
                      #box(withSpinner(plotOutput("Corr")), width = 6, title = "Corr"),
                      box(withSpinner(plotOutput("plot2")), width = 6, title = "Plot2")
                    )
                  )
            ),
            
            #############################################################
            tabItem(tabName = "global",
                    box(width = NULL, status = "primary", solidHeader = TRUE, title= "global.R",
                        downloadButton('downloadData1', 'Download'),
                        br(),br(),
                        pre(includeText("global.R"))
                        
                    )
            ),
            tabItem(tabName = "ui",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
                         downloadButton('downloadData2', 'Download'),
                         br(),br(),
                         pre(includeText("ui.R"))
                    )
            ),
            tabItem(tabName = "server",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
                         downloadButton('downloadData3', 'Download'),
                         br(),br(),
                         pre(includeText("server.R"))
                    )
            ),
            tabItem(tabName = "mainUI",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="main.R",
                         downloadButton('downloadData4', 'Download'),
                         br(),br(),
                         pre(includeText("ui/main.R"))
                    )
            ),
            tabItem(tabName = "dataInput",
                  box( width = NULL, status = "primary", solidHeader = TRUE, title="dataInput.R",
                       downloadButton('downloadData5', 'Download'),
                       br(),br(),
                       pre(includeText("dataInput.R"))
                  )
            ),
            tabItem(tabName = "columnsInput",
                  box( width = NULL, status = "primary", solidHeader = TRUE, title="columnsInput.R",
                       downloadButton('downloadData6', 'Download'),
                       br(),br(),
                       pre(includeText("columnsInput.R"))
                  )
            ),
            tabItem(tabName = "columnTypesInput",
                  box( width = NULL, status = "primary", solidHeader = TRUE, title="columnTypesInput.R",
                       downloadButton('downloadData7', 'Download'),
                       br(),br(),
                       pre(includeText("columnTypesInput.R"))
                  )
            ),
            tabItem(tabName = "about",
                    fluidPage(
                      tags$iframe(src = "DataDashabstract.html",
                                  width = '100%', height = '800px', 
                                 frameborder = 0, scrolling = 'auto'
                       )
                    )
            )
            ########################################
          )
        )
      )
    )
  )
))
