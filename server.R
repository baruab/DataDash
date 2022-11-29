

function(input, output, session) {
   
    shinyjs::show("main")
  
 ##### MODAL LOGIC ###################################
  
  modal_page <- reactiveVal(1)
  
  observeEvent(input$next_page, {
    modal_page(modal_page() + 1)
    if(modal_page() > 4) {
      modal_page(1)
      removeModal()
    }
  })
  
   
  #getData <- reactive({
  #  inFile <- req(input$m_uploadFile) 
  #  if ( endsWith(inFile$datapath, ".xlsx") || endsWith(inFile$datapath, ".xls") ) { 
  #      read_excel(inFile$datapath, .name_repair = function(x) gsub("\\s+", "_", x), 1)
  #  } else if (endsWith(inFile$datapath, ".csv")) {
  #     read_csv(inFile$datapath)
  #  }
  #})
  
  getData <- dataInputFileServer("m_datafile")
  
  
  
  #### Column Types Modele 2 Begin ######
  
  columnTypesInputUI <- function(id) {
    uiOutput(NS(id, 'columnTypes'))
  }
  
  columnTypesInputServer <- function(id, labelText, data) {
    
    stopifnot(!(is.null(data)))
    stopifnot(is.reactive(data))
    firstTime <- TRUE
    thedata <- NULL
    
    
    moduleServer(id, function(input, output, session) {
      
      output$columnTypes <- renderUI({
        thedata <- data_recoded(thedata)
        ns <- session$ns
        
        ui <- list()
        ui[[length(ui)+1]] <- p(labelText)
        for(i in  seq_len(ncol(thedata))) {
          inputId <- paste0('coltype_', names(thedata)[i], collapse = ' ')
          # print(paste0('~~ InputID =', inputId))
          
          options_ui <- NULL
          ui[[length(ui)+i]] <- fluidRow(
            column(2, p(names(thedata)[i,drop=TRUE])),
            column(2, p(class(thedata[,i,drop=TRUE]))),
            column(3, get_mcolumn_type(inputId = ns(inputId),
                                          type = class(thedata[,i]))),
            column(3, conditionalPanel(paste0('input.', inputId, '=="factor"', collapse = ' '),
                                       get_mfactor_levels(paste0('factor_', names(thedata)[i]),
                                                          levels = unique(thedata[,i,drop=TRUE]))))
          )
          
        }
        
        do.call(wellPanel, ui)
        
      })
      
      mcol_types <- c('character', 'numeric', 'logical', 'factor', 'Date')
      
     
      get_mcolumn_type <- function(inputId, label = inputId, type = 'character') {
        selectInput(inputId = inputId,
                    label = label,
                    choices = col_types,
                    selected = type)
      }
      
      get_mfactor_levels <- function(inputId, label = inputId, levels = character()) {
        textInput(inputId = inputId,
                  label = label,
                  value = paste(levels, collapse = ', '))
      }
      
      
    
      data_recoded <-  function(thedata) {
        if (is.null(thedata)) {
          thedata <- data()
        }
        reactive({
        for(i in seq_len(ncol(thedata))) {
          inputId <- paste0('coltype_', names(thedata)[i])
          if(is.null(input[[inputId]])) {
            return(thedata)
          }
          # print(paste0('Class0 value=', class(thedata[,i,drop=TRUE])))
          print(paste0('Input Class name=', class(input[[inputId]])))
          
          if(class(thedata[,i,drop=TRUE]) != input[[inputId]]) {
            
            #   print(paste0('Class1 value=', class(thedata[,i,drop=TRUE])))
            #    print(paste0('Other Class value=',  input[[inputId]]))
            
            # This is where the column conversion happens!
            if(input[[inputId]] == 'character') {
              thedata[,i] <- as.character(thedata[,i,drop=TRUE])
            } else if(input[[inputId]] == 'numeric') {
              thedata[,i] <- as.numeric(thedata[,i,drop=TRUE])
            } else if(input[[inputId]] == 'integer') {
              thedata[,i] <- as.integer(thedata[,i,drop=TRUE])
            } else if(input[[inputId]] == 'logical') {
              thedata[,i] <- as.logical(thedata[,i,drop=TRUE])
            } else if(input[[inputId]] == 'factor') {
              thedata[,i] <- as.factor(thedata[,i,drop=TRUE])
            } 
            #else if(input[[inputId]] == 'Date') {
            #  thedata[,i] <- as.Date(thedata[,i,drop=TRUE])
            #}
            else if (input[[inputId]] == 'Date') {
              print(paste0('Old Class=', class(thedata[,i,drop=TRUE])))
              print(paste0('Date value=', thedata[,i,drop=TRUE]))
              thedata[,i,drop=TRUE] <-   anydate(thedata[,i,drop=TRUE])  
              
              print(paste0('New Class=', class(thedata[,i,drop=TRUE])))
              print(paste0('New Date value=', thedata[,i,drop=TRUE]))
            }
            
          } ## end column type do not match
        }  ## end for loop 
        })
        
        return(thedata)
        
      } ## end data_recoded
        
    })  ## end module_server
    return (thedata)
  }
  
  ##### Module 2 End ############
  
  
 # columnsInputUI <- function(id) {
#    uiOutput(NS(id, 'columns'))
#  }
  
#  columnsInputServer <- function(id, label, data ,multiple) {
    
 #   stopifnot(!(is.null(data)))
    #  stopifnot(is.reactive(data))
  #  stopifnot(isTRUE(multiple) || isFALSE(multiple))
    
   # moduleServer(id, function(input, output, session) {
      
      ## Column Name
    #  output$columns <-  renderUI({ 
    #    selectInput(id, 
    #                label = label,
    #                choices = names(data()),
    #                multiple = multiple,
    #                selected = NULL)
    #  })
    #  
   # }) ### end moduleServer
  #} ### end columnsServer
  
  
  ##### MODAL UI ###################################
  
  output$modal_ui <- renderUI({
    input$next_page
    ui <- list()
    modal_page <- modal_page()
    if(modal_page == 1) {
      ui[[length(ui)+1]] <- p('Step 1')
      ui[[length(ui)+1]] <- dataInputFileUI("m_datafile", "User data file")
                            #fileInput("m_uploadFile", "Choose CSV/XLSX file")
     
    } else if(modal_page == 2) {
     
      ui[[length(ui)+1]] <- box(width = 16,  columnTypesInputUI("mm_columntypes"))  
      ui[[length(ui)+1]] <- renderDT(getData(), filter = "top", options = list(pageLength = 5, scrollX=TRUE, width='400px' ))
      
    } else if(modal_page == 3) {
      ui[[length(ui)+1]] <- columnsInputUI("mm_xcolumn")    
      ui[[length(ui)+1]] <- columnsInputUI("mm_ycolumn")    
     # ui[[length(ui)+1]] <- columnsInputUI("mm_zcolumn") 
    } else if(modal_page == 4) {
        ui[[length(ui)+1]] <-  plotOutput("dataplot") #  box(withSpinner(plotOutput("dataplot")), width = 6, title = "Plot")
    }
    
    
    do.call(mainPanel, ui)
  })
  
  ##### MODAL DIALOG ###################################
  
  observeEvent(input$upload, {
    showModal(modalDialog(
      title = 'File upload and data extraction...',
      
      size='l',
      box(width = 12,  uiOutput('modal_ui')),
      #uiOutput('modal_ui'),
      footer = mainPanel(actionButton('next_page', 'Next page'), modalButton('Cancel'))
    ))
  })
 
  mod_data <- columnTypesInputServer("mm_columntypes", "Column Types", getData)
  
  xvar <- columnsInputServer("mm_xcolumn", "Independent Variable", getData, FALSE)
  yvar <- columnsInputServer("mm_ycolumn", "Dependent Variable", getData, FALSE)
  zvar <- columnsInputServer("mm_zcolumn", "Other Variable", getData, FALSE)
  
  
  
  columnsInputServer("m_xcolumn", "MX-Variable", getData, FALSE)
  columnsInputServer("m_ycolumn", "MY-Variable", getData, FALSE)
  columnsInputServer("zcolumn", "MZ-Variable", getData, FALSE)
   
  
  # get variable type
  # * 3: all variables are numeric
  # * 2: two numeric variables
  # * 1: one numeric, one non-numeric variable
  # * 0: both non-numeric variables
  # * -1: only one variable provided
  variable_type <- reactive({
    if (input$mm_xcolumn != "NA")
      (is.numeric(getData()[[input$mm_xcolumn]]) * 100) + (is.numeric(getData()[[input$mm_ycolumn]])*10) + is.numeric(getData()[[input$mm_zcolumn]])
    else
      -1
  })
  
  
  ############################
  
  
  ###### Create plot  ##########
  output$dataplot <- renderPlot({
    num_xvars <- 0
   # print(paste0('is.null xcolumn =', is.null(input$mm_xcolumn)))
    #### IF X variable is NULL or no value selected
    if (is.null(input$mm_xcolumn)) {
      num_xvars <- 1
      # print(paste0('AAAA'))
      # only one variable: univariate plot
      p <- ggplot(getData(), aes_string(x = input$mm_ycolumn))
      
      if (is.numeric(getData()[[input$mm_ycolumn]]))
        p <- p + geom_histogram()
      else
        p <- p + geom_bar()
    } else {
        num_xvars <- 2
      #  print(paste0('x var ', getData()[[input$mm_xcolumn]]))
      #  print(paste0('y var ', getData()[[input$mm_ycolumn]]))
        
        if ( (is.numeric(getData()[[input$mm_xcolumn]])) && (is.numeric(getData()[[input$mm_ycolumn]])) ) {
          print(paste0('2222 '))
          # both numeric variables: scatterplot
          p <- ggplot(getData(), aes_string(x = input$mm_xcolumn, y = input$mm_ycolumn))
          p <- p + geom_point(alpha = 0.5) + geom_smooth()
        } else if (is.numeric(getData()[[input$mm_xcolumn]])) {
          print(paste0('3333 '))
          # only X is numeric var, one character var: boxplot
          p <- p <- ggplot(getData(), aes_string(x = input$mm_xcolumn, y = input$mm_ycolumn)) + 
            geom_boxplot()
        } else {
          print(paste0('4444'))
          # both X & Y are character variables: heatmap
          temp_df <- reactive(getData()[, c(input$mm_xcolumn, input$mm_ycolumn), drop = FALSE] %>%
                                group_by(across()) %>%
                                summarize(count = n())
          )
          p <- ggplot(temp_df(), 
                      mapping = aes_string(x = input$mm_xcolumn, y = input$mm_ycolumn, fill = "count")) +
            geom_tile() +
            scale_fill_gradient(low = "#e7e7fd", high = "#1111dd")
          
        }
      }
    
    ##else if (num_xvars >= 2) {
    #    print(paste0('55555 '))
    #    ##### IF 2 X-variables selected   
    #    if ( (is.numeric(InputDataset_model()[[input$xcolumn[1]]])) && (is.numeric(InputDataset_model()[[input$xcolumn[2]]])) ) {
    #      p <- ggplot(InputDataset_model(), aes(x = input$xcolumn[1], y = input$ycolumn, color = input$xcolumn[2])) +
    #        geom_jitter(width = .2)
    #    }
    #  }
    
    
    '+' <- function(e1, e2) {
      if (is.character(e1) | is.character(e2)) {
        paste0(e1, e2)
      } else {
        base::`+`(e1, e2)
      }
    }
    
    
    # add title
    if (  num_xvars == 1) {
      p <- p + labs(title = paste0(input$mm_ycolumn, " vs. ", input$mm_xcolumn))
    } else if (  num_xvars == 2) {
      p <- p + labs(title = paste0(input$mm_ycolumn, " vs. ", input$mm_xcolumn))
    } # else {
      #p <- p + labs(title = paste0("Distribution of ", input$ycolumn))
    #}
    
    # add styling
    p <- p + 
      theme_bw() +
      theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
            axis.title = element_text(size = rel(1.5)))
    
    print(p)
    
  }, height=400)
  
  ########################################
  
  
  
  
  ################ DASHBOARD ###################
  dataset<-reactive({ 
    inFile <- req(input$uploadFile) 
    if ( endsWith(inFile$datapath, ".xlsx") || endsWith(inFile$datapath, ".xls") ) { 
      if (req(input$select_filter) == "ColRow") { 
         dat <- read_excel(inFile$datapath, .name_repair = function(x) gsub("\\s+", "_", x), range = paste(input$range_begin,  input$range_end, sep = ":") , 1)
      } else {
        dat <- read_excel(inFile$datapath, .name_repair = function(x) gsub("\\s+", "_", x), range = cell_rows( min(input$row_index): max(input$row_index)), 1)
      }
    } else if (endsWith(inFile$datapath, ".csv")) {
      dat <- read_csv(inFile$datapath)
    }
    return (dat)
    
  })
  
  output$excelDataTable <- renderDT( dataset(), filter = "top", options = list(pageLength = 5, scrollX=TRUE) )

  output$gtDataTable <- render_gt(
      dataset() %>%  gt() ,
                            height = px(350),
                            width =  pct(100)
                          )
  
  output$test1 <-
    renderPrint(
      stargazer(
        dataset(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$test2 <-  renderPrint(summary(dataset()))
  
 # output$hist <- renderPlot(hist(sample()), res = 96)
  
 columnsInputServer("xcolumn", "X-Variable", dataset,TRUE)
  
  
#  output$xcolumns <- renderUI({ 
#      selectInput('xcolumn', 
#         label = "Select variables",
#         choices = names(dataset()),
#         multiple = TRUE,
#         selected = NULL)
#    })
 
 columnsInputServer("ycolumn", "Y-Variable", dataset, FALSE)

# output$test1 <- renderText({paste( input$xcolumn , '-', input$ycolumn ) })

  #output$ycolumns = renderUI({
  #  selectInput('ycolumn', 'Y-Data Columns', names(dataset()))
  #})
  
  
  # get plot type
  # * 2: both numeric variables
  # * 1: one numeric, one non-numeric variable
  # * 0: both non-numeric variables
  # * -1: only one variable provided
  plot_type <- reactive({
    
    print(paste0('is.null xcolumn =', is.null(input$xcolumn)))
    print(paste0('size xcolumn =',length(strsplit(input$xcolumn, " ")[[1]]) )) 
    print(paste0('xcolumn class=', class(input$xcolumn)))
    print(paste0('ycolumn class=', class(input$ycolumn)))
    
    print(paste('xcolumn  value1=', input$xcolumn[1]))
    print(paste('xcolumn  value2=', input$xcolumn[2]))
    
    print(paste('ycolumn  value=', input$ycolumn))
  #  print(paste0('xcolumn  data value=', InputDataset_model()[[input$xcolumn]]))
  #   print(paste0('ycolumn  data value=', InputDataset_model()[[input$ycolumn]]))
    
    if (!(is.null(input$xcolumn))) {
      print(paste0('size xcolumn =',str_count(input$xcolumn, "\\w+")))    #(dataset()[[input$xcolumn]] )))  
     ## print(paste0('is_quantitative xcolumn =',is_quantitative(dataset()[[input$xcolumn]] )))  
    }
    
    if (!is.null(input$ycolumn)) {
     # print(paste0('is_quantitative ycolumn =',is_quantitative(dataset()[[input$ycolumn]] )))  
    }
    if (!is.null(input$xcolumn)) {
      is.numeric(InputDataset_model()[[input$xcolumn]]) + is.numeric(InputDataset_model()[[input$ycolumn]])
    }
    else
      -1
  })
  
  # Code section for Linear Regression  
  # -----------------------------------
  InputDataset_model <- reactive({
    if (is.null(input$xcolumn)) {
      dt <- dataset()
    }
    else{
      dt <- dataset() %>% select(c(input$xcolumn), input$ycolumn)
    }
  })
  
  
  f <- reactive({
    as.formula(paste(input$ycolumn, "~.")) 
  })
    
  Linear_Model <- reactive({
    lm(f(), data = InputDataset_model())
  })
 
  output$test3 <-  renderPrint( paste0('InputDataset_model[,inputX]=', InputDataset_model()[,input$xcolumn][1]) )
#  output$test3 <-  renderPrint(paste(Linear_Model()[[1]]))
  
  
  cormat <- reactive({
    cor(InputDataset_model())
  #  round(cor(InputDataset_model()), 1)
  })
  
  output$Corr <-
    renderPlot(corrplot(
      cormat(),
      order = 'AOE'
  #    method = "number"
  ))
  
  
  output$Model <- renderPrint(summary(Linear_Model()))
  
  output$Model_new <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = "text",
        title = "Model Results",
        digits = 1,
        out = "table1.txt"
    )
  )
  
##############
  
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  output$results <- renderUI({
    y <- extract(input$ycolumn)
    x <- extract(input$xcolumn)
    fit <-Linear_Model()
    withMathJax(
      paste0(
        "Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
        ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
        ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3),
        ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3)
      )
    )
  })
  
   
  
  # Scatterplot output
  output$scatterplot <- renderPlotly({ 
    y <- extract(input$ycolumn)
    x <- extract(input$xcolumn[1])
    dat <- InputDataset_model()
    
    p <-  plot(InputDataset_model()[, 1], InputDataset_model()[, 2],
               main='Plot', xlab="x", ylab="y",xlim= 100, ylim=100)
  #  abline(Linear_Model(), col="red")
   # lines(lowess(dat[,y], dat[,x]), col="blue")
    plotly(p)
  })
  
  
  #######
  # Show/Hide column types when button is clicked.
  observeEvent(input$showColumnTypes, {
  
    if(input$showColumnTypes %% 2 == 1){
      shinyjs::hide(id = "myBox")
    }else{
      shinyjs::show(id = "myBox")
    }
  })

  
  col_types <- c('character', 'numeric', 'logical', 'factor', 'Date')
  
  get_column_type <- function(inputId, label = inputId, type = 'character') {
    selectInput(inputId = inputId,
                label = label,
                choices = col_types,
                selected = type)
  }
  
  get_factor_levels <- function(inputId, label = inputId, levels = character()) {
    textInput(inputId = inputId,
              label = label,
              value = paste(levels, collapse = ', '))
  }
  
  output$columns <- renderUI({
    thedata <- dataset()
    ui <- list()
    ui[[1]] <- fluidRow(
      box(title = "Date format",
          #INPUT
          selectInput ("date_format", h5("Date format"),
                       choices = list("ymd", "mdy", "dmy", "ydm")),
          h5("y = year, m = month, d = day"),
      )
    )
    
    for(i in 1:ncol(thedata)) {
      inputId <- paste0('coltype_', names(thedata)[i], collapse = ' ')
      
      ui[[i+1]] <- fluidRow(
        column(2, p(names(thedata)[i,drop=TRUE])),
        column(2, p(class(thedata[,i,drop=TRUE]))),
        column(3, get_column_type(inputId = inputId,
                                  type = class(thedata[,i]))),
        column(3, conditionalPanel(paste0('input.', inputId, '=="factor"', collapse = ' '),
                                  get_factor_levels(paste0('factor_', names(thedata)[i]),
                                 levels = unique(thedata[,i,drop=TRUE]))))
       )
      
    }
    do.call(wellPanel, ui)
  })
  
  observe({
    thedata <-dataset()
    for(i in 1:ncol(thedata)) {
      inputId <- paste0('coltype_', names(thedata)[i])
      req(input[[inputId]])
      if(class(thedata[,i,drop=TRUE]) != input[[inputId]]) {
         
  #      print(paste0(inputId, ' has changed to ', input[[inputId]]))
  #      print(paste0('Data to be changed ', thedata[,i,drop=TRUE]))
        
        if (input[[inputId]] == 'numeric') {
           print(paste0('Data value=', dataset()[,i,drop=TRUE]))
          
          thedata[,i] <- as.numeric(dataset()[,i,drop=TRUE])  
          print(paste0('New Class=', class(thedata[,i,drop=TRUE])))
        }
        if  (input[[inputId]] == 'factor') {
          print(paste0('Factor value=', dataset()[,i,drop=TRUE]))
          
          thedata[,i] <- as.factor(dataset()[,i,drop=TRUE])  
          print(paste0('New Class=', class(thedata[,i,drop=TRUE])))
        }
        if (input[[inputId]] == 'Date') {
            print(paste0('Date value=', dataset()[,i,drop=TRUE]))
            thedata[,i,drop=TRUE] <-   anydate(dataset()[,i,drop=TRUE])  # as.Date(dataset()[,i,drop=TRUE]) 
         
          print(paste0('New Class=', class(thedata[,i,drop=TRUE])))
          print(paste0('New Date value=', thedata[,i,drop=TRUE]))
        }
        
      }
    }
  })
  
  
  
  
  Fit <- reactive({
    (
      InputDataset_model() %>% na.omit() %>%
        #  boxplot(mpg ~ input$p , data=mtcars)
        boxplot(input$xcolumn[1] ~ input$ycolumn)  
    )
  })
  
  
  ###### Create plot2  ########## 
  output$plot1 <- renderPlot(Fit())
  
  output$plot2 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1)) # Change the panel layout
    plot(Linear_Model())
    abline(Linear_Model(), col="red", lwd=2)
  }) 
  
  #' Utility function that not only checks whether a variable is numeric but whether
  #' it has more than a certain number of unique values.
  is_qualitative <- function(x, category_threshold = 5) {
    !is.numeric(x) | length(unique(x)) <= category_threshold
  }
  
   
  
  ###### Create plot  ##########
  output$plot <- renderPlot({
    num_xvars <- 0
    print(paste0('is.null xcolumn =', is.null(input$xcolumn)))
    #print(paste0('size xcolumn =',length(strsplit(input$xcolumn, " ")[[1]]) )) 
    print(paste0('xcolumn class=', class(input$xcolumn)))
    print(paste0('ycolumn class=', class(input$ycolumn)))
    
    
    #### IF X variable is NULL or no value selected
    if (is.null(input$xcolumn)) {
      
      print(paste0('AAAA'))
        # only one variable: univariate plot
        p <- ggplot(InputDataset_model(), aes_string(x = input$ycolumn))
        
        if (is.numeric(InputDataset_model()[[input$ycolumn]]))
          p <- p + geom_histogram()
        else
          p <- p + geom_bar()
    } else {
      # IF X variable value is selected
      num_xvars <-  length(strsplit(input$xcolumn, " "))
      print(paste0('BBBB ', num_xvars))
      ##### IF only 1 X-variable selected
      if (num_xvars == 1) {
        
        print(paste0('1111 ', InputDataset_model()[[input$xcolumn]]))
        
        if ( (is_qualitative(InputDataset_model()[[input$xcolumn]])) && (is_qualitative(InputDataset_model()[[input$ycolumn]])) ) {
          print(paste0('2222 '))
          # both numeric variables: scatterplot
          p <- ggplot(InputDataset_model(), aes_string(x = input$xcolumn[1], y = input$ycolumn))
          p <- p + geom_point(alpha = 0.5) + geom_smooth()
        } else if (is.numeric(InputDataset_model()[[input$xcolumn]])) {
          print(paste0('3333 '))
          # only X is numeric var, one character var: boxplot
          p <- p <- ggplot(InputDataset_model(), aes_string(x = input$xcolumn[1], y = input$ycolumn)) + 
            geom_boxplot()
        } else {
          print(paste0('4444'))
          # both X & Y are character variables: heatmap
          temp_df <- reactive(InputDataset_model()[, c(input$xcolumn[1], input$ycolumn), drop = FALSE] %>%
                                group_by(across()) %>%
                                summarize(count = n())
          )
          p <- ggplot(temp_df(), 
                      mapping = aes_string(x = input$xcolumn[1], y = input$ycolumn, fill = "count")) +
            geom_tile() +
            scale_fill_gradient(low = "#e7e7fd", high = "#1111dd")
          
        }
      } else if (num_xvars >= 2) {
        print(paste0('55555 '))
           ##### IF 2 X-variables selected   
        if ( (is.numeric(InputDataset_model()[[input$xcolumn[1]]])) && (is.numeric(InputDataset_model()[[input$xcolumn[2]]])) ) {
          p <- ggplot(InputDataset_model(), aes(x = input$xcolumn[1], y = input$ycolumn, color = input$xcolumn[2])) +
            geom_jitter(width = .2)
        }
      }
    }
    
    '+' <- function(e1, e2) {
      if (is.character(e1) | is.character(e2)) {
        paste0(e1, e2)
      } else {
        base::`+`(e1, e2)
      }
    }
    
    
    # add title
   if (  num_xvars == 1) {
      p <- p + labs(title = paste0(input$ycolumn, " vs. ", input$xcolumn[1]))
    } else if (  num_xvars == 2) {
      p <- p + labs(title = paste0(input$ycolumn, " vs. ", input$xcolumn[1], " vs. ", input$xcolumn[2]))
    } else {
      p <- p + labs(title = paste0("Distribution of ", input$ycolumn))
    }
    
    # add styling
    p <- p + 
      theme_bw() +
      theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
            axis.title = element_text(size = rel(1.5)))
    
    print(p)
    
  }, height=400)

########################################

}
