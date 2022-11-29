

#### Column Types Modele 2 Begin ######

columnTypesInputUI <- function(id) {
  uiOutput(NS(id, 'columnTypes'))
}

columnTypesInputServer <- function(id, labelText, data) {
  
  stopifnot(!(is.null(data)))
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    output$columnTypes <- renderUI({
      thedata <- data()
      ns <- session$ns
      
      ui <- list()
      ui[[length(ui)+1]] <- p(labelText)
      for(i in 1:ncol(thedata)) {
        inputId <- ns(paste0('coltype_', names(thedata)[i], collapse = ' '))
        
        ui[[length(ui)+i]] <- fluidRow(
          column(2, p(names(thedata)[i,drop=TRUE])),
          column(2, p(class(thedata[,i,drop=TRUE]))),
          column(4,  get_mcolumn_type(inputId = ns(inputId),
                                      type = class(thedata[,i]))) #,
          #column(3, conditionalPanel(paste0('input.', inputId, '=="factor"', collapse = ' '),
          #                           get_mfactor_levels(paste0('factor_', names(thedata)[i]),
          #                                              levels = unique(thedata[,i,drop=TRUE]))))
        )
        
      }
      
      do.call(wellPanel, ui)
      
    })
    
    mcol_types <- c('character', 'numeric', 'logical', 'factor', 'Date')
    
    get_mcolumn_type <- function(inputId, label = inputId, type = 'character') {
      selectInput(inputId = inputId,
                  label = label,
                  choices = mcol_types,
                  selected = type)
    }
    
    get_mfactor_levels <- function(inputId, label = inputId, levels = character()) {
      textInput(inputId = inputId,
                label = label,
                value = paste(levels, collapse = ', '))
    }
    
  })
#######   module2 observe begin #########


observe({
  
  thedata <- data()
  #  print(paste0(' Thedata row/col =  ', is.na(thedata)), ncol(thedata)) ##, nrow(thdata)) 
  
  for(i in 1:ncol(thedata)) {
    inputId <- paste0('coltype_', names(thedata)[i])
    
    print(paste0(' Names: Thedata =  ', names(thedata)[i]))
    print(paste0(' inputId =  ', i ," - ", inputId ))
    
   # req(input[[inputId]])
    if(class(thedata[,i,drop=TRUE]) != input[[inputId]]) {
      
      print(paste0(inputId, ' has changed to ', input[[inputId]]))
      #print(paste0('Data to be changed ', thedata[,i,drop=TRUE]))
      
      ##### For NUMERIC Data Type ##########
      if (input[[inputId]] == 'numeric') {
        print(paste0('Data value=', data()[,i,drop=TRUE]))
        
        thedata[,i] <- as.numeric(data()[,i,drop=TRUE])  
        print(paste0('New Class=', class(thedata[,i,drop=TRUE])))
      }
      
      ##### For DATE Date Type ##########
      if (input[[inputId]] == 'Date') {
        #  print(paste0('Date value=', dataset()[,i,drop=TRUE]))
        #   thedata[,i,drop=TRUE] <- as.Date(dataset()[,i,drop=TRUE]) 
        
        #Change date format 
        thedata[,i] = reactive({
          datas = data()$inputId #auxiliary vector 
          datas = ymd(datas)
          df = data() #auxiliary data frame
          df$inputId = datas
          as_tsibble(df, index = inputId)
        })
        
        print(paste0('New Class=', class(thedata[,i,drop=TRUE])))
        print(paste0('New Date value=', thedata[,i,drop=TRUE]))
      }
      
      #print(paste0( 'New class value=', thedata[,i,drop=TRUE]))
      
      # TODO: This is where the column conversion happen!
    }
  }
})
#######   module2 observe end #########   

  
}





##### Module 2 End ############

