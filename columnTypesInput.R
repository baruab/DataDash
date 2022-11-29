

#### Column Types Modele 2 Begin ######

columnTypesInputUI <- function(id) {
  uiOutput(NS(id, 'columnTypes'))
}

columnTypesInputServer <- function(id, labelText, data) {
  
  stopifnot(!(is.null(data)))
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    output$columnTypes <- renderUI({
      thedata <- data_recoded()
      ns <- session$ns
      
      ui <- list()
      ui[[length(ui)+1]] <- p(labelText)
      for(i in 1:ncol(thedata)) {
        inputId <- paste0('coltype_', names(thedata)[i], collapse = ' ')
        
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
  
  data_recoded <- reactive({
    thedata <- getData()
    for(i in seq_len(ncol(thedata))) {
      inputId <- paste0('coltype_', names(thedata)[i])
      if(is.null(input[[inputId]])) {
        return(thedata)
      }
      if(class(thedata[,i,drop=TRUE]) != input[[inputId]]) {
        
        print(paste0('Data value=', thedata[,i,drop=TRUE]))
        
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
        } else if(input[[inputId]] == 'Date') {
          thedata[,i] <- as.Date(thedata[,i,drop=TRUE])
        }
      }
    }
    
    return(thedata)
  })  
  
  })
  
}





##### Module 2 End ############

