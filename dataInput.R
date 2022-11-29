
##### DataFile Upload Module 3 Begin ############

dataInputFileUI <- function(id, label = "Excel/CSV file") {
  fileInput(NS(id, 'file'), label)
}

dataInputFileServer <- function(id) {
  
  moduleServer(id, 
               function(input, output, session) {
                 # The selected file, if any
                 userFile <- reactive({ 
                   ### If no file is selected, don't do anything
                   validate(need(input$file, message=FALSE))   
                   input$file
                 })
                 
                 dataframe <- reactive({  
                   if ( endsWith(userFile()$datapath, ".xlsx") || endsWith(userFile()$datapath, ".xls") ) { 
                     read_excel(userFile()$datapath, .name_repair = function(x) gsub("\\s+", "_", x), 1)
                   } else if (endsWith(userFile()$datapath, ".csv")) {
                     read_csv(userFile()$datapath)
                   }   
                   
                 })
                 
                 # return reactive frame
                 return (dataframe)
               }) 
}
##### Module 3 End ############
