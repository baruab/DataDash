

columnsInputUI <- function(id) {
  uiOutput(NS(id, 'columns'))
}

columnsInputServer <- function(id, label, data ,multiple) {
  
  stopifnot(!(is.null(data)))
  #  stopifnot(is.reactive(data))
  stopifnot(isTRUE(multiple) || isFALSE(multiple))
  
  moduleServer(id, function(input, output, session) {
    output$columns <-  renderUI({ 
      selectInput(id, 
                  label = label,
                  choices = names(data()),
                  multiple = multiple,
                  selected = NULL)
    })
  }) ### end moduleServer
} ### end columnsServer