fluidPage(
  shiny::singleton(
    shiny::tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$link(rel = "stylesheet", href = "snackbar.css"),
      tags$script(src="snackbar.js")
    )
  ),

  # load shinyjs on
  shinyjs::useShinyjs(),
  source("ui/main.R", local = TRUE)$value
)
