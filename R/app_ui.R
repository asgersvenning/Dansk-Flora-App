#' @importFrom shiny tagList
#' @importFrom htmltools htmlTemplate

app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    htmlTemplate(
      "index.html",
      speciesToolUI("habitatUI"),
      habitatToolUI("speciesUI")
    )
  )
}



#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', 
    system.file('app/www', package = 'golemexample')
  )
  
  addResourcePath("shinyjs", 
                  system.file("srcjs", package = "shinyjs"))
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"), 
    tags$script(src="www/alertme.js"), 
    tags$script(src="www/handlers.js")
  )
}