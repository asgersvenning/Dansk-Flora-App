#' @import shiny
#' @import shinyjs
#' @importFrom htmltools htmlTemplate
app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    htmlTemplate(
      app_sys("app/www/index.html"),
      habitats = speciesToolUI("species"),
      species = habitatToolUI("habitats")
  ))
}

golem_add_external_resources <- function(){
  golem::add_resource_path('www', app_sys('app/www'))
  golem::add_resource_path("shinyjs", system.file("srcjs", package = "shinyjs"))
  
  tags$head(
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    tags$script(src="shinyjs/inject.js"),
    tags$script(src="shinyjs/shinyjs-default-funcs.js"),
    tags$script("var canUpdateDifficulty = false;"),
    tags$script(src="shared/jquery.js"),
    tags$script(src="www/main.js"),
    tags$script(src="www/accessory.js"),
    tags$link(rel="stylesheet", type="text/css", href="www/main.css"),
    tags$link(rel="icon", type="image/x-icon", href="www/sustainScapesFavIcon.svg"),
    # Or for example, you can add shinyalert::useShinyalert() here
    golem::bundle_resources(
      path = app_sys("app/www/"),
      app_title = "DFV Learning Tool"
    )
  )
}
