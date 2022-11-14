#' @importFrom shiny tagList
#' @importFrom htmltools htmlTemplate

app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    htmlTemplate(
      app_sys("app/www/index.html"),
      habitats = speciesToolUI("habitatUI"),
      species = habitatToolUI("speciesUI"),
      # full = T,
      # escape_header = T
    # )
  ))
}



#' @import shiny
#' @import shinyjs
golem_add_external_resources <- function(){
  
  golem::add_resource_path('www', app_sys('app/www'))
  
  # golem::favicon("www/sustainScapesFavIcon", ext = "svg")
  
  golem::add_resource_path("shinyjs", system.file("srcjs", package = "shinyjs"))
  # shinyjs::useShinyjs()
  
  tags$head(
    # HTML(
    # '<script src="shinyjs/inject.js" type="text/javascript"></script>
    # <script type="text/javascript"> var canUpdateDifficulty = false; </script>
    # <script src="shared/jquery.js" type="text/javascript"></script>
    # <script src="shared/shiny.js" type="text/javascript"></script>
    # <script src="www/main.js" type="text/javascript"></script>
    # <script src="www/accessory.js" type="text/javascript"></script>
    # <link rel="stylesheet" type="text/css" href="shiny.css"/>
    # <link rel="stylesheet" type="text/css" href="www/main.css"/>
    # '# <title> Artsøvelse til Dansk Flora Eksamen </title>
    # ),
    # golem::activate_js(),
    # useShinyjs(html = T),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    tags$script(src="shinyjs/inject.js"),
    tags$script(src="shinyjs/shinyjs-default-funcs.js"),
    tags$script("var canUpdateDifficulty = false;"),
    tags$script(src="shared/jquery.js"),
    tags$script(src="www/main.js"),
    tags$script(src="www/accessory.js"),
    # tags$link(rel="stylesheet", type="text/css", href="shared/shiny.min.css"),
    tags$link(rel="stylesheet", type="text/css", href="www/main.css"),
    tags$link(rel="icon", type="image/x-icon", href="www/sustainScapesFavIcon.svg"),
    # Or for example, you can add shinyalert::useShinyalert() here
    golem::bundle_resources(
      path = app_sys("app/www/"),
      app_title = "DFV Learning Tool"
      # package = "learnDFV"
    )
  )
}
