#' @importFrom shiny tagList
#' @importFrom htmltools htmlTemplate

app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    useShinyjs(),
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
  
  golem::add_resource_path("shinyjs", system.file("srcjs", package = "shinyjs"))
  
  tags$head(
    # HTML(
    # '<script type="text/javascript"> var canUpdateDifficulty = false; </script>
    # <script src="shared/jquery.js" type="text/javascript"></script>
    # <script src="shared/shiny.js" type="text/javascript"></script>
    # <script src="shinyjs/inject.js" type="text/javascript"></script>
    # <script src="www/accessory.js" type="text/javascript"></script>
    # <script src="www/main.js" type="text/javascript"></script>
    # <link rel="stylesheet" type="text/css" href="shiny.css"/>
    # <link rel="stylesheet" type="text/css" href="www/main.css"/>
    # <title> Artsøvelse til Dansk Flora Eksamen </title>'
    # ),
    # golem::activate_js(),
    useShinyjs(html = T),
    extendShinyjs(app_sys("main.js"),
                  functions = c("backgroundCol",
                                "addLoader",
                                "toggleElement",
                                "canUpdateDifficulty")),
    golem::favicon("sustainScapesFavIcon", ext = "png"),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    golem::bundle_resources(
      path = app_sys("app/www/"),
      app_title = "DFV Learning Tool"
    )
  )
}
