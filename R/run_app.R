#' run_app
#' @description Run the Shiny Application
#'
#' @param name a character. Name of the application shown in the browser tab.
#' @param time an object of class 'POSIXct'. Start-up time of the app.
#' @param port an integer. The network port on which the app is served.
#' @param test a character. "true" is interpreted as True while everything else is interpreted as False. If "true" don't start server, instead run a test.
#' @param ... additional parameters passed to 'with_golem_options'. 
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options

run_app <- function(
    name = "DFV Learning Tool", 
    time = Sys.time(), 
    port = 2811,
    test = "false",
    ...
) {
  if (test == "true") {
    test_app()
  }
  else {
  with_golem_options(
    app = shinyApp(ui = app_ui,
                   server = app_server,
                   options = list(port = port)),
    golem_opts = list(name = name, time = time),
    ...
  )
  }
}
