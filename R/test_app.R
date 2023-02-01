#' test_app
#' @description Internal test function.
#' @noRd
#' 
#' @importFrom shiny shinyApp testServer

test_app <- function() {
  testServer(shinyApp(app_ui, app_server), {
    test1 <- session$setInputs(fetchSpecies = 1)
    if (!test1) stop("Couldn't fetch species in species server!")
    
    print("All good!")
  })
}
