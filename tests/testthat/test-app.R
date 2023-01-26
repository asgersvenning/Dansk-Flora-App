library(learndfv)
library(shiny)

testServer(shinyApp(app_ui, app_server), {
  test1 <- session$setInputs(fetchSpecies = 1)
  if (!test1) stop("Couldn't fetch species in species server!")
  
  print("All good!")
})
