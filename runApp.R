# Run app locally

library(shiny)

runApp(launch.browser = T)


# # Deploy app
# 
# rsconnect::deployApp(
#   appDir = "C:/Users/asger/Documents/Tilfældigheder/DanskFloraApp",
#   appFiles = c("server.R",
#                "ui.R",
#                "observations.rds",
#                "habitatPools.rds",
#                ".secrets/",
#                "www/"),
#   appTitle = "Læringsredskab - Dansk Flora og Vegetationsøkologi",
#   upload = T,
#   lint = T # Autodebug on upload?
# )
