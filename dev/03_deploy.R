# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check(vignettes = F)

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
### VERY IMPORTANT!! ###
### The argument 'path = "deploy"' ensures that the built package tar.gz file is saved in the correct location! ### 
devtools::build(path = "deploy", vignettes = F)

## Docker ----
## If you want to deploy via a generic Dockerfile
######## !! DO NOT RUN !! #########
## This is only kept as the dockerfile was initially created using the function below, however it has been manually edited!!
# golem::add_dockerfile_with_renv(
#   output = "deploy",
#   dockerfile_cmd = 'R -e "options("shiny.port"=80,shiny.host="0.0.0.0");library(learndfv);learndfv::run_app(port = 80)"'
# )


