## code to prepare `arterDK` dataset goes here

arterDK <- readRDS("data-raw/clean data/arterDK.rds")

usethis::use_data(arterDK, overwrite = TRUE)
