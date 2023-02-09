## code to prepare `habitatPools` dataset goes here

habitatPools <- readRDS("data-raw/clean data/habitatPools.rds")

usethis::use_data(habitatPools, overwrite = TRUE)
