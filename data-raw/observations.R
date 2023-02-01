# library(tidyverse)
# library(magrittr)
# library(rvest)
# library(rjson)
# library(future.apply)

getObs <- function(n, skip) {
  if (skip <= 0 | n <= 0 | !(skip == as.integer(skip)) | !(n == as.integer(n))) stop("'skip' and 'n' must be positive integers.")
  tryCatch({
  fromJSON(file = paste0("https://arpo-prod-api-app.azurewebsites.net/records?searchText=&take=", n,"&skip=", n * (skip - 1),"&zoomLevel=7&taxonRanks=Art&taxonIds=d00b80a4-6d2e-41d4-81e2-aee500d1e2d1&recordValidationStatuses=1&recordValidationStatuses=5&recordValidationStatuses=2&terrestrial=true&searchMode=1&includeDescendantTaxons=true&isDeleted=&hasMedia=true&excludeSaughtButNotFound=true&includeSpeciesGroupFacet=true&includeIsBlurredFacet=false&includeOrphanRecords=false&sortBy=1&url=")) %$%
  items %>% 
  sapply(
    function(x) {
      x <- x %>% 
        extract(c("id","taxonId","observationAt","geoLocation","medias","scientificName","observers","validationStatus")) %>% 
        {
          .$medias <- lapply(.$medias, function(x) x$url)
          .
        }
      
      if (!("geoLocation" %in% names(x))) {
        x$geoLocation <- list(latitude = NA, longitude = NA, uncertaintyInMeters = NA)
      }
      
      x
    }
    ) 
  }, error = function(x) NA
  ) %>% 
  t %>% 
  as_tibble %>% 
  unnest(!c(geoLocation, medias)) %>% 
  unnest_wider(geoLocation)
}


plan("multisession")
observations <- apply(matrix(c(rep(9999, 8), 1:8), ncol = 2), 1, function(x) getObs(x[1], x[2]), simplify = F) %>% 
  bind_rows %>% 
  filter(longitude > 0 & latitude > 54 & latitude < 58)
plan("sequential")

## code to prepare `observations` dataset goes here

usethis::use_data(observations, overwrite = TRUE)
