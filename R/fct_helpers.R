#' translate_id
#' @description Function for translating INaturalist taxon id's to species names.
#' This function is only used to fix some problematic species names.
#' 
#' @noRd 
translate_id <- Vectorize(function(id, str, latin=F) {
  out <- if (!latin) {
    id %>% 
      as.character() %>% 
      switch (
        "159458" = "Opret Hejre",
        "76033" = "Mellembrudt Star",
        "76128" = "Gron Star",
        "77148" = "Bakke-svingel",
        "51726" = "Mark-forglemmigej",
        "129449" = "Strandarve"
      )
  } else {
    id %>% 
      as.character() %>% 
      switch (
        "129449" = "Honckenya peploides"
      )
  }
  
  if (is.null(out)) str else out
})

#' combine_two
#' @description Combines the latin names from sources.
#' @param x, chr First name
#' @param y, chr Second name
#' @returns If x and y are both not NA and equal x, else if both are not NA '{x} ({y})' else return prioritize not NA.
#' @noRd
#' 
#' @importFrom stringr str_to_title, str_extract

combine_two <- function(x,y) {
  x <- x %>% 
    str_to_title() %>% 
    str_extract("\\S+ \\S+|^\\S+$")
  
  y <- y %>% 
    str_to_title() %>% 
    str_extract("\\S+ \\S+|^\\S+$")
  
  if (!is.na(x) & !is.na(y) && x == y) {
    return(x) 
  } 
  else if (!is.na(x) & !is.na(y)) {
    return(paste0(x, " (", y, ")"))  
  } else if (is.na(x)) {
    return(y)
  } else {
    return(x)
  }
}

#' querypage
#' @description Function for getting the information (photo urls etc.) for 10 random (weighted according to some heuristics) observations. 10 observations are queried at once for efficiency and "throttle" api requests, since the INaturalist API is rather slow and also does not like being pinged to often.
#' @noRd
#' 
#' @importFrom rjson fromJSON
#' @importFrom stringr str_replace
#' @import dplyr
#' @importfrom tibble tibble
#' @importFrom purrr map_dbl



queryPage <- function(size = 10) {
  
  # Subfunction for quering the meta information of a list of INaturalist observation ID's.
  queryID <- function(ID) {
    res <- ID %>% 
      paste0(collapse=",") %>% 
      paste0("https://api.inaturalist.org/v1/observations/",.) %>% 
      fromJSON(file = .) %$% 
      results
    
    urls <- res %>% 
      lapply(function(x) {
        sapply(x$observation_photos, function(y) y$photo$url) %>% 
          str_replace("/square\\.", "/large.")
      })
    
    ids <- res %>% 
      sapply(function(x) x$taxon$min_species_taxon_id %>% unlist %>% first)
    
    outID <- res %>% 
      sapply(function(x) x$id) 
    
    tibble(photo_url = urls[order(outID)],
           taxon_id = ids[order(outID)])
  }
  
  out <- observations %>%
    # Subset the rows 'values$filterInd' of the observations data frame 
    slice(values$filterInd) %>% 
    # Weighted sampling of 10 rows of the observations data frame, 
    # weighting is done using a heuristic that combines the number,
    # of observations of each species and the user 'difficulty' of each species.
    slice_sample(n = size, weight_by = log(n)/n * values$difficulty[observations$scientific_name][values$filterInd]) %>% 
    arrange(id) %>% 
    # Query the INaturalist API for the subset and sampled observation ID's.
    mutate(nest = queryID(id)) %>% 
    unnest(nest) %>% 
    # Filter out observations that do not have any photos associated
    filter(map_dbl(photo_url,length) != 0) %>%
    # Add meta data
    left_join(speciesMeta, "taxon_id") %>% 
    mutate(DANSK.NAVN = translate_id(taxon_id,DANSK.NAVN),
           LATINSK.NAVN = translate_id(taxon_id,LATINSK.NAVN,T))
  
  # This is here just so the console can be used to debug errors in the output.
  print(out %>% 
          relocate(photo_url), n = size)
  
  
  return(out)
}