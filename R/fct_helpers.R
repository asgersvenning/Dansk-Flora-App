#' translate_id
#' @description Function for translating INaturalist taxon id's to species names.
#' This function is only used to fix some problematic species names.
#' 
#' @noRd 
#' @importFrom magrittr %>%

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
#' @importFrom stringr str_to_title str_extract
#' @importFrom magrittr %>%

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

#' getPage
#' @description Function for getting the information (photo urls etc.) for 10 random (weighted according to some heuristics) observations. 10 observations are queried at once for efficiency and "throttle" api requests, since the INaturalist API is rather slow and also does not like being pinged to often.
#' @noRd
#' 
#' @importFrom rjson fromJSON
#' @importFrom stringr str_replace
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom magrittr %>% %$%

globalVariables(c(".", "results", "ID", "images"))

getPage <- function() {
  # This is extremely cursed, but it works.
  out <- eval(quote({
    temp_ <- observations %>%
      # Subset the rows 'values$filterInd' of the observations data frame 
      slice(values$filterInd) %>% 
      # Weighted sampling of 10 rows of the observations data frame, 
      # weighting is done using a heuristic that combines the number,
      # of observations of each species and the user 'difficulty' of each species.
      slice_sample(n = 10, weight_by = log(n)/n * values$difficulty[observations$scientificName][values$filterInd]) %>% 
      # Filter out observations that do not have any photos associated
      filter(nchar(images) != 0) %>% 
      mutate(images = stringr::str_split(images, "\\|\\|")) %>%
      left_join(arterDKMeta, by = "scientificName")
    
    if (values$pensum) {
      temp_ <- temp_ %>% 
        mutate(acceptedVernacularName = pensumName)
    }
    
    temp_
  }), parent.frame())
  
  return(out)
}

#' htmlTemplate_mod
#' @description htmlTemplate but without the bs automatic header changes
#' 
#' @noRd
#' 
#' @importFrom shiny htmlOutput
#' @import magrittr

htmlTemplate_mod <- function(..., full = F, escape_header = F) {
  remove_body_and_header <- function(x) {
    temp <- x
    temp[[1]][[1]] <- temp[[1]][[1]] %>% 
      as.character %>% 
      stringr::str_remove("(?s).*<body>.{3}") 
    attributes(temp[[1]][[1]]) <- attributes(x[[1]][[1]])
    
    last <- length(temp[[1]])
    
    temp[[1]][[last]] <- temp[[1]][[last]] %>% 
      as.character %>% 
      stringr::str_remove("(?s).{1}</body>(?s).*") 
    
    attributes(temp[[1]][[last]]) <- attributes(x[[1]][[last]]) 
    
    temp
  }
  
  remove_header_comment <- function(x) {
    temp <- x
    temp[[1]][[1]][[1]] <- temp[[1]][[1]][[1]] %>% 
      stringr::str_remove("<!\\-\\- (?=<head>)") %>% 
      stringr::str_remove("(?<=<\\/head>) \\-\\->") 
    
    attributes(temp[[1]][[1]][[1]]) <- attributes(x[[1]][[1]][[1]])
    
    temp
  }
  
  out <- htmlTemplate(...)
  
  if (!full & escape_header) stop("Cannot be a partial HTML with header.")
  
  if (!full) {
    return(remove_body_and_header(out))
  }
  if (escape_header) {
    return(remove_header_comment(out))
  }

  out
}
