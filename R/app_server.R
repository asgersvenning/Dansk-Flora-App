#' app_server
#' @description Complete app shiny server
#' 
#' @noRd
#' 
#' @import shiny
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>% is_greater_than set_names

globalVariables(c("habtype", "fid", "rid", "scientificName", "acceptedVernacularNameNotScientific.x", "acceptedVernacularNameNotScientific.y", ".", "var"))

app_server <- function(input, output, session) {
  shinyjs::useShinyjs(html = TRUE)
  shinyjs::extendShinyjs("www/main.js",
                         functions = c("backgroundCol",
                                       "addLoader",
                                       "toggleElement",
                                       "canUpdateDifficulty"))
  
  # Load the NOVANA habitat species pools
  habitats <- habitatPools %>% 
    mutate(habtype = stringi::stri_unescape_unicode(habtype)) %>% 
    group_by(habtype) %>% 
    mutate(n = n()) %>% 
    ungroup %>% 
    select(!fid)
  
  arterDKMeta <- arterDKMeta %>% 
    mutate(across(c(acceptedVernacularName, pensumName), stringi::stri_unescape_unicode),
           habitats = lapply(habitats, function(x) {
             if (is.null(x)) return(NULL)
             x %>% 
               mutate(habtype = stringi::stri_unescape_unicode(habtype))
           }))
  
  # Initiate a reactive object to store dynamic values
  values <- reactiveValues()
  
  # Load the data frame with the INaturalist ids along with species names
  observations <- arterDK %>% 
    group_by(scientificName) %>% 
    mutate(n = n()) %>% 
    ungroup
  
  # 'filterInd' should be a vector of rows to use from the observations data frame.
  # This way the observations data frame can be subsetted dynamically.
  values$filterInd <- 1:nrow(observations)
  
  # 'difficulty' is a named vector (names = species names) with the user difficulty
  # of ID'ing the observation photos. This can be used to sample the observations by
  # difficulty. The difficulties are initated as 1 for all species
  values$difficulty <- observations$scientificName %>% 
    unique %>% 
    {
      set_names(rep(1, length(.)),.)
    }
  
  # Since the app queries 10 observations at once, a reactive value that stores the current
  # index of the shown observation.
  values$currentInd <- 0
  
  # Use the Dansk Flora og Vegetationsøkologi pensum version of the app
  values$pensum <- F
  
  # Function for subsetting the observations using the filter form in the user interface.
  observeEvent(input$filterApply, {
    if (!(input$filterValue %in% c("", "Skriv text her!"))) {
      filterColumn <- switch(input$filterVariable,
                             "Navn (latin)" = "scientificName",
                             "Navn (dansk)" = "acceptedVernacularName",
                             "Sl\u00e6gt (latin)" = "scientific_Sl\u00e6gt",
                             "Familie (latin)" = "scientific_Familie",
                             "Orden (latin)" = "scientific_Orden",
                             "Klasse (latin)" = "scientific_Klasse",
                             "R\u00e6kke (latin)" = "scientific_R\u00e6kke")
      
      pensumFlag <- stringr::str_detect(input$filterValue, stringr::regex("pensum", T))
      filterRegex <- if (pensumFlag) stringr::str_remove_all(input$filterValue, "pensum *,* *| *,*$") else input$filterValue
      values$pensum <- pensumFlag
      
      filterRegexes <- filterRegex %>% 
        stringr::str_split(" *, *") %>% 
        unlist %>%
        extract(which(nchar(.) > 2))
      
      arterFilterRows <- arterDKMeta %>%
        select(filterColumn, pensum) %>% 
        rename("var" = 1) %>% 
        mutate(rid = row_number()) %>% 
        filter(pensum)
      
      arterFilterRows <- if (!is.null(filterRegexes) && length(filterRegexes) > 0) {
        arterFilterRows %>% 
          filter(filterRegexes %>% 
                   sapply(function(x) agrepl(x,var)) %>% 
                   rowSums %>% 
                   is_greater_than(0)) %>% 
          pull(rid)
      } 
      else {
        arterFilterRows$rid
      }
      
      arterFilter <- arterDKMeta$scientificName[arterFilterRows]
      
      
      values$filterInd <- which(observations$scientificName %in% arterFilter)
      
      print(values$filterInd)
    }
  })
  
  speciesToolServer("species") # , observations, values
  
  habitatToolServer("habitats") # , values
}
