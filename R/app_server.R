#' app_server
#' @description Complete app shiny server
#' 
#' @noRd
#' 
#' @import shiny
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @import dplyr
#' @importFrom stringr str_split
#' @importFrom magrittr %>% is_greater_than set_names

app_server <- function(input, output, session) {
  shinyjs::useShinyjs(html = TRUE)
  shinyjs::extendShinyjs("www/main.js",
                         functions = c("backgroundCol",
                                       "addLoader",
                                       "toggleElement",
                                       "canUpdateDifficulty"))
  
  # Load the NOVANA habitat species pools
  habitats <- habitatPools %>% 
    group_by(habtype) %>% 
    mutate(n = n()) %>% 
    ungroup %>% 
    select(!fid)
  
  # Initiate a reactive object to store dynamic values
  values <- reactiveValues()
  
  # Load the data frame with the INaturalist ids along with species names
  observations <- observations %>% 
    ungroup %>% 
    filter(!is.na(scientific_name)) %>%
    group_by(scientific_name) %>% 
    mutate(n = n()) %>% 
    ungroup
  
  # 'filterInd' should be a vector of rows to use from the observations data frame.
  # This way the observations data frame can be subsetted dynamically.
  values$filterInd <- 1:nrow(observations)
  
  # 'difficulty' is a named vector (names = species names) with the user difficulty
  # of ID'ing the observation photos. This can be used to sample the observations by
  # difficulty. The difficulties are initated as 1 for all species
  values$difficulty <- observations$scientific_name %>% 
    unique %>% 
    {
      set_names(rep(1, length(.)),.)
    }
  
  # Since the app queries 10 observations at once, a reactive value that stores the current
  # index of the shown observation.
  values$currentInd <- 0
  
  # When the app has finished setting up the necessary data frames and completed the first query
  # show the user a checkmark.
  output$speciesImage <- renderText({paste0('<img id="speciesReady" src="', "/www/checkmark.jpg", '"></img>')})
  
  # Function for subsetting the observations using the filter form in the user interface.
  observeEvent(input$filterApply, {
    print(input$filterVariable)
    print(input$filterValue)
    
    values$filterInd <- observations %>% 
      select(input$filterVariable) %>% 
      rename("var" = 1) %>% 
      mutate(rid = row_number()) %>% 
      filter(input$filterValue %>% 
               str_split(",") %>% 
               unlist %>% 
               sapply(function(x) agrepl(x,var)) %>% 
               rowSums %>% 
               is_greater_than(0)) %>% 
      pull(rid)
  })
  
  # Connect shiny modules for the learning tools
  
  speciesToolServer("species")
  
  habitatToolServer("habitats")
}
