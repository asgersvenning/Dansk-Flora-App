#' app_server
#' @description Complete app shiny server
#' 
#' @noRd
#' 
#' @import shiny
#' @importFrom shinyjs useShinyjs, extendShinyjs
#' @importFrom readr read_rds
#' @import dplyr
#' @importFrom stringr str_split

app_server <- function(input, output) {
  useShinyjs(html = T)
  
  # Load the custom javascript files that allow for extended behavior of the app.
  extendShinyjs("main.js",
                functions = c("backgroundCol",
                              "addLoader",
                              "toggleElement",
                              "canUpdateDifficulty"))
  
  # Load the NOVANA habitat species pools
  habitats <-  habitat %>% 
    group_by(habtype) %>% 
    mutate(n = n()) %>% 
    ungroup %>% 
    select(!fid)
  
  # Load the data frame with the INaturalist ids along with species names
  observations <- read_rds("clean data/observations.rds") %>% 
    ungroup %>% 
    filter(!is.na(scientific_name)) %>%
    group_by(scientific_name) %>% 
    mutate(n = n()) %>% 
    ungroup
  
  # Load the species meta data
  speciesMeta <- read_rds("clean data/speciesMeta.rds")
  
  # Initiate a reactive object to store dynamic values
  values <- reactiveValues()
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
  output$speciesImage <- renderText({'<img id="speciesReady" src="checkmark.jpg"></img>'})
  
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
  
  speciesToolServer("speciesTool")
  
  habitatToolServer("habitatTool")
}