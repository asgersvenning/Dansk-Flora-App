# Complete R Shiny server script for the app, 
# should probably be split into Shiny modules :)

library(shiny)
library(shinyjs)
library(tidyverse)
library(rjson)
library(rrapply)
library(magrittr)
library(keys)
library(extrafont)
library(googlesheets4)
library(furrr)
# library(dbplyr)
# library(RSQLite)

# Since some of the data is stored in a Google sheets, 
# a Google account is needed and thus a oauth token.
# In order for the app to work when run a server, the 
# oauth token is saved in the .secrets folder along with the app
# that way users (or the hosting server) do not need to sign in to Google to use the app.
# This should also probably be changed :)
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

server = function(input, output) {
  useShinyjs(html = T)
  
  # Load the custom javascript files that allow for extended behavior of the app.
  extendShinyjs("main.js",
                functions = c("backgroundCol",
                              "addLoader",
                              "toggleElement",
                              "canUpdateDifficulty"))
  
  # Load the NOVANA habitat species pools
  habitats <- read_rds("clean data/habitatPools.rds") %>% 
    # mutate(pool = map(pool, function(x) x %>% 
    #                     mutate(wiki_url = ifelse(str_detect(wiki_url,"https"),wiki_url,NA)))) %>% 
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
  
  # Load the species meta data from Google sheets
  speciesMeta <- read_sheet("https://docs.google.com/spreadsheets/d/1LSeUuOCh7GNoSrQaMkslrA42-xF-wdAf_wef4d3XzxQ/edit#gid=1980062234","speciesMeta") %>% 
    rename(taxon_id = ID) 
  
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
  
  # Function for translating INaturalist taxon id's to species names
  # This function is only used to fix some problematic species names.
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
  
  # Function for getting the information (photo urls etc.) 
  # for 10 random (weighted according to some heuristics) observations.
  # 10 observations are queried at once for efficiency and "throttle" api requests,
  # since the INaturalist API is rather slow and also does not like being pinged to often.
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
  
  ########## Species app
  
  observeEvent(input$fetchSpecies, {
    # Make sure the app queries 10 observations upon load by default (only once though!)
    if (input$fetchSpecies == 1) {
      js$addLoader()
      
      values$observationPhotos <- queryPage()
    }
    
    # Increment the current observation index upon user input on the button 'TRYK FOR (NY) ART!'
    values$currentInd <- values$currentInd + 1
    # Some observations contain more than 1 photo, initially show the 1st
    values$whichImage <- 1
    
    # Legacy code that used to show a user input box, where the user could input a guess as text.
    #js$toggleElement("Input", "inline")
    # Remove the ellenberg element upon loading new observation.
    toggleElement("ellenberg","none")
    
    # If all of the observations that were queried are exhausted query 10 new.
    if (values$currentInd > nrow(values$observationPhotos)) {
      values$currentInd <- 1
      
      # Show a loading symbol until the query is completed.
      js$addLoader()
      
      values$observationPhotos <- queryPage()
    }
    
    # Store the urls of the current observation photos.
    values$img_urls <- values$observationPhotos$photo_url[[values$currentInd]]
    
    # If an observation has more than one picture, change the color of the 'SKIFT BILLEDE' to slightly yellow.
    if (length(values$img_urls) > 1) js$backgroundCol("switchImage","#ffa") else js$backgroundCol("switchImage","#fff")
    
    # Create a html img element with the source pointing to the current observation photo url.
    output$speciesImage <- renderText({
      c('<a href="',values$observationPhotos$url[values$currentInd],'">',
        '<img id="speciesImg" onload="rotateImg()" src="',
        values$img_urls[values$whichImage],
        '"></a>')
    })
    
    # Create a button in the user interface 'AFSLOR ARTEN!' that can be used to reveal the species.
    output$speciesInformation <- renderText({
      '<button onclick="Shiny.setInputValue(`revealSpecies`, `1`, {priority: `event`});"> Afslør arten! </div>'
    })
  })
  
  # When the button 'SKIFT BILLEDE' is pressed,
  # attempt to show a different image from the same observation
  observeEvent(input$switchImage, {
    if (input$fetchSpecies > 0) {
      values$whichImage <- if (values$whichImage == length(values$img_urls)) 1 else values$whichImage + 1
      
      output$speciesImage <- renderText({
        c('<a href="',values$observationPhotos$url[values$currentInd],'">',
          '<img id="speciesImg" onload="rotateImg()" src="',
          values$img_urls[values$whichImage],
          '"></a>')
      })
      
    }
  })
  
  combine_DFV_INAT <- function(DFV,INAT) {
    DFV <- DFV %>% 
      stringr::str_to_title() %>% 
      str_extract("\\S+ \\S+|^\\S+$")
    
    INAT <- INAT %>% 
      stringr::str_to_title() %>% 
      str_extract("\\S+ \\S+|^\\S+$")
    
    if (!is.na(DFV) & !is.na(INAT) && DFV == INAT) {
      return(DFV) 
    } 
    else if (!is.na(DFV) & !is.na(INAT)) {
      return(paste0(DFV, " (", INAT, ")"))  
    } else if (is.na(DFV)) {
      return(INAT)
    } else {
      return(DFV)
    }
  }
  
  # When the button 'AFSLOR ARTEN!' is pressed:
  observeEvent(input$revealSpecies, {
    # js$toggleElement("Input")
    # Allow the user to input a difficulty score
    js$canUpdateDifficulty()
    
    if (input$fetchSpecies > 0) {
      # Assemble a html string which contains the meta information of the species
      # in the current observation.
      output$speciesInformation <- renderText({
        taxonomy <- values$observationPhotos %>%
          slice(values$currentInd) %>% 
          select(DANSK.NAVN,
                 common_name,
                 LATINSK.NAVN,
                 taxon_genus_name,
                 taxon_family_name,
                 Familie,
                 taxon_order_name,  
                 taxon_class_name) %>% 
          mutate(Familie = map2_chr(Familie, taxon_family_name, combine_DFV_INAT),
                 DANSK.NAVN = map2_chr(DANSK.NAVN,common_name, combine_DFV_INAT)) %>% 
          select(!c(taxon_family_name,common_name)) %>% 
          unlist %>% 
          magrittr::extract(which(!is.na(.))) %>%
          {
            c(paste0('<p class = "speciesName">',.[1:2],"</p>"),
              paste0('<p class = "speciesTaxonomy">',.[-c(1:2)],"</p>"))
          } %>% 
          paste0(collapse = "")
        
        habitats <- values$observationPhotos %>%
          slice(values$currentInd) %>% 
          pull(habitat)
        
        if (is.na(habitats)) habitats <- "Sjældent eller ukendt"
        
        paste0(taxonomy,"<br><h3>Forveksling:</h3><p>",values$observationPhotos$forveksling[values$currentInd],"</p><br><h3>Habitater</h3><p>",habitats,"</p><br>")
      })
      
      # Show the ellenberg information element
      js$toggleElement("ellenberg","block")
      
      # Set the contents of the ellenberg element to be a html table of ellenberg values
      output$ellenberg <- renderText({
        values$observationPhotos %>%
          slice(values$currentInd) %>% 
          select(c(Light,Moisture,`Soil reaction (pH)`,`Nitrogen (N)`,Salinity)) %>% 
          mutate(across(everything(),~.x %>% 
                          as.character %>% 
                          replace_na("Ukendt"))) %>% 
          kableExtra::kable("html",align="c")
      })
      
      # Replace the species image with an iframe of the species wikipedia entry 
      output$speciesImage <- renderText({
        paste0('<iframe id="speciesIframe" src="',values$observationPhotos$wiki_url[values$currentInd],'" width = "1100px" height = "1500px" scrolling="no" style="border: none; display: block; margin: 5% auto;"></iframe>')
      })
    }
  })
  
  observeEvent(input$difficulty, {
    # Function for translating keystrokes to difficulty multipliers
    update <- switch(input$difficulty,
                     "49" = "0.5",
                     "50" = "1",
                     "51" = "2",
                     "52" = "4") %>% 
      as.numeric
    
    # Update difficulty scores
    values$difficulty[values$observationPhotos$scientific_name[values$currentInd]] <-
      values$difficulty[values$observationPhotos$scientific_name[values$currentInd]] * update
    
    output$difficultyTrigger <- renderText({paste0('<p id = "difficulty" style = "display: none;">', 
                                                   switch(input$difficulty,
                                                          "49" = "Nem",
                                                          "50" = "Tilpas",
                                                          "51" = "Usikker",
                                                          "52" = "Svær"),
                                                   '</p>')})
    
    output$difficultyPlot <- renderPlot({
      tibble(Species = names(values$difficulty),
             Difficulty = values$difficulty) %>%
        filter(Difficulty != 1) %>%
        arrange(desc(Difficulty),Species) %>% 
        mutate(Species = factor(Species, levels = unique(Species))) %>% 
        slice_head(n = 10) %>% 
        ggplot(aes(Species,Difficulty)) +
        geom_col() +
        ggpubr::theme_pubr() +
        theme(text = element_text(family = "Georgia"),
              title = element_text(face = "bold"),
              plot.title = element_text(face = "plain",
                                        hjust = .5,
                                        size  = 16),
              aspect.ratio = 1,
              axis.text.x = element_text(angle = 45,
                                         hjust = 1),
              # panel.background = element_rect(fill='transparent'), #transparent panel bg
              plot.background = element_rect(fill='transparent', color = NA), #transparent plot 
              # legend.background = element_rect(fill='transparent'), #transparent legend bg
              # legend.box.background = element_rect(fill='transparent')
        ) + 
        labs(x = NULL, y = "Sværhedsgrad", title = "Top 10 sværeste arter")
    },
    width = 1000,
    height = 1000,
    res = 200,
    bg="transparent")
    
    showElement("difficulty",T,"fade",.5)
    delay(3000,hideElement("difficulty",T,"fade",1.5))
  })
  
  
  ###### Habitat app
  
  observeEvent(input$fetchHabitat, {
    # Sample a random NOVANA habitat plot/circle, weighted according to the frequency of the habitat
    values$thisHabitat <- habitats %>%
      slice_sample(n = 1, weight_by = log(n)/n)
    
    # Legacy?
    iframe_from_url <- Vectorize(function(url) {
      if (is.na(url)) return("")
      paste0('<span class="iframe-preview">
      <a href="#" class="close">X</a>
    <iframe src="',url,'" style="border:0px #FFFFFF none;" name="test" scrolling="no" frameborder="0" marginheight="0px" marginwidth="0px" height="1000px" width="1500px;"></iframe><br>
  </span>')
    })
    
    # Show a table of the species list in the current NOVANA habitat plot/circle
    output$habitatPool <- renderText({
      # Function for creating a html table with a variable number of columns and a minimum number of rows.
      # The function first calculates if more than the minimum number of rows are needed, 
      # then fills the table columnwise.
      createTable <- function(cells, nColumns = 3, minrows = 15, tableClass = "columnTable") {
        nCells <- length(cells)
        
        nRows <- max(15, floor(nCells/3))
        
        whichColumn <- floor(seq(0,nCells-1)/nRows) + 1 
        
        columns <- vector("character",nColumns)
        
        for (i in 1:nColumns) {
          columns[i] <- paste0('    <td class="habitatSpecies"> ', which(whichColumn == i), ". ", cells[whichColumn == i]," </td>",collapse="\n")
        }
        
        return(
          paste0("<table class=\"",tableClass,"\">\n", 
                 paste0(
                   "  <tr>\n",
                   columns, 
                   "\n  </tr>", 
                        collapse="\n"), 
                 "\n</table>")
        )
      }
      
      out <- paste0(speciesMeta %>%
                      # slice_sample(n = 5) %>%
                      filter(str_extract(LATINSK.NAVN,"\\S+ \\S+|\\S+") %in% 
                               unlist(values$thisHabitat$pool[[1]]$LATINSK.NAVN)) %>%
                      select(8:12) %>%
                      summarize(across(everything(),mean,na.rm=T)) %>%
                      kableExtra::kable("html",align="c",digits = 2) %>%
                      str_replace("<table>",'<table style="margin: auto; font-size: 24px; border-spacing: 5vw 5px; border: solid 2px;">'),
                    "\n<br>\n<br>\n<br>\n",
                    values$thisHabitat$pool[[1]]$DANSK.NAVN %>% 
                      unlist %>% 
                      createTable(tableClass = "columnTable danskTable"),
                    "\n<br>\n<br>\n",
                    "<table>",
                    values$thisHabitat$pool[[1]]$LATINSK.NAVN %>% 
                      unlist %>%
                      createTable(tableClass = "columnTable latinTable"))
      cat(out)
      out
    })
    
    # Create a button ('Afslor habitatet!') for revealing the habitat.
    output$habitatInformation <- renderText({
      '<button onclick="Shiny.setInputValue(`revealHabitat`, `1`, {priority: `event`});"> Afslør habitatet! </div>'
    })
  })
  
  # When the button 'Afslor habitatet!' is pressed show the habitat name 
  # in the habitatInformation element and replace the species list with
  # a histogram of the frequencies of the (up to) 25 most common species
  # in the habitat.
  observeEvent(input$revealHabitat, {
    output$habitatInformation <- renderText({
      values$thisHabitat$habtype
    })
    
    output$habitatPool <- renderText({paste0('<img style="width: 100%;" src="histograms/',values$thisHabitat$habtype,'.png">')})
  })
  
  
}