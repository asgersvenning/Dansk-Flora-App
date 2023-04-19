#' speciesTool UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny

speciesToolUI <- function(...) {
  htmlTemplate_mod(
    app_sys("app/www/species.html"),
    filterVariable = selectInput("filterVariable", 
                                 NULL,
                                 c("Navn (latin)",
                                   "Navn (dansk)",
                                   "Sl\u00e6gt (latin)",
                                   "Familie (latin)",
                                   "Orden (latin)",
                                   "Klasse (latin)",
                                   "R\u00e6kke (latin)"),
                                 selected = "Familie (latin)"),
    filterInput = textInput("filterValue",
                            NULL,
                            "Skriv text her!"),
    filterApply  = actionButton("filterApply",
                                "Filtrer"),
    toggleIframe = checkboxInput("toggleIframe", "Vis arter.dk (langsomt)", 
                                 width = "fit-content"),
    cellArgs = list(class = "filterSpecies"),
    fetchButton = actionButton("fetchSpecies", "Tryk for (ny) art!"),
    switchButton = actionButton("switchImage", "Skift billede"),
    speciesImage = htmlOutput("speciesImage"),
    # difficultyTrigger = htmlOutput("difficultyTrigger"),
    # difficultyPlot = plotOutput("difficultyPlot", width = Inf, height = Inf),
    speciesInformation = div(
      htmlOutput("speciesInformation"),
      htmlOutput("ellenberg")
    ),
    full = F
  )
}

#' speciesTool Server Functions
#'
#' @noRd 
#' @import shiny
#' @importFrom shinyjs js hideElement showElement delay toggleElement 
#' @import dplyr
#' @importFrom magrittr extract %>% %$%
#' @importFrom knitr kable
#' @importFrom tibble tibble
#' @importFrom rjson fromJSON
speciesToolServer <- function(...) {
  eval(quote({
    # When the app has finished setting up the necessary data frames and completed the first query
    # show the user a checkmark.
    output$speciesImage <- renderText({paste0('<img id="speciesReady" src="', "www/checkmark.jpg", '"></img>')})
    
    shinyjs::useShinyjs(html = TRUE)
    shinyjs::extendShinyjs("www/main.js",
                           functions = c("backgroundCol",
                                         "addLoader",
                                         "toggleElement",
                                         "canUpdateDifficulty",
                                         "showSpecies"))
    
    ########## Species app
    
    observeEvent(input$fetchSpecies, {
      
      # Make sure the app queries 10 observations upon load by default (only once though!)
      if (input$fetchSpecies == 1) {
        js$addLoader()
        
        values$observationPhotos <- getPage()
      }
      
      # Increment the current observation index upon user input on the button 'TRYK FOR (NY) ART!'
      values$currentInd <- values$currentInd + 1
      # Some observations contain more than 1 photo, initially show the 1st
      values$whichImage <- 1
      
      # Remove the ellenberg element upon loading new observation.
      toggleElement("ellenberg","none")
      
      # If all of the observations that were queried are exhausted query 10 new.
      if (values$currentInd > nrow(values$observationPhotos)) {
        values$currentInd <- 1
        
        # Show a loading symbol until the query is completed.
        js$addLoader()
        
        values$observationPhotos <- getPage()
      }
      
      # Store the urls of the current observation photos.
      values$img_urls <- values$observationPhotos$images[[values$currentInd]]
      values$observer <- values$observationPhotos$observer[values$currentInd]
      values$observerURL <- paste0("https://arpo-prod-api-app.azurewebsites.net/account?searchText=",
                                   URLencode(values$observer) 
                                   ,"&take=1&skip=0&sortType=7") %>% 
        fromJSON(file = .) %$%
        items %>% 
        {
          tryCatch({paste0('href="https://arter.dk/user/', .[[1]]$id, '"')}, error = function(x) "Ukendt")
        }
      
      
      # If an observation has more than one picture, change the color of the 'SKIFT BILLEDE' to slightly yellow.
      if (length(values$img_urls) > 1) js$backgroundCol("switchImage","#ff8") else js$backgroundCol("switchImage","#fff")
      
      # Create a html img element with the source pointing to the current observation photo url.
      photo_html <- paste0(
        '<img id="speciesImg" onload="rotateImg()" src="', values$img_urls[values$whichImage], '">\n',
        '<a ', values$observerURL, '>Foto: ', values$observer, '</a>'
      )
      js$showSpecies(photo_html)
      
      # Create a button in the user interface 'AFSLOR ARTEN!' that can be used to reveal the species.
      output$speciesInformation <- renderText({
        '<button onclick="Shiny.setInputValue(`revealSpecies`, `1`, {priority: `event`});"> Afsl\u00f8r arten! </div>'
      })
    })
    
    # When the button 'SKIFT BILLEDE' is pressed,
    # attempt to show a different image from the same observation
    observeEvent(input$switchImage, {
      
      if (input$fetchSpecies > 0) {
        values$whichImage <- if (values$whichImage == length(values$img_urls)) 1 else values$whichImage + 1
        
        photo_html <- paste0(
          '<img id="speciesImg" onload="rotateImg()" src="', values$img_urls[values$whichImage], '">\n',
          '<a ', values$observerURL, '>Foto: ', values$observer, '</a>'
        )
        
        js$showSpecies(photo_html)
      }
    })
    
    # When the button 'AFSLOR ARTEN!' is pressed:
    observeEvent(input$revealSpecies, {
      # Allow the user to input a difficulty score
      js$canUpdateDifficulty()
      
      if (input$fetchSpecies > 0) {
        # Assemble a html string which contains the meta information of the species
        # in the current observation.
        output$speciesInformation <- renderText({
          observationInfo <- values$observationPhotos %>%
            slice(values$currentInd)
          
          taxonomy <- observationInfo %>% 
            select(acceptedVernacularName, scientificName, 
                   which(stringr::str_detect(colnames(.), "^scientific_|^vernacular_"))) %>%
            rename("vernacular_Name" = acceptedVernacularName, "scientific_Name" = scientificName) %>% 
            tidyr::pivot_longer(everything(), names_to="rank", values_to="name") %>% 
            mutate(type = factor(stringr::str_extract(rank, "^scientific|^vernacular")),
                   rank = stringr::str_remove(rank, "^scientific_|^vernacular_")) %>% 
            tidyr::pivot_wider(id_cols = type, names_from=rank, values_from=name) %>% 
            summarize(across(!type, ~paste0(.x[1], " (", .x[2], ")"))) %>% 
            relocate(1, 7, 6, 5, 4, 3, 2) %>% 
            unlist
          
          taxonomy <- c(
            paste0('<p class = "speciesName">', observationInfo$scientificName[1], " / ", observationInfo$acceptedVernacularName[1], "</p>"),
            paste0('<p class = "speciesTaxonomy">', taxonomy ,"</p>")
          ) %>% 
            paste0(collapse = "")
          
          habitats <- observationInfo %>% 
            pull(habitats) %>% 
            first
          
          if (is.null(habitats) || is.na(habitats) || all(habitats$n <= 0.05)) {
            habitats <- "Sj\u00e6lden eller ukendt"
          } else {
            habitats <- habitats %>%
              filter(n > 0.05) %>% 
              dplyr::arrange(desc(.$n)) %>% 
              summarize(
                out = paste0("<p class=\"singleHabitatText\" style=\"opacity:", n, "\">", habtype, "</p>")
              ) %>% 
              pull(out) %>% 
              paste0(collapse="")
          }
          
          forveksling <- observationInfo$similarSpeciesDescription
          if (is.null(habitats) || is.na(forveksling) || nchar(forveksling) < 2) forveksling <- "Ukendt eller ingen."
          
          paste0(taxonomy, "<br><h3>Forveksling:</h3> ", forveksling, " <h3>Habitater</h3> ", habitats)
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
                            replace(which(is.na(.)), "Ukendt"))) %>% 
            kable("html",align="c")
        })
        
        # Replace the species image with an iframe of the species wikipedia entry
        if (input$toggleIframe) js$showSpecies(paste0('<iframe id="speciesIframe" src="https://arter.dk/taxa/taxon/details/',values$observationPhotos$id[values$currentInd],'"scrolling="yes" style="border: none; margin: 5% auto; height: 90%; width: 90%;"></iframe>'))
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
      values$difficulty[values$observationPhotos$scientificName[values$currentInd]] <-
        values$difficulty[values$observationPhotos$scientificName[values$currentInd]] * update
    })
  }),
  parent.frame(n = 1))
}
