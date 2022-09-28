#' speciesTool UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @importFrom shinyjs toggleElement
#' @importFrom shinyjs js
#' @import dplyr
#' @importFrom magrittr extract
#' @import ggplot
#' @importFrom ggpubr theme_pubr
#' @importFrom kableExtra kable
#' @importFrom tibble tibble
#' @importFrom purrr map2_chr

speciesToolUI <- function(id){
  ns <- NS(id)
  tagList(
    filterObservations = inputPanel(
      selectInput("filterVariable",NULL,c("scientific_name","common_name","taxon_class_name","taxon_order_name","taxon_family_name","taxon_genus_name"),selected = "taxon_family_name"),
      textInput("filterValue",NULL,"Skriv text her!"),
      br(class = ""),
      actionButton("filterApply","Filtrer"),
      cellArgs = list(class = "filterSpecies"),
      id = "filterInput"
    ),
    fetchButton = actionButton("fetchSpecies","Tryk for (ny) art!"),
    switchButton = actionButton("switchImage", "Skift billede"),
    speciesImage = htmlOutput("speciesImage"),
    difficultyTrigger = htmlOutput("difficultyTrigger"),
    difficultyPlot = plotOutput("difficultyPlot", width = Inf, height = Inf),
    speciesInformation = inputPanel(
      htmlOutput("speciesInformation"),
      htmlOutput("ellenberg")
      # div(
      #   textInput("guessSpecies","Art"),
      #   textInput("guessGenus","Slægt"),
      #   textInput("guessFamily","Familie"),
      #   id = "Input",
      #   style = "width: 100%;"
      # )
    )
  )
}

#' speciesTool Server Functions
#'
#' @noRd 
speciesToolServer <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
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
            mutate(Familie = map2_chr(Familie, taxon_family_name, combine_two),
                   DANSK.NAVN = map2_chr(DANSK.NAVN,common_name, combine_two)) %>% 
            select(!c(taxon_family_name,common_name)) %>% 
            unlist %>% 
            extract(which(!is.na(.))) %>%
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
            kable("html",align="c")
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
          theme_pubr() +
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
    
    
  })
}

## To be copied in the UI
# mod_speciesTool_ui("speciesTool_1")

## To be copied in the server
# mod_speciesTool_server("speciesTool_1")
