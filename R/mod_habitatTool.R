#' habitatTool UI Function
#'
#' @description A shiny Module.
#'
#' @param id
#'
#' @noRd 
#'
#' @import shiny
#' @import dplyr
#' @importFrom kableExtra kable
#' @importFrom stringr str_replace

habitatToolUI <- function(...){
  # ns <- NS(id)
  # tagList(
  htmlTemplate_mod(
    app_sys("app/www/habitats.html"),
    fetchHabitat = actionButton("fetchHabitat","Tryk for (nyt) habitat!"),
    habitatInformation = htmlOutput("habitatInformation"),
    habitatPool = htmlOutput("habitatPool"),
    habitatEllenberg = htmlOutput("habitatEllenberg"),
    full = F
  )
  # )
}

#' habitatTool Server Functions
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom kableExtra kable
#' @import dplyr
#' @import shiny
#' @importFrom magrittr %>%

habitatToolServer <- function(...) {
  eval(quote({
    
    ###### Habitat app
    
    observeEvent(input$fetchHabitat, {
      # Sample a random NOVANA habitat plot/circle, weighted according to the frequency of the habitat
      values$thisHabitat <- habitats %>%
        slice_sample(n = 1, weight_by = log(n)/n)
      
      
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
                        kable("html",align="c",digits = 2) %>%
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
        '<button onclick="Shiny.setInputValue(`revealHabitat`, `1`, {priority: `event`});"> Afsl\u00f8r habitatet! </div>'
      })
    })
    
    # When the button 'Afslor habitatet!' is pressed show the habitat name 
    # in the habitatInformation element and replace the species list with
    # a histogram of the frequencies of the (up to) 25 most common species
    # in the habitat.
    observeEvent(input$revealHabitat, {
      output$habitatInformation <- renderText({
        cleanHabitatNames(values$thisHabitat$habtype)
      })
      
      output$habitatPool <- renderText({paste0('<img style="height: 100%; margin: auto;" src="', 
                                               paste0("www/histograms/",values$thisHabitat$habtype,".png"), 
                                               '">')})
    })
  }),
  parent.frame(n = 1))
}

#' Habitat name cleaning function
#' 
#' @description A function that takes one or more habitat names from the included habitatPools dataframe and inserts the correct local danish characters.
#' 
#' @param name a character vector. A name from the 'habtype' column in the 'habitatPools' dataframe exported in this package, to be mapped to the correct utf-8 escaped danish spelling.
#' 
#' @returns a character vector of the same length as 'name'.

cleanHabitatNames <- function(name) {
  unlist(unname(Vectorize(function(x) {
    switch(x,
           "rigker" = "Rigk\u00e6r",
           "gra klit - gron klit (gra-gron klit)" = "Gr\u00e5 klit - Gr\u00f8n klit (Gr\u00e5-gr\u00f8n klit)",
           "hojmose (aktiv hojmose)"  = "H\u00f8jmose (Aktiv h\u00f8jmose)",
           "hengesek" = "H\u00e6nges\u00e6k",
           "kalkrigt gresland - kalkoverdrev (kalkoverdrev)"  = "Kalkrigt gr\u00e6sland - Kalkoverdrev (Kalkoverdrev)",
           "strandoverdrev (strandeng)" = "Strandoverdrev (Strandeng)",
           "klitlavning" = "Klitlavning",
           "ovre salteng (Indlandssalteng)" = "\u00d8vre salteng (Indlandssalteng)",
           "klithede" = "Klithede",
           "klithede (grarisklit)" = "Klithede (Gr\u00e5risklit)",
           "klithede (eneberklit)" = "Klithede (Eneb\u00e6rklit)",
           "vad hede" = "V\u00e5d hede",
           "tor hede" = "T\u00f8r hede",
           "surtrigt gresland - surt overdrev (surt overdrev)" = "Surtrigt gr\u00e6sland - Surt overdrev (Surt overdrev)",
           "hojmose (torvelavning)" = "H\u00f8jmose (T\u00f8rvelavning)",
           "kildeveld" = "Kildev\u00e6ld",
           "hvid klit" = "Hvid klit",
           "forklit og sandstrand (forklit)" = "Forklit og sandstrand (Forklit)",
           "tort kalksandsoverdrev" = "T\u00f8rt kalksandsoverdrev",
           "hojmose (nedbrudt hojmose)" = "H\u00f8jmose (Nedbrudt h\u00f8jmose)",
           "gron klit (revling-indlandsklit)" = "Gr\u00f8n klit (Revling-indlandsklit)",
           "klithede (havtornklit)" = "Klithede (Havtornklit)",
           "bog pa mor (bog pa mor med kristtorn)" = "B\u00f8g p\u00e5 mor (B\u00f8g p\u00e5 mor med kristtorn)",
           "nedre salteng (enarig strandengsvegetation)" = "Nedre salteng (En\u00e5rig strandengsvegetation)",
           "krat (fx med eg, hassel, eller tjorn) (eneberkrat)" = "Krat - fx med eg, hassel, eller tj\u00f8rn (Eneb\u00e6rkrat)",
           "bog pa kalk" = "B\u00f8g p\u00e5 kalk",
           "bog pa mor" = "B\u00f8g p\u00e5 mor",
           "gron klit (visse-indlandsklit)" = "Gr\u00f8n klit (Visse-indlandsklit)",
           "bog pa muld" = "B\u00f8g p\u00e5 muld",
           "krat (fx med eg, hassel, eller tjorn) (stilkegekrat)" = "Krat - fx med eg, hassel, eller tj\u00f8rn (Stilkegekrat)",
           "gron klit (gres-indlandsklit)" = "Gr\u00f8n klit (Gr\u00e6s-indlandsklit)",
           "vade (vadegressamfund)" = "Vade (Vadegr\u00e6ssamfund)",
           "rullestrand (kystklint eller -klippe)" = "Rullestrand (Kystklint eller -klippe)"
    )
  })(tolower(name))))
}
