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

habitatToolUI <- function(id){
  ns <- NS(id)
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

habitatToolServer <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
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
  })
}

## To be copied in the UI
# mod_habitatTool_ui("habitatTool_1")

## To be copied in the server
# mod_habitatTool_server("habitatTool_1")
