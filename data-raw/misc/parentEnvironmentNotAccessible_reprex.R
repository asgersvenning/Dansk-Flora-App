library(shiny)

ui <- fluidPage(
  column(6,
         fluidRow(
           actionButton("increment", "Increment counter", 
                        style="width: 15vw;
                      display: block; 
                      margin: .5vh auto .5vh auto; 
                      height: 4vh;")
         ),
         fluidRow(
           actionButton("decrement", "Decrement counter", 
                        style="width: 15vw;
                      display: block; 
                      margin: .5vh auto .5vh auto; 
                      height: 4vh;")
         ),
         style = "padding: 40vh 0;"
  ),
  column(6,
         fluidRow(
           div(
             textOutput("counter"),
             style="text-align: center; 
                        font-size: 5vh;
                        height: 10vh;
                        display: flex;
                        justify-content: center;
                        align-content: center;
                        flex-direction: column;"
           )
         ),
         style = "padding: 40vh 0;"
  ),
  title = "Parent environment not accessible")

inner_server_fix <- function() {
  eval(
    quote({
      observeEvent(input$increment, {
        values$count <- values$count + 1
      })
      
      observeEvent(input$decrement, {
        values$count <- values$count - 1
      })
    }),
    parent.frame(n = 1)
  )
}

inner_server <- function(id) {
  moduleServer(id, function(input, out, session) {
    observeEvent(input$increment, {
      values$count <- values$count + 1
    })
    
    observeEvent(input$decrement, {
      values$count <- values$count - 1
    })
  })
}

server <- function(input, output, session) {
  
  values <- reactiveValues()
  
  values$count <- 0
  
  observeEvent(values$count, {
    output$counter <- renderText({
      values$count
    })
  })
  
  # Only 1 of these lines should be uncommented at a time
  inner_server_fix() # Fixed 'module server'
  # inner_server("inner") # Reprex moduleServer
  
}

runApp(
  shinyApp(
    ui = ui,
    server = server
  )
)
