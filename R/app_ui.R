htmlTemplate(
  "www/index.html",
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
  ),
  fetchHabitat = actionButton("fetchHabitat","Tryk for (nyt) habitat!"),
  habitatInformation = htmlOutput("habitatInformation"),
  habitatPool = htmlOutput("habitatPool"),
  habitatEllenberg = htmlOutput("habitatEllenberg")
)