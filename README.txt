########################### IMPORTANT ###########################

!!! OBS !!! Det er vigtigt at man sikre sig at have installeret både den nyeste version af R, RTools (begge fra R's hjemmeside) og RStudio, ellers kan der opstå en masse meget besværlige problemer!

1) Kør functionen nedenfor i RGui (måske best ikke at have RStudio åbent imens :))!

sapply(c("shiny","shinyjs","tidyverse","rjson","rrapply","magrittr","keys","extrafont","googlesheets4","kableExtra","furrr","ggpubr","hash"), function(x) {
  tryCatch({library(x,character.only = T)},error=function(y) install.packages(x))
})

2) Åben filen runApp.R (evt. i RStudio, men det er sådan set ligegyldigt)

#################################################################