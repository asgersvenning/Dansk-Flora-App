library(tidyverse)
library(magrittr)
library(rjson)
library(rrapply)
library(readxl)
library(rvest)
library(future)
library(furrr)

# Function that tries to find the wikipedia URL (either danish or english wiki) for a species name
lookup_INAT_wikipedia <- function(x, dansk = F) {
  thisURL <- tryCatch({
    temp <- x %>% 
      str_replace(" ", "_") %>% 
      str_remove(" .+") %>% 
      paste0("https://",if (dansk) "da" else "en",".wikipedia.org/w/api.php?action=opensearch&search=",.,"&limit=10&namespace=0&format=json") %>% 
      URLencode %>% 
      fromJSON(file = .) %>% 
      unlist %>% 
      extract(str_detect(., paste0(if (dansk) "da" else "en", ".wikipedia.org/wiki/"))) 
    
    if (length(temp) > 1) temp <- first(temp[nchar(temp) == min(nchar(temp))])
    
    url(temp, "rb")
  }, error = function(x) NA)
  
  if (is.na(thisURL)) return(NA)
  
  if (dansk) {
    thisURL <- thisURL %>%
      read_html() %>% 
      html_element("#p-lang .vector-menu-content-list") %>% 
      html_children() %>% 
      html_children() %>% 
      html_attr("href") %>% 
      extract(str_detect(.,"en.wikipedia.org"))
    
    if (length(thisURL) == 0) return(NA)
    
    thisURL <- url(thisURL, "rb")
  }
  
  if (is.na(thisURL)) return(NA)
  
  outHTML <- thisURL %>% 
    read_html()
  
  out <- outHTML %>% 
    html_elements(".navbox span") %>% 
    html_text() %>% 
    str_extract("(?<=iNaturalist: )[:digit:]+") %>% 
    extract(which(!is.na(.))) %>% 
    as.numeric %>% 
    first
  
  close(thisURL)
  
  return(out)
}

DFV_pensum <- read_excel("DFV_Pensum_2022.xlsx")


#####################################################################
### Code to gather INaturalist taxon id's for species in list #######
# plan("multisession")
# DFV_pensum_w_ID <- DFV_pensum %>%
#   arrange(LATINSK.NAVN) %>%
#   mutate(ID = future_map_dbl(LATINSK.NAVN, lookup_INAT_wikipedia),
#          ID = future_map2_dbl(DANSK.NAVN,ID, function(x,y) {
#            if (!is.na(y)) return(y)
#            lookup_INAT_wikipedia(x,T)
#          }),
#          ID = future_map2_dbl(LATINSK.NAVN, ID, function(x,y) {
#            if (!is.na(y)) return(y)
#            out <- fromJSON(file = URLencode(paste0("https://api.inaturalist.org/v1/taxa?q=",str_extract(x, "[[:alnum:]\\.]+ [[:alnum:]\\.]+"),"&is_active=true")))
#            if (out$total_results == 0) NA else if (out$total_results == 1) out$results[[1]]$id else out$results[[1]]$id
#          }))
# # mutate(ID = map(LATINSK.NAVN, function(x) {
# #   fromJSON(file = paste0("https://api.inaturalist.org/v1/taxa?q=",str_replace(x," ", "%20")))
# # }))
# plan("sequential")
# 
# writeLines(as.character(DFV_pensum_w_ID %>%
#                           drop_na %>%
#                           pull(ID)),"allSP_ID.txt")
# 
# 
# write_rds(DFV_pensum_w_ID, "DFV_pensum_w_ID.rds")

DFV_pensum_w_ID <- read_rds("DFV_pensum_w_ID.rds")


