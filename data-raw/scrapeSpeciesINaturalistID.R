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
