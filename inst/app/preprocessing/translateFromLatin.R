# Function for translating latin names to danish or english using wikipedia
translate_from_latin <- function(name, link = F, lang = "en", revert = F) {
  inner_fun <- function(x, link = link, lang = lang) {
    out <- tryCatch({
      x %>% 
        str_replace(" ", "_") %>% 
        paste0("https://", lang, ".wikipedia.org/wiki/", .) %>% 
        read_html
    }, error = function(y) NA)
    
    if (is.na(out)) {
      out <- x %>% 
        str_remove(" .+") %>% 
        paste0("https://", lang, ".wikipedia.org/w/api.php?action=opensearch&search=",
               ., "&limit=100&namespace=0&format=json") %>% 
        URLencode %>% 
        fromJSON(file = .) %>% 
        unlist %>% 
        extract(str_detect(., ".wikipedia.org")) %>% 
        extract(which(adist(x,str_remove(., paste0("https://", lang, ".wikipedia.org/wiki/"))) < 4))
      
      if (length(out) == 0) out <- NA
      
      if (!link) out <- read_html(out) else if (!is.na(out)) return(out)
    } else if (link) return(x %>% 
                              str_replace(" ", "_") %>% 
                              paste0("https://",lang,".wikipedia.org/wiki/",.))
    
    if (link & !is.na(out)) return(out) else if (is.na(out)) stop("No danish link found!")
    
    out <- out %>% 
      html_elements(if (lang == "en") "#p-lang .mw-list-item a" else "#firstHeading") 
    
    whichOut <- if (lang == "en") out %>% 
      html_attr("title") %>% 
      str_detect(.,"Danish") %>% 
      which else html_text(out)
    
    out <- if (link) html_attr(out,"href")[whichOut] else str_extract(html_attr(out,"title"), ".+(?= – )")[whichOut]
    # print(out)
    if (out == "" | length(out) == 0) NA else out
  }
  
  tryCatch({
    inner_fun(name,link,lang)
  },
  error = function(x) tryCatch({
    inner_fun(name,link,if (lang == "en") "da" else "en")
  }),
  error = function(x) tryCatch({
    if (!revert) stop("Not reverting!")
    warning("Reverting to genus!")
    inner_fun(str_extract(name,"\\S+"),link)
  }), 
  error = function(x) NA)
}