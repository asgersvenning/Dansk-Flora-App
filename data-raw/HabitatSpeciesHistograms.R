# library(readr)
# library(stringr)
# library(stringi)
# library(dplyr)
# library(tibble)
# library(future.apply)
# library(magrittr)

plotHab <- read_rds("clean data/plotHabitat.rds")

# Creates a dictionary for translating between Danish and scientific names
plan("multisession")
danishNames <- names(plotHab)[30:ncol(plotHab)] %>% 
  str_replace_all("_"," ") %>% 
  str_to_sentence() %>% 
  future_sapply(translate_from_latin)
plan("sequential")

danishNames <- danishNames %>% 
  replace(sapply(.,is.na),
          names(.)[sapply(.,is.na)])

name_hash <- hash(names(danishNames),unlist(danishNames))

# Creates a "dictionary" for translating between scientific names and Wikipedia urls
# This code is extremely redundant from the previous block and they should probably just be integrated!
# However the Wikipedia API is VERY fast, and as such this doesn't take too too long compared to some of the other scripts.
plan("multisession")
wiki_urls <- names(plotHab)[30:ncol(plotHab)] %>%
  str_replace_all("_"," ") %>% 
  str_to_sentence() %>% 
  future_sapply(translate_from_latin,link=T,lang="da")
plan("sequential")

wiki_urls <- wiki_urls %>% 
  replace(sapply(.,length) != 1,
          names(.)[sapply(.,length) != 1]) %>% 
  replace(sapply(.,is.na),
          names(.)[sapply(.,is.na)])

wiki_hash <- hash(names(wiki_urls),unlist(wiki_urls))

# Replace Danish letters with English letters
# There has to be a better way, also I am having issues with the Danish letters being corrupted when the scripts are loaded/unloaded or saved.
replace_aeo <- function(str) {
  str %>% 
    str_replace_all("æ","e") %>% 
    str_replace_all("Æ","E") %>% 
    str_replace_all("ø","o") %>% 
    str_replace_all("Ø","O") %>% 
    str_replace_all("å","a") %>% 
    str_replace_all("Å","A") %>% 
    str_replace_all("/", "-")
}

# Predraw species histograms for the NOVANA habitats
plotHab %>%
  select(!c(3:29)) %>%
  select(!1) %>%
  mutate(fid = row_number(),
         across(!c(fid,habtype),~ifelse(.x==0,NA,.x))) %>%
  pivot_longer(!c(habtype,fid),values_drop_na = T) %>%
  select(!value) %>%
  mutate(name = str_replace_all(name,"_"," ") %>%
           stringr::str_to_sentence() %>%
           hash::values(name_hash,.)) %>%
  distinct(fid,name,.keep_all=T) %>%
  count(habtype,name) %>%
  group_by(habtype) %>%
  mutate(n = n/sum(n)) %>%
  arrange(desc(n)) %>%
  filter(n > 0.01) %>%
  nest(dat = !habtype) %>%
  mutate(plt = map2(dat, habtype, function(x,y) {
    x %>%
      arrange(n) %>%
      mutate(name = factor(name, levels = name)) %>%
      ggplot(aes(n,name)) +
      geom_col() +
      geom_text(aes(label = sprintf("%.1f%%", round(n * 100,1))),
                position = position_dodge(width = 0.9), hjust = -0.25) +
      ggpubr::theme_pubr() +
      scale_x_continuous(labels = scales::percent) +
      scale_y_discrete(labels = function(x) str_extract(x,".{0,25}")) +
      coord_cartesian(expand = F, clip = "off") +
      theme(text = element_text(family = "Georgia"),
            title = element_text(face = "bold"),
            plot.title = element_text(hjust = .5),
            plot.margin = margin(0.25,2.5,0,0,"lines")) +
      labs(title = y, x = "Frekvens", y = NULL)
  }),
  hist = pmap(list(plt,habtype,dat), function(x,y,d) {
    ggsave(paste0("www/histograms/", replace_aeo(habtype), ".png"), x,
           type = "cairo-png", dpi = 200, 
           width = 4, height = 4 * nrow(d)/25, 
           scale = 2, limitsize = F)
    return(NA)
  }))



