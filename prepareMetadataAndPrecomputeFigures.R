#### Script for gathering and distributing meta information for species
# There are foreign characters from the danish alphabet in this script,
# these may be corrupted upon saving, so make sure to check this, if an error
# occurs.

library(tidyverse)
library(magrittr)
library(sf)
library(hash)
library(fuzzyjoin)
library(googlesheets4)
library(googledrive)
library(readxl)
library(rvest)
library(future.apply)
library(furrr)


# Code to combine dataframes with observations downloaded from INaturalist
read_csv("observations.csv") %>% 
  bind_rows(read_csv("observations-3.csv"),
            read_csv("observations-4.csv"),
            # read_csv("observations-5.csv"),
            read_csv("observations-bunke.csv")) %>% 
  select(!taxon_id) %>% 
  mutate(scientific_name = str_extract(scientific_name,"\\S+ \\S+")) %>% 
  group_by(scientific_name) %>% 
  filter(sample(c(rep(T,min(c(2*10^3,n()))),rep(F,max(0,n() - 2*10^3))))) %>% 
  write_rds("observations.rds", compress = "xz")

# Function for translating latin names to danish or english using wikipedia
translate_from_latin <- function(name,link=F,lang="en",revert=F) {
  inner_fun <- function(x,link=link,lang=lang) {
    out <- tryCatch({
      x %>% 
        str_replace(" ", "_") %>% 
        paste0("https://",lang,".wikipedia.org/wiki/",.) %>% 
        read_html
    },error = function(y) NA)
    
    if (is.na(out)) {
      out <- x %>% 
        str_remove(" .+") %>% 
        paste0("https://",lang,".wikipedia.org/w/api.php?action=opensearch&search=",
               .,"&limit=100&namespace=0&format=json") %>% 
        URLencode %>% 
        fromJSON(file = .) %>% 
        unlist %>% 
        extract(str_detect(., ".wikipedia.org")) %>% 
        # first %>% 
        extract(which(adist(x,str_remove(.,paste0("https://",lang,".wikipedia.org/wiki/"))) < 4))
      
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

# gs4_auth(scope = "https://www.googleapis.com/auth/drive")

# read_rds("PlotCommunity.rds")
# read_rds("Spatial.rds")

# habMeta <- tibble(hab = "Strande og strandenge
# Strandvold med en?rige planter (1210)
# Strandvold med fler?rige planter (1220)
# Kystklint eller -klippe (1230)
# En?rig strandengsvegetation (1310)
# Vadegr?ssamfund (1320)
# Strandeng (1330)
# Indlandssalteng (1340)
# Kystklitter
# Forklit (2110)
# Hvid klit (2120)
# Gr?/gr?n klit (2130)
# Klithede (2140)
# Havtornklit (2160)
# Gr?risklit (2170)
# Klitlavning (2190)
# Eneb?rklit (2250)
# Indlandsklitter, hede og krat
# Visse-indlandsklit (2310)
# Revling-indlandsklit (2320)
# Gr?s-indlandsklit (2330)
# V?d hede (4010)
# T?r hede (4030)
# Eneb?rkrat (5130)
# Overdrev, eng og klipper
# T?rt kalksandsoverdrev (6120)
# Kalkoverdrev (6210)
# Surt overdrev (6230)
# Tidvis v?d eng (6410)
# Indlandsklippe (8220)
# Indlandsklippe med pionerplanter (8230)
# Moser
# Aktiv h?jmose (7110)
# Nedbrudt h?jmose (7120)
# H?nges?k (7140)
# T?rvelavning (7150)
# Avneknippemose (7210)
# Kildev?ld (7220)
# Rigk?r (7230)
# Skove
# Skovklit (2180)
# B?g p? mor (9110)
# B?g p? mor med kristtorn (9120)
# B?g p? muld (9130)
# B?g p? kalk (9150)
# Ege-blandskov (9160)
# Vinteregeskov (9170)
# Stilkegekrat (9190)
# Skovbevokset t?rvemose (91D0)
# Elle- og askeskov (91E0)" %>% 
#                     str_split("\n") %>% 
#                     unlist) %>% 
#   mutate(type = cumsum(!str_detect(hab,"[:digit:]"))) %>%
#   group_by(type) %>% 
#   summarize(type = first(hab),
#             hab = hab[-1],
#             no = str_extract(hab, "(?<=[(]).+(?=[)])"),
#             hab = str_extract(hab, ".+(?= [(])")) 

habMeta <- read_csv2("habitatMeta.csv") %>% 
  filter(!is.na(DFV.NAME)) %>% 
  mutate(hab = ifelse(DFV.NAME == hab | DFV.NAME == "Gr? klit / Gr?n klit", DFV.NAME,paste0(DFV.NAME, " (", hab, ")")))

habCodes <- habMeta %>% 
  {
    hash(.$hab,.$no)
  } %>% 
  invert

majorHab <- hash(habMeta$hab,habMeta$type)

plotHab <- read_rds("Spatial.rds") %>% 
  st_drop_geometry() %>% 
  as_tibble %>% 
  filter(habtype %in% keys(habCodes)) %>% 
  mutate(habtype = as.character(habtype),
         habtype = hash::values(habCodes,habtype),habtype) %>% 
  left_join(read_rds("PlotCommunity.rds") %>% 
              dplyr::select(!year),
            by = "plot") %>% 
  filter(!is.na(habtype)) %>% 
  mutate(majorHab = hash::values(majorHab, habtype)) %>% 
  relocate(majorHab,habtype) 

speciesHabitat <- plotHab %>% 
  select(!c(3:29)) %>% 
  select(!1) %>% 
  group_by(habtype) %>% 
  summarize(across(everything(),~mean(.x,na.rm=T))) %>%
  pivot_longer(!habtype) %>% 
  mutate(species = name %>% 
           stringr::str_to_title() %>% 
           str_replace_all("_"," ")) %>% 
  select(!name) %>%  
  pivot_wider(species,names_from=habtype,values_from=value,values_fill = 0)

initMeta <- read_rds("DFV_pensum_w_ID.rds") %>%
  mutate(LATINSK.NAVN = str_remove(LATINSK.NAVN, "\\?")) %>% 
  left_join(speciesHabitat %>%
              rename(LATINSK.NAVN = species),
            by = "LATINSK.NAVN")

plan("multisession")
speciesMeta <- initMeta %>%
  # slice_sample(n = 5) %>%
  mutate(forveksling = future_map_chr(DANSK.NAVN, function(x) {
    if (x == "Almindelig eg") x <- "Stilk-eg"
    if (x == "Ahorn (?r)") x <- "Ahorn"
    if (x == "Almindelig engkarse") x <- "Engkarse"
    if (x == "Engriflet hvidtj?rn") x <- "Engriflet hvidtj?rn"
    if (x == "Bromb?r coll.") x <- "Bromb?r"
    
    paste0("https://www.naturbasen.dk/umbraco/api/species/GetArtForNewSearchBox?term=", x) %>%
      URLencode %>% 
      fromJSON(file = .) %>%
      first %>%
      unlist %>%
      extract(c("id","artDK4Url")) %>%
      {paste0("https://www.naturbasen.dk/art/",.[1],"/",.[2])} %>%
      read_html %>%
      html_element("#abForveksling") %>%
      html_text() %>%
      str_replace_all("\\.(?=\\S)",". ")
  })) %>%
  # mutate(forveksling = "") %>% 
  relocate(forveksling,.after=4) %>% 
  regex_left_join(speciesHabitat %>% 
                    filter(!(species %in% initMeta$LATINSK.NAVN)) %>% 
                    arrange(species) %>% 
                    mutate(species = str_extract(species, "\\S+ \\S+")) %>% 
                    group_by(species) %>% 
                    summarize(across(everything(),~ifelse(any(.x==1,na.rm=T),1,0))) %>% 
                    rename(LATINSK.NAVN = species),
                  by = "LATINSK.NAVN") %>% 
  group_by(LATINSK.NAVN.x) %>% 
  summarize(
    across(c(DANSK.NAVN,Familie,ID,forveksling),first),
    across(!c(DANSK.NAVN,Familie,ID,LATINSK.NAVN.y,forveksling),function(x) {
      out <- mean(x,na.rm=T)
      
      if (is.na(out)) 0 else out
    })
  ) %>% 
  rename(LATINSK.NAVN = LATINSK.NAVN.x) %>% 
  mutate(habitat = apply(across(!c(1:5)),1,function(x) {
    habs <- x[x > 0.05] %>% sort %>% rev %>% 
      round(1)
    if (length(habs) == 0) return("") 
    paste0('<p class = "habitatText" style="display: inline; color: rgb(',
           170 - 170*habs,",",170 - 170*habs,",",170 - 170*habs,
           ');">',str_remove(names(habs),"\\.[xy]$"),"</p>",collapse="")
  },simplify = T) %>% unlist) %>% 
  select(1:5,habitat) %>% 
  mutate(LATINSK.NAVN = str_extract(LATINSK.NAVN,"\\S+ \\S+"),
         wiki_url = future_map2_chr(LATINSK.NAVN,DANSK.NAVN, function(x,y) {
           if (x == "Ranunculus ficaria") x <- "Ficaria verna"
           translate_from_latin(x,link=T,lang="da")
         })) %>% 
  left_join(read_rds("ellenberg.rds"),by="LATINSK.NAVN")

plan("sequential")

sheet_id <- googledrive::as_id("https://docs.google.com/spreadsheets/d/1LSeUuOCh7GNoSrQaMkslrA42-xF-wdAf_wef4d3XzxQ/edit#gid=800029090")

# Save the species metadata in a Google sheets
# The sheet should be available to write on using any Google account, if it isn't contact me or save the sheet locally in this directory
write_sheet(speciesMeta, sheet_id,2)


plan("multisession")
danishNames <- names(plotHab)[30:ncol(plotHab)] %>% 
  str_replace_all("_"," ") %>% 
  stringr::str_to_sentence() %>% 
  future_sapply(translate_from_latin)
plan("sequential")

danishNames <- danishNames %>% 
  replace(sapply(.,is.na),
          names(.)[sapply(.,is.na)])

name_hash <- hash(names(danishNames),unlist(danishNames))

plan("multisession")
wiki_urls <- names(plotHab)[30:ncol(plotHab)] %>%
  str_replace_all("_"," ") %>% 
  stringr::str_to_sentence() %>% 
  future_sapply(translate_from_latin,link=T,lang="da")
plan("sequential")

wiki_urls <- wiki_urls %>% 
  replace(sapply(.,length) != 1,
          names(.)[sapply(.,length) != 1]) %>% 
  replace(sapply(.,is.na),
          names(.)[sapply(.,is.na)])

wiki_hash <- hash(names(wiki_urls),unlist(wiki_urls))

# Replace danish letters with english letters
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
  # filter(habtype == "Aktiv h?jmose") %>%
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
  # slice_head(n = 25) %>%
  # ungroup %>%
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
    ggsave(paste0("www/histograms/",replace_aeo(habtype),".png"),x,
           type = "cairo-png",dpi=200,width=4,height=4*nrow(d)/25,scale=2,limitsize = F)
    return(NA)
  }))

# Save the NOVANA habitat circle species lists as a data frame in a rds file
plotHab %>%
  select(!c(3:29)) %>%
  select(!1) %>%
  mutate(fid = row_number()) %>%
  relocate(habtype,fid) %>% 
  mutate(across(!c(1:2),~ifelse(.x==1,1,NA))) %>% 
  pivot_longer(!c(1:2),names_to="species",values_drop_na = T) %>% 
  mutate(LATINSK.NAVN = str_replace_all(species,"_"," ") %>%
           stringr::str_to_sentence(),
         DANSK.NAVN = hash::values(name_hash,LATINSK.NAVN),
         wiki_url = hash::values(wiki_hash,LATINSK.NAVN),
         wiki_url = ifelse(str_detect(wiki_url,"https"),wiki_url,NA),
         LATINSK.NAVN = str_extract(LATINSK.NAVN, "\\S+ \\S+|\\S+")) %>%
  select(!c(value,species)) %>%
  nest(pool = c(LATINSK.NAVN,DANSK.NAVN,wiki_url)) %>%
  mutate(habtype = replace_aeo(habtype)) %>% 
  write_rds("habitatPools.rds",compress="xz")

