# Code to combine dataframes with observations downloaded from INaturalist
read_csv("raw data/observations.csv") %>% 
  bind_rows(read_csv("raw data/observations-3.csv"),
            read_csv("raw data/observations-4.csv"),
            # read_csv("observations-5.csv"),
            read_csv("raw data/observations-bunke.csv")) %>% 
  select(!taxon_id) %>% 
  mutate(scientific_name = str_extract(scientific_name,"\\S+ \\S+")) %>% 
  group_by(scientific_name) %>% 
  filter(sample(c(rep(T,min(c(2*10^3,n()))),rep(F,max(0,n() - 2*10^3))))) %>% 
  write_rds("clean data/observations.rds", compress = "xz")

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

initMeta <- read_rds("clean data/DFV_pensum_w_ID.rds") %>%
  mutate(LATINSK.NAVN = str_remove(LATINSK.NAVN, "\\?")) %>% 
  left_join(speciesHabitat %>%
              rename(LATINSK.NAVN = species),
            by = "LATINSK.NAVN")

plan("multisession",workers=8)
speciesMeta <- initMeta %>%
  # slice_sample(n = 5) %>%
  mutate(forveksling = future_map_chr(DANSK.NAVN, function(x) {
    if (x == "Almindelig eg") x <- "Stilk-eg"
    if (x == "Ahorn (?r)") x <- "Ahorn"
    if (x == "Almindelig engkarse") x <- "Engkarse"
    if (x == "Engriflet hvidtj?rn") x <- "Engriflet hvidtj?rn"
    if (x == "Bromb?r coll.") x <- "Bromb?r"
    
    tryCatch({
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
    }, error = function(x) NA)
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
  }, simplify = T) %>% unlist) %>% 
  select(1:5,habitat) %>% 
  mutate(LATINSK.NAVN = str_extract(LATINSK.NAVN,"\\S+ \\S+"),
         wiki_url = future_map2_chr(LATINSK.NAVN,DANSK.NAVN, function(x,y) {
           if (x == "Ranunculus ficaria") x <- "Ficaria verna"
           translate_from_latin(x,link=T,lang="da")
         })) %>% 
  left_join(read_rds("clean data/ellenberg.rds"),by="LATINSK.NAVN")

plan("sequential")

write_rds(speciesMeta, "clean data/speciesMeta.rds")