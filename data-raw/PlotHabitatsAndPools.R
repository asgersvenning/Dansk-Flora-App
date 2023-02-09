# library(readr)
# library(tidyr)
# library(tibble)
# library(dplyr)
# library(sf)
# library(hash)
# library(stringr)

# Read the habitat meta data frame and subset the habitats in the course curriculum
habMeta <- read_csv2("raw data/habitatMeta.csv") %>% 
  filter(!is.na(DFV.NAME)) %>% 
  mutate(hab = ifelse(DFV.NAME == hab | DFV.NAME == "Gr? klit / Gr?n klit", DFV.NAME,paste0(DFV.NAME, " (", hab, ")")))

# Create a dictionary for translating between habitat codes and names.
habCodes <- habMeta %>% 
  {
    hash(.$hab,.$no)
  } %>% 
  invert

# Create a dictionary for translating from habitat to major habitat
majorHab <- hash(habMeta$hab,habMeta$type)

# Clean and translate the NOVANA plots with environmental variables and species lists to use habitat names consistent with the curriculum
plotHab <- read_rds("raw data/Spatial.rds") %>% 
  st_drop_geometry() %>% 
  as_tibble %>% 
  filter(habtype %in% keys(habCodes)) %>% 
  mutate(habtype = as.character(habtype),
         habtype = hash::values(habCodes,habtype),habtype) %>% 
  left_join(read_rds("raw data/PlotCommunity.rds") %>% 
              dplyr::select(!year),
            by = "plot") %>% 
  filter(!is.na(habtype)) %>% 
  mutate(majorHab = hash::values(majorHab, habtype)) %>% 
  relocate(majorHab,habtype) 

write_rds(plotHab, "clean data/plotHabitat.rds", compress = "xz")


DK2LT <- arterDKMeta %>% 
  select(scientificName, acceptedVernacularName) %>%
  mutate(src = "ArterDK") %>% 
  bind_rows(select(DFV_pensum_w_ID, LATINSK.NAVN, DANSK.NAVN) %>% 
              set_colnames(c("scientificName", "acceptedVernacularName")) %>% 
              mutate(src = "DFV")) %>% 
  group_by(scientificName, src) %>% 
  summarize(
    acceptedVernacularName = acceptedVernacularName[which.min(nchar(acceptedVernacularName))][1]
  ) %>% 
  nest(vernac = !scientificName) %>% 
  mutate(vernac = map(vernac, function(x) {
    if (nrow(x) == 1) return(x)
    
    if (length(unique(x$acceptedVernacularName)) == 2) {
      x %>% 
        filter(src == "DFV")
    } 
    else {
      x %>% 
        summarize(
          acceptedVernacularName = first(acceptedVernacularName),
          src = "Both"
        )
    }
  })) %>% 
  unnest(vernac)

name_hash <- hash(
  c(DK2LT$scientificName, "Bryopsida"), 
  c(DK2LT$acceptedVernacularName, "Bladmosser")
)

# Transform the species presence matrix into a species pool dataframe (much more efficient due to the number of missing species pr. plot, i.e. 0 entries) and clean and translate the species names to Danish add Wikipedia URL convenience. 
plotHab %>%
  select(!c(3:29)) %>%
  select(!1) %>%
  mutate(fid = row_number()) %>%
  relocate(habtype,fid) %>% 
  mutate(across(!c(1:2),~ifelse(.x==1,1,NA))) %>% 
  pivot_longer(!c(1:2),names_to="species",values_drop_na = T) %>% 
  mutate(LATINSK.NAVN = str_replace_all(species,"_"," ") %>%
           str_remove(" \\S{0,4}$") %>% 
           stringr::str_to_sentence() %>% 
           str_extract("\\S+ \\S+|\\S+"),
         DANSK.NAVN = map_chr(LATINSK.NAVN, function(x) {
           if (!has.key(x, name_hash)) return(x)
           values(name_hash, x)
         })) %>%
  select(!c(value,species)) %>%
  nest(pool = c(LATINSK.NAVN,DANSK.NAVN)) %>%
  mutate(pool = map(pool, function(x) {
    x %>% 
      mutate(LATINSK.NAVN = LATINSK.NAVN %>% 
               set_names(DANSK.NAVN)) %>% 
      pull(LATINSK.NAVN)
  })) %>% 
  mutate(habtype = stringi::stri_escape_unicode(habtype)) %>% 
  # mutate(habtype = replace_aeo(habtype)) %>% 
  write_rds("clean data/habitatPools.rds",compress="xz") 
