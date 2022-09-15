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

write_rds(plotHab, "clean data/plotHabitat.rds")

# Transform the species presence matrix into a species pool dataframe (much more efficient due to the number of missing species pr. plot, i.e. 0 entries) and clean and translate the species names to Danish add Wikipedia URL convenience. 
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
  write_rds("clean data/habitatPools.rds",compress="xz") 