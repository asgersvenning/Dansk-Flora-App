# Script that creates a data frame of Ellenberg-values from ellenberg1991.txt

library(readxl)

eNames <- "Art L T K F R N SUB" %>% str_split(" ") %>% unlist
eDat <- "ellenberg1991.txt" %>% 
  readChar(file.info(.)$size) 

ellenberg1991 <- tibble(data = eDat %>% 
  str_extract_all("\\S+ \\S+ \\S \\S \\S \\S \\S \\S \\S+") %>% 
  unlist) %>% 
  mutate(data = map(data, function(x) {
    x %>% 
      str_split("(?<!^\\S{0,30}) ") %>% 
      unlist %>% 
      set_names(eNames)
  })) %>% 
  unnest_wider(data) %>% 
  rename(LATINSK.NAVN = Art, Light = L, Temperature = `T`, Continentiality = K, Moisture = `F`, "Soil reaction (pH)" = R, "Nitrogen (N)" = N) %>% 
  mutate(across(!LATINSK.NAVN,~ifelse(.x=="x",NA,.x)),
         across(!c(LATINSK.NAVN,SUB),as.numeric))

ellenberg <- read_excel("ellenberg.xlsx") %>% 
  select(`Scientific name`, Light, Moisture, `Soil reaction (pH)`, `Nitrogen (N)`, Salinity) %>% 
  mutate(`Scientific name` = str_extract(`Scientific name`, "[:alpha:]+ [:alpha:]+")) %>% 
  rename(LATINSK.NAVN = 1) %>% 
  full_join(ellenberg1991) %>% 
  group_by(LATINSK.NAVN) %>% 
  summarize(across(where(is.numeric),mean,na.rm=T)) %>% 
  filter(!is.na(LATINSK.NAVN))

write_rds(ellenberg,"ellenberg.rds")
