DFV_pensum <- read_excel("raw data/DFV_Pensum_2022.xlsx")

if (file.exists("clean data/DFV_Pensum_w_ID.rds")) {
  DFV_pensum_w_ID <- read_rds("clean data/DFV_pensum_w_ID.rds")
} else {
  ####################################################################
  ## Code to gather INaturalist taxon id's for species in list #######
  plan("multisession")
  DFV_pensum_w_ID <- DFV_pensum %>%
    arrange(LATINSK.NAVN) %>%
    mutate(ID = future_map_dbl(LATINSK.NAVN, lookup_INAT_wikipedia),
           ID = future_map2_dbl(DANSK.NAVN,ID, function(x,y) {
             if (!is.na(y)) return(y)
             lookup_INAT_wikipedia(x,T)
           }),
           ID = future_map2_dbl(LATINSK.NAVN, ID, function(x,y) {
             if (!is.na(y)) return(y)
             out <- fromJSON(file = URLencode(paste0("https://api.inaturalist.org/v1/taxa?q=",str_extract(x, "[[:alnum:]\\.]+ [[:alnum:]\\.]+"),"&is_active=true")))
             if (out$total_results == 0) NA else if (out$total_results == 1) out$results[[1]]$id else out$results[[1]]$id
           }))
  plan("sequential")
  
  writeLines(as.character(DFV_pensum_w_ID %>%
                            drop_na %>%
                            pull(ID)),"misc/allSP_ID.txt")
  
  
  write_rds(DFV_pensum_w_ID, "clean data/DFV_pensum_w_ID.rds")
}

# ## code to prepare `DFV_pensum_w_ID` dataset goes here
# 
# usethis::use_data(DFV_pensum_w_ID, overwrite = TRUE)
