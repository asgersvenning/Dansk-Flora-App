setwd("inst/app/www/histograms") # Set working directory to path of the histograms folder inside inst/app/www

for (i in list.files()) {
  file.rename(i, stringr::str_replace_all(stringr::str_replace_all(i, "[[:space:]-(),]", "_"), "_+","_"))
} 
  
