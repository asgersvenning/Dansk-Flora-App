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
library(rjson)
library(future)

#### Script for computing all the necessary preprocessing to run the app.
# A few notes on the preprocessing workflow:
# 1) The packages that are loaded in this script are all the ones that are needed in order to run all other scripts in the preprocessing folder
#.2) The order in which the scripts are run in this folder, is also the order in which the scripts should be run if all the files in the "clean data" folder are deleted.
# 3) Some of these scripts can take *quite* a while since they make A LOT of API queries. This is mainly due to the number of data sources, species and also because many of the scripts contain many alternative API queries that they will fall back on if on the API queries fails, which happens quite often in my experience.
# 4) If some files are mysteriously missing check the "misc" folder. 

#### Pipeline
# 0.
# This script is separate from the rest of the preprocessing pipeline. Merges and cleans Ellenberg from the raw data sources.
source("preprocessing/ellenberg.R")

# 1.
# This script only contains a function for "translating" Latin scientific names to vernacular names (either Danish or English) using Wikipedia. The function is used in other scripts.
source("preprocessing/translateFromLatin.R")

# 2.
# This script tries to find the INaturalist taxon ID (needed later) also using Wikipedia, however it has implemented 3 different pathways to find the INaturalist ID:
#   - Search Wikipedia using the scientific Latin name
#   - Search Wikipedia using the vernacular danish name
#   - Query the INaturalist API using the scientific Latin name
source("preprocessing/scrapeSpeciesINaturalistID.R")

# 3.
# This script cleans and joins two raw data files (from the course github anno 2021) containing information on the species composition and habitat type. The result is a data frame (tibble) containing a row for each NOVANA plot with the following columns:
#   - habtype: Contains the "habitat name" of the plot (in danish, but the danish letters are substituted).
#   - fid: internal plot id.
#   - pool: The plot species pool as a nested data frame with rows for ONLY the present species and the following columns:
#     - LATINSK.NAVN: Scientific Latin name.
#     - DANSK.NAVN: Danish vernacular name.
#     - wiki_url: Wikipedia url, preferably in Danish, but in English if no Danish page could be found.
source("preprocessing/PlotHabitatsAndPools.R")

# 4. 
# This script creates species histograms for each of the habitats (only shows species with a presence frequency of more than 1%).
source("preprocessing/HabitatSpeciesHistograms.R")

# 5.
# This script assembles meta data for the INaturalist observations; a data frame containing the following columns:
#   - id: INaturalist observation ID (necessary to query the INaturalist API for the photo URLS).
#   - url: URL of the INaturalist observation.
#   - *_name: columns containing the scientific, vernacular name and the taxonomy of the observation.
#   - Rest of columns: Meta data information on the observation.
# The script also assembles the meta data for the species as a data frame containing the following:
#   - scientific and vernacular name.
#   - family in accordance with the curriculum i.e. not current taxonomy.
#   - INaturalist taxon ID
#   - Forveksling: Common species mixups (from NaturBasen)
#   - habitat: habitats in which the species is found as a HTML string.
#   - Wikipedia url.
#   - Ellenberg values.
source("preprocessing/Metadata.R")