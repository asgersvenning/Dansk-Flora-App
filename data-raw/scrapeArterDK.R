# library(tidyverse)
# library(furrr)
# library(magrittr)
# library(rjson)
# library(readr)
# library(fuzzyjoin)
# library(magrittr)

conflicted::conflict_prefer_all("magrittr")
conflicted::conflict_prefer("filter", "dplyr")

queryTaxons <- function(n = 200) {
  paste0("https://arpo-prod-api-app.azurewebsites.net/records?searchText=&take=", n, "&skip=0&speciesGroups=Planter&taxonRanks=Art&excludeTaxonIds=b98365de-f785-ea11-aa77-501ac539d1ea&recordValidationStatuses=1&recordValidationStatuses=2&recordValidationStatuses=5&searchMode=4&includeDescendantTaxons=true&excludeUnderlyingTaxons=true&hasMedia=true&excludeSaughtButNotFound=true&includeSpeciesGroupFacet=false&includeIsBlurredFacet=false&includeOrphanRecords=false")
}

queryImages <- function(id, n = 200) {
  paste0("https://arpo-prod-api-app.azurewebsites.net/records?searchText=&take=", n, "&skip=0&taxonIds=", id, "&includeDescendantTaxons=true&isDeleted=&hasMedia=true&excludeSaughtButNotFound=true&includeSpeciesGroupFacet=false&includeIsBlurredFacet=false&includeOrphanRecords=false&url=")
}

getDetailedInformation <- function(id) {
  fromJSON(file = paste0("https://arpo-prod-api-app.azurewebsites.net/taxons/", id))
}

getInformation <- function(json) {
  data <- json %>% 
    extract(c("id","primaryName", "secondaryName")) %>% 
    unlist
  
  out <- c(
    if ("id" %in% names(data)) data["id"] else NA,
    if ("primaryName" %in% names(data)) data["primaryName"] else NA,
    if ("secondaryName" %in% names(data)) data["secondaryName"] else NA
  )
}

getImages <- function(json) {
  location <- as.vector(json$geoLocation)[c("latitude", "longitude")]
  
  tibble(
    images = json$medias %>% 
      sapply(function(x) x$url) %>% 
      list,
    latitude = location[["latitude"]],
    longitude = location[["longitude"]],
    observer = json$observers
  )
}

plan("multisession", workers = 8)
allData <- fromJSON(file = queryTaxons(3000)) %$% 
  hydratedFacets %$%
  taxonId %$%
  values %>% 
  sapply(getInformation, simplify = T) %>% 
  t %>% 
  as_tibble %>%
  rename("vernacularName" = "primaryName", "scientificName" = "secondaryName") %>% 
  mutate(flag = str_detect(vernacularName, regex("brombær", T))) %>% 
  nest(brombær = !flag) %>% 
  mutate(brombær = map2(flag, brombær, function(f, d) {
    if (f) {
      d %>% 
        summarize(
          vernacularName = "Brombær",
          scientificName = "Rubus coll.",
          id = "ac7ed72e-1498-4b36-a436-ad2e00b3de77"
        )
    }
    else {
      d
    }
  })) %>% 
  unnest(brombær) %>%
  select(!flag) %>% 
  mutate(
    images = future_map(id, function(x) {
      fromJSON(file = queryImages(x, 500))$items %>% 
        lapply(getImages) %>% 
        bind_rows()
    }),
    data = future_map(id, getDetailedInformation)) %>% 
  select(images, data) %>% 
  unnest_wider(data) %>% 
  select(!similarSpeciesDescription) %>% 
  unnest_wider(description) %>% 
  select(where(~!all(is.na(.) | is.null(.)))) 
plan("sequential")

ellenberg <- read_rds("data-raw/clean data/ellenberg.rds")
habitats <- read_rds("data-raw/clean data/habitatPools.rds") %>% 
  group_by(habtype) %>% 
  mutate(n = n()) %>% 
  ungroup %>% 
  select(!fid)


allDataClass <- allData %>% 
  # slice_sample(n = 10) %>% 
  mutate(classification = map(classification, function(x) {
    tryCatch({
      x$ancestorTaxa %>% 
        lapply(as_tibble) %>% 
        bind_rows %>% 
        filter(rank %in% c(100, 200, 500, 900, 1300, 1600)) %>% 
        arrange(rank) %>% 
        mutate(rank = c("Rige", "Række", "Klasse", "Orden", "Familie", "Slægt")) %>% 
        rename("scientific" = taxonCanonicalName, "vernacular" = "taxonVernacularName") %>% 
        select(rank, scientific, vernacular) %>% 
        pivot_wider(names_from = rank, values_from = c(scientific, vernacular))
    }, error = function(x) print(str(x)))
  })) %>% 
  unnest(classification) %>% 
  left_join(ellenberg %>% 
              rename("scientificName" = "LATINSK.NAVN"),
            by = "scientificName") %>% 
  # unnest(images) %>% 
  select(!c(helpPageUrl, 
            geographyValidationParameters, 
            phenologyValidationParameters, 
            generalInfo, 
            editor, 
            creator, 
            trofi,
            biology, 
            distribution, 
            taggedMedias, 
            parent, 
            parentSuperTaxon, 
            serialNumber, 
            dkReference, 
            administrativeListAssociations, 
            biotope, 
            speciesGroup, 
            morphoGroup, 
            funFacts)) %>% 
  select(images, !where(is.list))

cleanDanish <- function(x) {
  x %>% 
    str_remove_all("-") %>% 
    str_remove_all("\\(.+\\)") %>% 
    str_squish()
}

cleanLatin <- function(x) {
  x %>% 
    str_extract("\\S+ *\\S*") %>% 
    str_squish()
}

DFV_pensum_w_ID <- read_rds("clean data/DFV_pensum_w_ID.rds")

BASE <- DFV_pensum_w_ID[,c(2,1)]
REF <- allDataClass[,c(6,4)]

BASE[[1]] <- cleanLatin(BASE[[1]])
REF[[1]] <- cleanLatin(REF[[1]])
BASE[[2]] <- cleanDanish(BASE[[2]])
REF[[2]] <- cleanDanish(REF[[2]])

latinD <- adist(BASE[[1]], REF[[1]], ignore.case = T, partial = T)

danishD <- adist(BASE[[2]], REF[[2]], ignore.case = T, partial = T) 

REF_rowNumber <- (latinD * danishD) %>% 
  set_rownames(BASE[[2]]) %>% 
  set_colnames(REF[[2]]) %>% 
  replace(which(. > 2), NA) %>% 
  apply(1, function(x) {
    out <- which.min(x)
    if (length(out) == 0) return(NA)
    out
  }) %>% 
  sapply(function(x) if (length(x) == 0) NA else x) %>% 
  set_names(BASE[[2]])

arterDK_exceptions <- allDataClass %>% 
  mutate(RN = row_number()) %>% 
  filter(str_detect(acceptedVernacularName, regex("brombær|dunet havre|eng-havre|^almindelig ene$|^grøn star$", T))) %>% 
  mutate(RN = set_names(RN, acceptedVernacularName)) %>% 
  pull(RN)

arterDK_exceptions <- arterDK_exceptions[c(1,2,4,3,5)] 

REF_rowNumber[c(c(192, 63), which(is.na(REF_rowNumber)))] <- arterDK_exceptions

arterDKMeta <- allDataClass %>% 
  mutate(pensum = row_number() %in% REF_rowNumber) %>% 
  mutate(pensumName = names(sort(REF_rowNumber))[cumsum(pensum) + 1] %>% lag) %>%
  fuzzy_left_join(habitats %>% 
                    unnest(pool) %>% 
                    mutate(LATINSK.NAVN = pool,
                           DANSK.NAVN = names(pool)) %>% 
                    count(habtype, DANSK.NAVN) %>%
                    group_by(DANSK.NAVN) %>% 
                    mutate(n = n/sum(n)) %>% 
                    ungroup %>% 
                    nest(habitats = !DANSK.NAVN) %>% 
                    rename("acceptedVernacularName" = "DANSK.NAVN"),
                  by = "acceptedVernacularName",
                  function(x,y) map2_lgl(x,y, function(a,b) adist(a,b)[1] < 3)) %>% 
  select(!c(acceptedVernacularName.y, habitat, acceptedVernacularNameNotScientific, eTag, isDefaultTaxon, isDeleted, isDKTaxon, isHybrid, images)) %>% 
  rename("acceptedVernacularName" = acceptedVernacularName.x)

arterDKMeta <- arterDKMeta %>% 
  mutate(across(c(acceptedVernacularName, pensumName), stringi::stri_escape_unicode),
         habitats = map(habitats, function(x) {
           if (is.null(x)) return(NULL)
           x %>% 
             mutate(habtype = stringi::stri_escape_unicode(habtype))
         }))

saveRDS(arterDKMeta, "data-raw/clean data/arterDKMeta.rds")
saveRDS(allDataClass %>% 
          select(images, scientificName) %>% 
          unnest(images) %>% 
          select(scientificName,
                 images,
                 observer) %>% 
          mutate(images = map_chr(images, paste0, collapse="||")), "data-raw/clean data/arterDK.rds",
        compress = T)

# library(ggmap)
#register_google(API_KEY) # Insert your own API Key
map <- get_map("Denmark", zoom = 7, maptype = "toner-background")

allData %>% 
  select(images, acceptedVernacularName) %>% 
  unnest(images) %>% 
  filter(latitude > 52.5 & longitude > 0) %>% 
  {
    ggmap(ggmap=map) +
      geom_bin_2d(data = .,
                  aes(x=longitude, y=latitude), 
                  bins = 100,
                  inherit.aes = F,
                  alpha = .75) +
      scale_fill_viridis_c(trans = "log10")
  }
