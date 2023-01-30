# library(tidyverse)
# library(furrr)
# library(magrittr)
# library(rjson)
# library(readr)
# library(fuzzyjoin)

queryTaxons <- function(n = 200, request = "taxons") {
  paste0("https://arpo-prod-api-app.azurewebsites.net/", request, "?searchText=&take=", n, "&skip=0&notMatched=false&isDkTaxon=true&isDefaultTaxon=true&isMissingPhoto=false&hasPhoto=false&ancestorIds=d00b80a4-6d2e-41d4-81e2-aee500d1e2d1&includeAdministrativeLists=true")
}

queryImages <- function(id, n = 200) {
  paste0("https://arpo-prod-api-app.azurewebsites.net/records?searchText=&take=", n, "&skip=0&taxonIds=", id, "&includeDescendantTaxons=true&isDeleted=&hasMedia=true&excludeSaughtButNotFound=true&includeSpeciesGroupFacet=false&includeIsBlurredFacet=false&includeOrphanRecords=false&url=")
}

getDetailedInformation <- function(id) {
  fromJSON(file = paste0("https://arpo-prod-api-app.azurewebsites.net/taxons/", id))
}

getInformation <- function(json) {
  data <- json %>% 
    extract(c("id","acceptedVernacularName", "scientificName")) %>% 
    unlist
  
  out <- c(
    if ("id" %in% names(data)) data["id"] else NA,
    if ("acceptedVernacularName" %in% names(data)) data["acceptedVernacularName"] else NA,
    if ("scientificName" %in% names(data)) data["scientificName"] else NA
  )
}

getImages <- function(json) {
  location <- as.vector(json$geoLocation)[c("latitude", "longitude")]
  
  tibble(
    images = json$medias %>% 
      sapply(function(x) x$url) %>% 
      list,
    latitude = location[["latitude"]],
    longitude = location[["longitude"]]
  )
}

plan("multisession", workers = 8)
allData <- fromJSON(file = queryTaxons(3000)) %>% 
  extract2("items") %>% 
  sapply(getInformation, simplify = T) %>% 
  t %>% 
  as_tibble %>% 
  filter(str_detect(scientificName, "[^[:space:]]+ [^[:space:]]+")) %>% 
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
  fuzzy_left_join(habitats%>% 
                    unnest(pool) %>% 
                    count(habtype, DANSK.NAVN) %>%
                    group_by(DANSK.NAVN) %>% 
                    mutate(n = n/sum(n)) %>% 
                    ungroup %>% 
                    nest(habitats = !DANSK.NAVN) %>% 
                    rename("acceptedVernacularNameNotScientific" = "DANSK.NAVN"),
                  by = "acceptedVernacularNameNotScientific",
                  function(x,y) map2_lgl(x,y, function(a,b) adist(a,b)[1] < 3)) %>% 
  left_join(ellenberg %>% 
              rename("scientificName" = "LATINSK.NAVN"),
            by = "scientificName") %>% 
  unnest(images)
  # pull(classification) %>% 
  # first

arterDKMeta <- allDataClass %>% 
  select(where(~!any(sapply(., is.null)))) %>% 
  select(where(~!is.list(.))) %>% 
  group_by(scientificName) %>% 
  summarize(
    across(everything(), ~set_names(first(.x, default = NA), length(unique(.x))))
  ) %>%
  select(scientificName, where(~all(names(.) == 1)))

saveRDS(arterDKMeta, "data-raw/clean data/arterDKMeta.rds")
saveRDS(allDataClass %>% 
          select(scientificName,!names(arterDKMeta)), "data-raw/clean data/arterDK.rds",
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
