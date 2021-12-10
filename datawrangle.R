require(plotly)
require(tidyverse)

datawrangle <- function(category) {
  # Region categories to sort counties
  superior <- c("Butte", "Colusa", "Del Norte", "Glenn", "Humboldt", "Lake", 
                "Lassen", "Mendocino", "Modoc", "Nevada", "Plumas", "Shasta", 
                "Sierra", "Siskiyou", "Tehama", "Trinity")
  central <- c("Alpine", "Amador", "Calaveras", "El Dorado", "Fresno", "Inyo", 
               "Kings", "Madera", "Mariposa", "Merced", "Mono", "Placer", 
               "Sacramento", "San Joaquin", "Stanislaus", "Sutter", "Yuba", 
               "Tulare", "Tuolumne", "Yolo")
  bay <- c("Alameda", "Contra Costa", "Marin", "Monterey", "Napa", "San Benito", 
           "San Francisco", "San Mateo", "Santa Clara", "Santa Cruz", "Solano", 
           "Sonoma")
  southern <- c("Imperial", "Kern", "Orange", "Riverside", "San Bernardino", 
                "San Diego", "San Luis Obispo", "Santa Barbara", "Ventura", 
                "Los Angeles")
  
  download3 <- getURL("https://data.lacity.org/api/views/iv7a-6rrq/rows.csv?accessType=DOWNLOAD")
  demoCA <- read.csv(text = download3)
  
  
  
  demoCA <- 
    demoCA %>%
    filter(date == "10/31/2021 12:00:00 AM") %>%
    filter(demographic_category == category) %>%
    mutate(region = case_when(county %in% superior ~ "Superior",
                              county %in% central ~ "Central",
                              county %in% bay ~ "Bay Area",
                              county %in% southern ~ "Southern")) %>%
    summarise(date = date,
              region = region,
              county = county,
              demographic_category = demographic_category,
              demographic_value = demographic_value,
              perc_fully_vax = (cumulative_fully_vaccinated/est_population))
  
  demoCA <- na.omit(demoCA)
  
  demoCA <-
    demoCA %>%
    group_by(region, demographic_value) %>%
    summarise(region = region,
              demographic_value = demographic_value,
              region_fully_vax = mean(perc_fully_vax)*100)
  
  demoCA <- unique(demoCA)
  
  return(demoCA)
}

datawrangle2 <- function(category) {
  # Region categories to sort counties
  superior <- c("Butte", "Colusa", "Del Norte", "Glenn", "Humboldt", "Lake", 
                "Lassen", "Mendocino", "Modoc", "Nevada", "Plumas", "Shasta", 
                "Sierra", "Siskiyou", "Tehama", "Trinity")
  central <- c("Alpine", "Amador", "Calaveras", "El Dorado", "Fresno", "Inyo", 
               "Kings", "Madera", "Mariposa", "Merced", "Mono", "Placer", 
               "Sacramento", "San Joaquin", "Stanislaus", "Sutter", "Yuba", 
               "Tulare", "Tuolumne", "Yolo")
  bay <- c("Alameda", "Contra Costa", "Marin", "Monterey", "Napa", "San Benito", 
           "San Francisco", "San Mateo", "Santa Clara", "Santa Cruz", "Solano", 
           "Sonoma")
  southern <- c("Imperial", "Kern", "Orange", "Riverside", "San Bernardino", 
                "San Diego", "San Luis Obispo", "Santa Barbara", "Ventura", 
                "Los Angeles")
  
  download3 <- getURL("https://data.lacity.org/api/views/iv7a-6rrq/rows.csv?accessType=DOWNLOAD")
  demoCA <- read.csv(text = download3)
  
  
  
  demoCA <- 
    demoCA %>%
    filter(date == "10/31/2021 12:00:00 AM") %>%
    filter(demographic_category == category) %>%
    mutate(region = case_when(county %in% superior ~ "Superior",
                              county %in% central ~ "Central",
                              county %in% bay ~ "Bay Area",
                              county %in% southern ~ "Southern")) %>%
    summarise(date = date,
              region = region,
              county = county,
              demographic_category = demographic_category,
              demographic_value = demographic_value,
              perc_fully_vax = (cumulative_fully_vaccinated/est_population))
  
  demoCA <- na.omit(demoCA)
  
  demoCA <-
    demoCA %>%
    group_by(region, demographic_value) %>%
    summarise(region = region,
              county = county,
              demographic_value = demographic_value,
              perc_fully_vax = perc_fully_vax*100)
  
  return(demoCA)
}