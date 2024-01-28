rm(list = ls())
script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_directory)

library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(glue)
library(tidyverse)
library(tidytext)


update_csv <- function(trams, download_id) {
  
  filename <- "trams.csv"
  trams$result$download_id <- download_id
  
  if (file.exists(filename)) {
    trams_archived <- read_csv(filename, col_types = cols(
        Brigade = col_character(),
        Lines = col_character(),
        Lon = col_double(),
        VehicleNumber = col_character(),
        Lat = col_double(),
        download_id = col_double(),
        Time = col_character()
    )
  )
    trams_updated <- bind_rows(trams_archived, trams$result)
  } else {
    trams_updated <- trams$result
  }
  write_csv(trams_updated, filename)
  
}


download_trams <- function(){
  
  um.waw.api <- "9c9a80e9-68d1-4f74-8d49-c241f3f5649f"
  um.waw.url <- "https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id=f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey="
  
  url.api <- paste(um.waw.url, um.waw.api, "&type=2", sep = "")
  trams <- fromJSON(url.api)
  
  return(trams)
}


trams <- download_trams()


trams$result %>%
  filter(Lines == 17) %>%
  filter(Time > Sys.time() - 3600) %>%
  head()


trams$result %>%
  filter(Lines == 17) %>%
  filter(Time > Sys.time() - 3600) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    radius = 4,
    lng = ~Lon,
    lat = ~Lat,
    opacity = 0.8,
    label = ~Lines,
    fillOpacity = 5
    )


update_csv(trams = download_trams(), download_id = 1)
update_csv(trams = download_trams(), download_id = 2)
update_csv(trams = download_trams(), download_id = 3)


update_csv_n_times <- function(n, sleep_time_sec){
  for (i in 1:(n-1)){
    update_csv(trams = download_trams(), download_id = i)
    update_time <- format(Sys.time(), "%H:%M:%S")
    print(glue("Done {i}-th update at {update_time}"))
    Sys.sleep(sleep_time_sec)
  }
  update_csv(trams = download_trams(), download_id = n)
  print(glue("Updated {n} times"))
  update_time <- format(Sys.time(), "%H:%M:%S")
  print(glue("Done {i}-th update at {update_time}"))
}


update_csv_n_times(n = 4, sleep_time_sec = 60)










