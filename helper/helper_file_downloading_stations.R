library(rnoaa)
library(tidyverse)
library(oz)

aus_code  = "ASN"
var = c("PRCP", "DAPR", "DWPR", "MDPR")

if(!file.exists("data/all_stations_meta.rds")){
all_stations_meta <- ghcnd_stations()
  saveRDS(all_stations_meta, "data/all_stations_meta.rds")
}

# Australian stations
aus_stations_meta <- all_stations_meta %>%
  filter(element == "PRCP") %>%
  filter(str_detect(id, aus_code)) %>%
  filter(longitude > 0) # removes ocean

# Southeast Queensland Stations
seq_stations_meta <- all_stations_meta |>
  filter(element == "PRCP") |>
  filter(str_detect(id, aus_code)) |>
  filter(latitude < -25 & latitude > -28.5) |>
  filter(longitude > 151 & longitude < 153.6)

seq_station_plot <- ggplot()  +
  geom_point(data  = aus_stations_meta,
             aes(x= longitude,  y = latitude),
             shape = 21, size = 0.2) +
  geom_point(data =  seq_stations_meta,
             aes(x= longitude,  y = latitude),
             shape = 21, fill = "red") +
  coord_fixed()

# Data download
# ~12 minutes for 435 stations
if(!file.exists("data/seq_stations_data.rds")){
  monitors <- seq_stations_meta$id
  time1 <- Sys.time()
  reduced_station_data <- meteo_pull_monitors(monitors = monitors,
                                            keep_flags =  TRUE,
                                            var = c("PRCP", "DAPR", "DWPR", "MDPR"))
  saveRDS("data/all_stations_meta.rds")
  time2 <- Sys.time()
}
