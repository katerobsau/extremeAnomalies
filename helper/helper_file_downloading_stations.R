library(rnoaa)
library(tidyverse)
library(oz)

aus_code  = "ASN"
min_years = 50
var = c("PRCP", "DAPR", "DWPR", "MDPR")
delta = 50
east_coast_longitude = 150

all_stations <- ghcnd_stations()

# Australian stations
aus_stations <- all_stations %>%
  filter(element == "PRCP") %>%
  filter(str_detect(id, aus_code)) %>%
  filter(longitude > 0) # removes ocean

# Southeast Queensland Stations
seq_stations <- all_stations |>
  filter(element == "PRCP") |>
  filter(str_detect(id, aus_code)) |>
  filter(latitude < -25 & latitude > -28.5) |>
  filter(longitude > 151 & longitude < 153.6)

seq_station_plot <- ggplot()  +
  geom_point(data  = aus_stations,
             aes(x= longitude,  y = latitude),
             shape = 21, size = 0.2) +
  geom_point(data =  seq_stations,
             aes(x= longitude,  y = latitude),
             shape = 21, fill = "red") +
  coord_fixed()

# Reduced Figure Stations
fig_stations <- seq_stations |>
  filter(latitude < -27.4 & latitude > -27.55) |>
  filter(longitude > 152.9 & longitude < 153.1)

fig_station_plot <- ggplot()  +
  geom_point(data  = seq_stations,
             aes(x= longitude,  y = latitude),
             shape = 21, size = 0.2) +
  geom_point(data =  fig_stations,
             aes(x= longitude,  y = latitude),
             shape = 21, fill = "red") +
  coord_fixed()

# Data download
# ~12 minutes for 435 stations
monitors <- seq_stations$id
time1 <- Sys.time()
reduced_station_data <- meteo_pull_monitors(monitors = monitors,
                                            keep_flags =  TRUE,
                                            var = "PRCP")
time2 <- Sys.time()
