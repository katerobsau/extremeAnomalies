library(tidyverse)

monitors <- fig_stations$id

fig_station_data <- meteo_pull_monitors(monitors = monitors,
                                            keep_flags =  TRUE,
                                            var = "PRCP") |>
  left_join(fig_stations)

# Filter to figure dates
extreme_date = as_date("1974-01-26")
date_radius = 4
fig_dates = extreme_date + -4:4

obs_data_for_fig = fig_station_data |>
  filter(date %in% fig_dates) |>
  mutate(prcp = as.numeric(prcp)/10)

ggplot(obs_data_for_fig) +
  geom_point(aes(x = longitude, y = latitude, col = prcp)) +
  facet_wrap(~ date)
