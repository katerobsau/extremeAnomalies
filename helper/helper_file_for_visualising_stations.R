library(tidyverse)
library(RColorBrewer)

station_meta = fig_stations
monitors <- fig_stations$id
extreme_date = as_date("1974-01-26")
date_radius = 4
fct_levels = c("No rain", "Rain", "Missing", "Accum")
fct_shapes = c(1, 16, 4, 13)

# -----------------------------------------------------------------------
# Pull all station data for figure
# -----------------------------------------------------------------------

fig_station_data_all <- meteo_pull_monitors(monitors = monitors,
                                            keep_flags =  TRUE,
                                            var = c("PRCP", "DAPR", "DWPR", "MDPR"))

# -----------------------------------------------------------------------
# Filter station data to necessary subset
# -----------------------------------------------------------------------

# Filter to figure dates, assign variables types, join meta data
fig_dates = extreme_date + - date_radius:date_radius
fig_data = fig_station_data_all |>
  filter(date %in% fig_dates) |>
  mutate(prcp = as.numeric(prcp)/10,
         dapr = as.numeric(dapr),
         mdpr = as.numeric(mdpr)/10) |>
  left_join(fig_stations)

# -----------------------------------------------------------------------
# Create a new variable for the shape
# -----------------------------------------------------------------------

# Combine daily prcp and tagged accumulations
fig_data_for_plot = fig_data |>
  mutate(dapr_noNA = ifelse(is.na(dapr), 0, dapr)) |>
  mutate(prcp_combined = ifelse(dapr_noNA > 0, mdpr, prcp)) |>
  mutate(prcp_type = 'Rain') |>
  mutate(prcp_type = ifelse(prcp == 0, 'No rain', prcp_type)) |>
  mutate(prcp_type = ifelse(is.na(prcp_combined), 'Missing', prcp_type)) |>
  mutate(prcp_type = ifelse(dapr_noNA > 0, 'Accum', prcp_type)) |>
  mutate(prcp_type = factor(prcp_type, levels = fct_levels)) |>
  select(longitude, latitude, prcp_combined, prcp_type, date, qflag_prcp)

# -----------------------------------------------------------------------
# Plot extreme rainfall event
# -----------------------------------------------------------------------

# 4 - cross
# 1 - empty circle
# 16 - filled circle
# 13 - circle with cross

ggplot(fig_data_for_plot) +
  geom_point(aes(x = longitude, y = latitude,
                 col = prcp_combined, shape = prcp_type), size = 4) +
  scale_shape_manual(name = "Obs Type",
                     values = fct_shapes) +
  scale_color_distiller(name = "Prcp (mm)") +
  facet_wrap(~ date) +
  xlab("Longitude") +
  ylab("Latitue") +
  ggtitle("Rainfall Observations", "Brisbane, Australia January 1974")

# To dos:
# Reverse defualt colour scale ? scale_color_distiller(direction = -1)
# Fix ticks

# ggplot(fig_data_for_plot ) +
#   geom_point(aes(x = longitude, y = latitude,
#                  col = qflag_prcp), size = 4) +
#   facet_wrap(~ date)


