library(tidyverse)
library(RColorBrewer)

# -----------------------------------------------------------------------
# Variables
# -----------------------------------------------------------------------

lat_upp = -27.4
lat_low = -27.55
long_low = 152.9
long_upp = 153.1

extreme_date = as_date("1974-01-26")
date_radius = 4

rainfall_vars = c("PRCP", "DAPR", "DWPR", "MDPR")
fct_levels = c("No rain", "Rain", "Missing", "Accum")
fct_shapes = c(1, 16, 4, 13)

# -----------------------------------------------------------------------
# Pull all station data for figure
# -----------------------------------------------------------------------

meta_data = readRDS("data/all_stations_meta.rda")

fig_stations_meta <- meta_data |>
  filter(latitude > lat_low & latitude < lat_upp) |>
  filter(longitude > long_low & longitude < long_upp)
monitors <- fig_stations_meta$id

# fig_station_plot <- ggplot()  +
#   geom_point(data  = seq_stations_meta,
#              aes(x= longitude,  y = latitude),
#              shape = 21, size = 0.2) +
#   geom_point(data =  fig_stations_meta,
#              aes(x= longitude,  y = latitude),
#              shape = 21, fill = "red") +
#   coord_fixed()

fig_station_data_all <- meteo_pull_monitors(monitors = monitors,
                                        keep_flags =  TRUE,
                                        var = rainfall_vars)

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
  mutate(qflag_prcp = as.factor(qflag_prcp)) |>
  select(id, longitude, latitude, prcp_combined, prcp_type, date, qflag_prcp)

# -----------------------------------------------------------------------
# Spatially plot extreme rainfall event
# -----------------------------------------------------------------------

# 4 - cross
# 1 - empty circle
# 16 - filled circle
# 13 - circle with cross

ggplot(fig_data_for_plot) +
  geom_point(aes(x = longitude, y = latitude,
                 col = prcp_combined, shape = prcp_type),
             size = 4, stroke = 1.15) +
  scale_shape_manual(name = "Obs Type",
                     values = fct_shapes) +
  scale_color_gradient(name = "Prcp (mm)", low = "skyblue", high = "navy") +
  facet_wrap(~ date) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Rainfall Observations", "Brisbane, Australia January 1974")

# -----------------------------------------------------------------------
# Temporally plot extreme rainfall event
# -----------------------------------------------------------------------

extreme_event_temporal_plot <- ggplot(fig_data_for_plot) +
  geom_point(aes(x = date, y = id,
                 col = prcp_combined, shape = prcp_type),
             size = 4, stroke = 1.15) +
  scale_shape_manual(name = "Obs Type",
                     values = fct_shapes) +
  scale_color_gradient(name = "Prcp (mm)", low = "skyblue", high = "navy") +
  xlab("Date") +
  ylab("Station Id") +
  ggtitle("Rainfall Observations", "Brisbane, Australia January 1974") +
  theme_bw()

extreme_event_temporal_plot

file_path = paste("data/example_", year(extreme_date), ".rds", sep = "")
saveRDS(fig_data_for_plot, file_path)

# -----------------------------------------------------------------------
# To dos:
# -----------------------------------------------------------------------

# Fix ticks

# Should ideally vis qflags as well

# ggplot(fig_data_for_plot ) +
#   geom_point(aes(x = longitude, y = latitude,
#                  col = qflag_prcp), size = 4) +
#   facet_wrap(~ date)


