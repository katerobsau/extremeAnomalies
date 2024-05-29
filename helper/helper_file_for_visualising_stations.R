library(tidyverse)
library(RColorBrewer)
library(rnoaa)

# -----------------------------------------------------------------------
# Variables
# -----------------------------------------------------------------------

delta = 0.25
stn_lat = -27.07
stn_long = 153.10
extreme_date = as_date("2009-05-20") #as_date("1965-07-20")
date_radius = 4

meta_data = readRDS("data/aus_stations_meta.rds")

source("R/get_data_for_fig.R")
source("R/create_spatial_plot.R")
source("R/create_temporal_plot.R")

fig_data_for_plot <- get_data_for_fig(extreme_date, data_radius = 4, delta = 0.2,
                 stn_lat, stn_long, meta_data)

# -----------------------------------------------------------------------
# Spatially plot extreme rainfall event
# -----------------------------------------------------------------------

extreme_event_spatial_plot <- create_spatial_plot(fig_data_for_plot,
                    city_country_str = "Brisbane, Australia",
                    month_year_str = "January, 1974")

extreme_event_spatial_plot

# -----------------------------------------------------------------------
# Temporally plot extreme rainfall event
# -----------------------------------------------------------------------

extreme_event_temporal_plot <- create_temporal_plot(
  fig_data_for_plot,
  city_country_str = "Brisbane, Australia",
  month_year_str = "January, 1974")

extreme_event_temporal_plot

# fig_file_path = paste("data/fig_data", year(extreme_date), ".rds", sep = "")
# saveRDS(fig_data_for_plot, fig_file_path)

# -----------------------------------------------------------------------
# To dos:
# -----------------------------------------------------------------------

# Fix ticks

# Should ideally vis qflags as well

# ggplot(fig_data_for_plot ) +
#   geom_point(aes(x = longitude, y = latitude,
#                  col = qflag_prcp), size = 4) +
#   facet_wrap(~ date)


