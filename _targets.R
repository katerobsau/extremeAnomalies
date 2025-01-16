# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "rnoaa",
    "oz",
    "dplyr",
    "stringr",
    "tidyr",
    "geosphere",
    "Matrix",
    "rlang",
    "lubridate"
  ),
  format = "qs", # default storage format
  deployment = "worker",
  storage = "worker",
  retrieval = "worker",
  seed = 0,
  error = "abridge",
  iteration = "list"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.


aus_code <- "ASN"
min_years <- 50
delta <- 50
east_coast_longitude <- 150


list(
  # Read data
  tar_target(station_meta, ghcnd_stations()),
  tar_target(seq_stations, station_meta |>
    filter(element == "PRCP") |>
    filter(str_detect(id, aus_code)) |>
    filter(latitude < -25 & latitude > -28.5) |>
    filter(longitude > 151 & longitude < 153.6)),
  tar_target(monitors, seq_stations$id),
  tar_target(
    reduced_station_data,
    meteo_pull_monitors(
      monitors = monitors,
      keep_flags = TRUE,
      var = c("PRCP", "DAPR", "DWPR", "MDPR")
      # dapr is used to determine tagged accumulation
      # mapr is the value of tagged accumulation
    )
  ),

  # process data
  tar_target(df_prcp, reduced_station_data %>%
    # select(id, date, prcp, qflag_prcp) %>%
    mutate(
      across(c(dapr, prcp, mdpr), as.numeric),
      across(c(prcp, mdpr), \(x) x / 10)
    )),
  tar_target(station_loc, station_meta %>%
    distinct(id, latitude, longitude, elevation, name) %>%
    filter(latitude < -25 & latitude > -28.5) %>%
    filter(longitude > 151 & longitude < 153.6)),

  # All stations id
  # 1296 stations in total
  tar_target(id_stations, distinct(df_prcp, id)),
  # neighbour of all the stations
  # max dist 20 km
  tar_target(df_nbr, nbr_subset(id_stations$id, station_loc, Inf)),

  # threshold of extreme
  # calculated based on 99% percentiles of all rainy days
  # of all surrounding stations
  tar_target(
    ls_nbr,
    split(df_nbr$neighbour, df_nbr$target)
  ),
  tar_target(
    vec_thr,
    {
      df_p <- drop_na(df_prcp, prcp)
      vec_prcp <- df_p$prcp
      names(vec_prcp) <- df_p$id
      vec_prcp <- vec_prcp[vec_prcp > 0]
      vec_thr <- vapply(ls_nbr,
        \(x) quantile(vec_prcp[names(vec_prcp) %in% x], 0.99),
        FUN.VALUE = numeric(1L)
      )
      vec_thr
    }
  ),

  # extreme days
  tar_target(
    df_prcp_ext,
    find_extreme(df_prcp, vec_thr, nbr = ls_nbr)
  ),

  # 1. missing extremes
  tar_target(
    df_prcp_mis,
    mutate(df_prcp_ext, flag_missing_extreme = is.na(prcp) & extreme) %>%
      group_by(id) %>%
      mutate(missing_extreme_days = cumn_nonzero(flag_missing_extreme)) %>%
      ungroup()
  ),


  # 2. accumulated extremes
  tar_target(
    # aggregates
    accumulated_agg,
    df_prcp_mis %>%
      mutate(
        accumulated_monday = !is.na(dapr) & dapr > 0
      ) %>%
      filter(accumulated_monday) %>%
      select(id, date, dapr)
  ),
  # days with missing value that were accumulated
  tar_target(accumulated_days, {
    id <- accumulated_agg$id
    date <- accumulated_agg$date
    dapr <- accumulated_agg$dapr
    tibble(
      id = rep(id, dapr),
      date = do.call(c, mapply(
        \(date, dapr) {
          seq(from = date - days(dapr - 1), to = date, by = as.difftime(days(1)))
        },
        date, dapr,
        SIMPLIFY = FALSE
      ))
    ) %>%
      mutate(accumulated = TRUE)
  }),
  tar_target(
    df_prcp_acc,
    df_prcp_mis %>%
      left_join(accumulated_days, by = join_by(id, date)) %>%
      replace_na(list(accumulated = FALSE)) %>%
      mutate(flag_accumulated_extreme = accumulated & extreme)
  ),


  # 3. extreme 0s
  tar_target(
    df_prcp_zero,
    df_prcp_acc %>%
      mutate(flag_extreme_zero = extreme & !is.na(prcp) & prcp == 0)
  ),


  # end
  tar_target(end, NULL)
)
