tar_qplot <- function(id, date = NULL, period = NULL, data = NULL) {
  if (is.null(period)) {
    period <- c(date - days(7), date + days(7))
  }
  if (is.null(date)) {
    date <- seq(period[[1]], period[[2]], by = as.difftime(days(1)))
  }
  df_nbr <- nbr_subset(id, tar_read(station_loc), Inf)
  id_nbr_bydist <- df_nbr %>%
    arrange(dist) %>%
    pull(neighbour) %>%
    unique()
  id_plot <- c(id, id_nbr_bydist)
  if (is.null(data)) {
    data <- tar_read(df_prcp_zero)
  }
  data %>%
    filter(
      id %in% id_plot,
      between(.data$date, period[[1]] - days(3), period[[2]] + days(3))
    ) %>%
    mutate(
      prcp_type = case_when(
        !is.na(dapr) & dapr > 0 ~ "Accum",
        is.na(prcp) ~ "Missing",
        prcp == 0 ~ "No Rain",
        .default = "Rain"
      ),
      id = factor(id, levels = rev(id_plot))
    ) %>%
    mutate(prcp = case_when(prcp_type == "Accum" ~ mdpr,
      .default = prcp
    )) %>%
    ggplot() +
    geom_point(
      aes(x = date, y = id),
      shape = 1,
      colour = "red",
      size = 5, stroke = 1.15,
      data = tibble(date = .env$date, id = .env$id)
    ) +
    geom_point(
      aes(
        x = date, y = id,
        col = prcp, shape = prcp_type
      ),
      size = 4, stroke = 1.15
    ) +
    scale_shape_manual(
      name = "Obs Type",
      values = c("Missing" = 4, "No Rain" = 1, "Rain" = 16, "Accum" = 13)
    ) +
    geom_hline(yintercept = id, colour = "red", alpha = 0.2) +
    scale_color_gradient(name = "Prcp (mm)", low = "skyblue", high = "navy") +
    xlab("Date") +
    ylab("Station Id") +
    # ggtitle("Rainfall Observations", "Brisbane, Australia January 1974") +
    theme_bw()
}
