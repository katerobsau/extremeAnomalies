create_temporal_plot <- function(fig_data_for_plot, city_country_str, month_year_str){

  extreme_event_temporal_plot <- ggplot(fig_data_for_plot) +
    geom_point(aes(x = date, y = id,
                   col = prcp_combined, shape = prcp_type),
               size = 4, stroke = 1.15) +
    scale_shape_manual(name = "Obs Type",
                       values = fct_shapes) +
    scale_color_gradient(name = "Prcp (mm)", low = "skyblue", high = "navy") +
    xlab("Date") +
    ylab("Station Id") +
    ggtitle("Rainfall Observations",
            paste(city_country_str, month_year_str)) +
    theme_bw()

  return(extreme_event_temporal_plot)

}
