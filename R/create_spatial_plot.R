create_spatial_plot <- function(fig_data_for_plot, city_country_str, month_year_str){

  spat_plot <- ggplot(fig_data_for_plot) +
    geom_point(aes(x = longitude, y = latitude,
                   col = prcp_combined, shape = prcp_type),
               size = 4, stroke = 1.15) +
    scale_shape_manual(name = "Obs Type",
                       values = fct_shapes) +
    scale_color_gradient(name = "Prcp (mm)", low = "skyblue", high = "navy") +
    facet_wrap(~ date) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Rainfall Observations",
            paste(city_country_str, month_year_str))

  return(spat_plot)

}
