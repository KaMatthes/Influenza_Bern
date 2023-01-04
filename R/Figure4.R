plot1 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim1),  pandemic_end=as.Date(datlim2), Pandemic_Name="1918 (July-September)"))
plot2 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim1),  pandemic_end=as.Date(datlim2), Pandemic_Name="1918 (July-September)"))

plot3 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim3),  pandemic_end=as.Date(datlim6), Pandemic_Name="1918-1919 (October-June)"))
plot4 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim3),  pandemic_end=as.Date(datlim6), Pandemic_Name="1918-1919 (October-June)"))

plot5 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim7),  pandemic_end=as.Date(datlim8), Pandemic_Name="1920 (January-June)"))
plot6 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim7),  pandemic_end=as.Date(datlim8), Pandemic_Name="1920 (January-June)"))

plot7 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim9),  pandemic_end=as.Date(datlim10), Pandemic_Name="1922 (January-April)"))
plot8 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim9),  pandemic_end=as.Date(datlim10), Pandemic_Name="1922(January-April)"))

plot9 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim11),  pandemic_end=as.Date(datlim12), Pandemic_Name="1924 (January-May)"))
plot10 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim11),  pandemic_end=as.Date(datlim12), Pandemic_Name="1924 (January-May)"))

plot_maps <- plot_grid(
          plot1,NULL, plot2,
          plot3, NULL,plot4,
          plot5,NULL, plot6,
          plot7, NULL,plot8,
          plot9, NULL, plot10,
          rel_widths = c(1, -0.3, 1),
          ncol=3)



plot_grid(plot1, NULL, plot2, rel_widths = c(1, -0.1, 1), align = "hv",
          labels = c("A", "B"), nrow = 1)

cowplot::save_plot("output/Figure_maps2.pdf", plot_maps ,base_height=35,base_width=15)

cowplot::save_plot("output/Figure_maps2.pdf", plot_maps ,base_height=42,base_width=15)
