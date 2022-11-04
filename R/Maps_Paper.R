plot1 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim1),  pandemic_end=as.Date(datlim2), Pandemic_Name="1918 first wave"))
plot2 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim1),  pandemic_end=as.Date(datlim2), Pandemic_Name="1918 first wave"))

plot3 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim3),  pandemic_end=as.Date(datlim6), Pandemic_Name="1918-1919 second wave"))
plot4 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim3),  pandemic_end=as.Date(datlim6), Pandemic_Name="1918-1919 second wave"))

plot5 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim7),  pandemic_end=as.Date(datlim8), Pandemic_Name="1920"))
plot6 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim7),  pandemic_end=as.Date(datlim8), Pandemic_Name="1920"))

plot7 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim9),  pandemic_end=as.Date(datlim10), Pandemic_Name="1922"))
plot8 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim9),  pandemic_end=as.Date(datlim10), Pandemic_Name="1922"))

plot9 <- tmap_grob(function_plot_maps(pandemic_start=as.Date(datlim11),  pandemic_end=as.Date(datlim12), Pandemic_Name="1924"))
plot10 <- tmap_grob(function_hotspot_wave(pandemic_start=as.Date(datlim11),  pandemic_end=as.Date(datlim12), Pandemic_Name="1924"))

plot_maps <- plot_grid(plot1, plot2,
          plot3, plot4,
          plot5, plot6,
          plot7, plot8,
          plot9, plot10,
          ncol=2)


# cowplot::save_plot("output/Figure_maps.pdf", plot_maps ,base_height=35,base_width=15)
