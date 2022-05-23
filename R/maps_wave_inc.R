function_maps_wave_inc <- function(Wave_Inf){
  
  load("data/fitted_values_1.RData")
  fitted_values_1 <- mean.samples
  load("data/fitted_values_2.RData")
  fitted_values_2<- mean.samples
  load("data/fitted_values_3.RData")
  fitted_values_3 <- mean.samples
  
  mean.samples <- rbind(fitted_values_1, fitted_values_2, fitted_values_3) %>%
    ungroup() %>%
    dplyr::group_by(Region, Wave) %>%
    mutate(fit=sum(fit)) %>%
    ungroup() %>%
    mutate(Num=ifelse(is.na(Num), 0, Num),
           Inc = Num/Population*10000) %>%
    filter(Wave == Wave_Inf)
    
# sf::sf_use_s2(TRUE)
  
  Inc_breaks <- mean.samples %>%
    select(Inc) %>%
    filter(Inc >0)

bezirk_geo <- read_sf("data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
  mutate(GEM_ID = as.character(GEM_ID)) %>%
  full_join(mean.samples)  %>%
  ungroup() %>%
  mutate(
         Inc_Q = cut(Inc,
                     breaks=c(0,quantile(Inc_breaks,
                                     probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
                     include.lowest = TRUE))) %>%
  filter(!Wave=="NA")
      
plot_wave <- ggplot()+
  # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
  #                 pattern_spacing = 0.03,pattern_size=0.5 )+
  geom_sf(data= bezirk_geo, aes(fill=   Inc_Q),alpha=1,col="black", size=0.1) +
  facet_wrap(~Wave, ncol=3) +
  scale_fill_manual("",values = col5viridis) +
  ggtitle(paste0("Incidence - Wave ", Wave_Inf))+
  theme(
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom")
# cowplot::save_plot("output/plot_excess.pdf",plot_excess,base_height=12,base_width=10)

return(plot_wave)

}

