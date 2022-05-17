function_maps<- function(){

    load("data/data_bern.RData")
   
  
  data_bern <- data_bern %>%
    distinct(Year, GEM_ID, .keep_all = TRUE ) %>%
  select(Year, GEM_ID, NumbCases) 
    
# sf::sf_use_s2(TRUE)

bezirk_geo <- read_sf("data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
  mutate(GEM_ID = as.character(GEM_ID)) %>%
  full_join(mean.samples)  %>%
  mutate(
    # SIR_Q = cut(SIR,
    #                                    breaks=quantile(SIR,
    #                                                    probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
    #                                    include.lowest = TRUE),
         Inc_Q = cut(Inc,
                     breaks=quantile(Inc,
                                     probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
                     include.lowest = TRUE),
         Fit_Q = cut(fit,
                     breaks=quantile(fit,
                                     probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
                     include.lowest = TRUE))
      
plot_excess<- ggplot()+
  # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
  #                 pattern_spacing = 0.03,pattern_size=0.5 )+
  geom_sf(data= bezirk_geo, aes(fill=   Fit_Q),alpha=1,col="black", size=0.1) +
  scale_fill_manual("",values = col8viridis) +
  scale_pattern_manual("significant",
                       breaks =c("0", "1"),
                       labels=c("no", "yes"),
                       values = c("none","circle"))+
  ggtitle("Relative excess deaths")+
  theme(
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom")
# cowplot::save_plot("output/plot_excess.pdf",plot_excess,base_height=12,base_width=10)




return(plot_excess)

}

