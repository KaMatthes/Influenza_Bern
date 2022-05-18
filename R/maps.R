function_maps<- function(){
  
  load("data/fitted_values1918.RData")
  fitted_values1918 <- mean.samples
  load("data/fitted_values1919.RData")
  fitted_values1919 <- mean.samples
  load("data/fitted_values1920.RData")
  fitted_values1920 <- mean.samples
  load("data/fitted_values1921.RData")
  fitted_values1921 <- mean.samples
  load("data/fitted_values1922.RData")
  fitted_values1922 <- mean.samples
  load("data/fitted_values1923.RData")
  fitted_values1923 <- mean.samples
  load("data/fitted_values1924.RData")
  fitted_values1924 <- mean.samples
  load("data/fitted_values1925.RData")
  fitted_values1925 <- mean.samples
  load("data/fitted_values1926.RData")
  fitted_values1926 <- mean.samples
  
  mean.samples <- rbind(fitted_values1918, fitted_values1919, fitted_values1920,
                        fitted_values1921, fitted_values1922, fitted_values1923,
                        fitted_values1924, fitted_values1925, fitted_values1926) %>%
    ungroup() %>%
    mutate(fit = ifelse(fit=="Inf", Num, fit),
           Inc = fit/Population*10000,
           Inc_obs = Num/Population/10000) 
    
# sf::sf_use_s2(TRUE)

bezirk_geo <- read_sf("data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
  mutate(GEM_ID = as.character(GEM_ID)) %>%
  full_join(mean.samples)  %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    # SIR_Q = cut(SIR,
    #                                    breaks=quantile(SIR,
    #                                                    probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
    #                                    include.lowest = TRUE),
         Inc_Q = cut(Inc,
                     breaks=quantile(Inc,
                                     probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
                     include.lowest = TRUE)) %>%
  ungroup() %>%
  mutate(Inc_Q= recode(Inc_Q,
                                    "[2.9e-39,460]" = "Q1",
                                    "(460,930]" = "Q2",
                                    "(930,1.43e+03]" = "Q3",
                                    "(1.43e+03,2.16e+03]" = "Q4",
                                    "(2.16e+03,6.58e+03]" = "Q5",
                                    
                                    "[0,208]" = "Q1",
                                    "(208,310]" = "Q2",
                                    "(310,400]" = "Q3",
                                    "(400,574]" = "Q4",
                                    "(574,1e+238]" = "Q5",
                                    
                                    "[3.48e-05,152]" = "Q1",
                                    "(152,208]" = "Q2",
                                    "(208,270]" = "Q3",
                                    "(270,359]" = "Q4",
                                    "(359,1.2e+87]" = "Q5",
                                    
                                    "[20.02,20.23]" = "Q1",
                                    "(20.23,20.27]" = "Q2",
                                    "(20.27,20.32]" = "Q3",
                                    "(20.32,20.36]" = "Q4",
                                    "(20.36,5.368e+44]" = "Q5",
                                    
                                    "[0,55.3]" = "Q1",
                                    "(55.3,102]" = "Q2",
                                    "(102,137]" = "Q3",
                                    "(137,190]" = "Q4",
                                    "(190,1.6e+133]" = "Q5",
                                    
                                    "[18.42,18.59]" = "Q1",
                                    "(18.59,18.64]" = "Q2",
                                    "(18.64,18.69]" = "Q3",
                                    "(18.69,18.74]" = "Q4",
                                    "(18.74,6.875e+36]" = "Q5",
                                    
                                    "[68.22,100.2]" = "Q1",
                                    "(100.2,100.5]" = "Q2",
                                    "(100.5,100.8]" = "Q3",
                                    "(100.8,101.1]" = "Q4",
                                    "(101.1,8.743e+117]" = "Q5",
                                    
                                    "[0,44.3]" = "Q1",
                                    "(44.3,58]" = "Q2",
                                    "(58,87.6]" = "Q3",
                                    "(87.6,118]" = "Q4",
                                    "(118,7.29e+206]" = "Q5",
                                    
                                    "[33.82,34.18]" = "Q1",
                                    "(34.18,34.28]" = "Q2",
                                    "(34.28,34.35]" = "Q3",
                                    "(34.35,34.45]" = "Q4",
                                    "(34.45,7.208e+101]" = "Q5")) %>%
  ungroup() %>%
  filter(!Year=="NA") %>%
  mutate(
    # SIR_Q = cut(SIR,
    #                                    breaks=quantile(SIR,
    #                                                    probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
    #                                    include.lowest = TRUE),
    Inc_Q_t = cut(Inc,
                breaks=quantile(Inc,
                                probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
                include.lowest = TRUE))
      
plot_excess <- ggplot()+
  # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
  #                 pattern_spacing = 0.03,pattern_size=0.5 )+
  geom_sf(data= bezirk_geo, aes(fill=   Inc_obs),alpha=1,col="black", size=0.1) +
  facet_wrap(~Year, ncol=3) +
  scale_fill_manual("",values = col5viridis) +
  ggtitle("Incidence")+
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

