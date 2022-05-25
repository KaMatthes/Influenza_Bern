function_maps_all <- function(){
  # 
  # load("data/fitted_values1918.RData")
  # fitted_values1918 <- mean.samples
  # load("data/fitted_values1919.RData")
  # fitted_values1919 <- mean.samples
  # load("data/fitted_values1920.RData")
  # fitted_values1920 <- mean.samples
  # load("data/fitted_values1921.RData")
  # fitted_values1921 <- mean.samples
  # load("data/fitted_values1922.RData")
  # fitted_values1922 <- mean.samples
  # load("data/fitted_values1923.RData")
  # fitted_values1923 <- mean.samples
  # load("data/fitted_values1924.RData")
  # fitted_values1924 <- mean.samples
  # load("data/fitted_values1925.RData")
  # fitted_values1925 <- mean.samples
  # load("data/fitted_values1926.RData")
  # fitted_values1926 <- mean.samples

  load("../data/fitted_values1918.RData")
  fitted_values1918 <- mean.samples
  load("../data/fitted_values1919.RData")
  fitted_values1919 <- mean.samples
  load("../data/fitted_values1920.RData")
  fitted_values1920 <- mean.samples
  load("../data/fitted_values1921.RData")
  fitted_values1921 <- mean.samples
  load("../data/fitted_values1922.RData")
  fitted_values1922 <- mean.samples
  load("../data/fitted_values1923.RData")
  fitted_values1923 <- mean.samples
  load("../data/fitted_values1924.RData")
  fitted_values1924 <- mean.samples
  load("../data/fitted_values1925.RData")
  fitted_values1925 <- mean.samples
  load("../data/fitted_values1926.RData")
  fitted_values1926 <- mean.samples
  
  mean.samples <- rbind(fitted_values1918, fitted_values1919, fitted_values1920,
                        fitted_values1921, fitted_values1922, fitted_values1923,
                        fitted_values1924, fitted_values1925, fitted_values1926) %>%
    ungroup() %>%
    mutate(fit = ifelse(fit=="Inf", Num, fit),
           Inc = fit/Population*1000,
           Inc_obs = Num/Population*1000,
           SIR = Inc/Inc_obs,
           SIR = ifelse(is.na(SIR), 0, SIR),
           SIR = ifelse(SIR=="Inf", 10, SIR),
           SIR = ifelse(SIR>10, 10, SIR))
  
# sf::sf_use_s2(TRUE)
  

    bezirk_geo <- read_sf("../data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
      mutate(GEM_ID = as.character(GEM_ID)) %>%
      full_join(mean.samples)  %>%
      ungroup() %>%
      mutate(
        # Inc_Q = cut(Inc,
        #             breaks=quantile(Inc,
        #                             probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
        #             include.lowest = TRUE),
        Inc_obs_Q = cut(Inc_obs,
                        breaks = c( -Inf,0,5,10, 20, 50,100,200, 400,Inf), 
                        labels = c("0", ">0-5", ">5-10",
                                   ">10-20", ">20-50",  ">50-100",
                                   ">100-200",">200-400",">400"),
                        include.lowest = TRUE, right = TRUE)) %>%
      # SIR_Q = cut(SIR,breaks = c(0, 1,1.1,1.3,1.4,1.5,2,2.5, Inf), 
      #               # labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
      #               #            "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
      #               #            "[25%, 30%)","[30%, 35%)",">35"),
      #               include.lowest = TRUE, right = FALSE))

      ungroup() %>%
      mutate( Inc_obs_Q = as.character( Inc_obs_Q),
              Inc_obs_Q = ifelse(is.na(Inc_obs_Q),"no data", Inc_obs_Q))
    
    no_data1 <-   bezirk_geo %>%
      filter(Inc_obs_Q=="no data") %>%
      mutate(Year="1918")
    
    no_data2 <-   bezirk_geo %>%
      filter(Inc_obs_Q=="no data") %>%
      mutate(Year="1919")
    
    no_data3 <-   bezirk_geo %>%
      filter(Inc_obs_Q=="no data") %>%
      mutate(Year="1920")
  
    no_data4 <-   bezirk_geo %>%
      filter(Inc_obs_Q=="no data") %>%
      mutate(Year="1921")
    
    no_data5 <-   bezirk_geo %>%
      filter(Inc_obs_Q=="no data") %>%
      mutate(Year="1922")
    
    no_data6 <-   bezirk_geo %>%
      filter(Inc_obs_Q=="no data") %>%
      mutate(Year="1923")
    
    no_data7 <-   bezirk_geo %>%
      filter(Inc_obs_Q=="no data") %>%
      mutate(Year="1924")
    
    no_data8 <-   bezirk_geo %>%
      filter(Inc_obs_Q=="no data") %>%
      mutate(Year="1925")
    
    no_data9 <-   bezirk_geo %>%
      filter(Inc_obs_Q=="no data") %>%
      mutate(Year="1926")
    
    no_data <- rbind(no_data1,no_data2,no_data3,no_data4,no_data5,no_data6,no_data7,no_data8,no_data9 )
    
    bezirk_geo <- bezirk_geo %>%
      filter(!Inc_obs_Q=="no data") %>%
      rbind(no_data) %>%
      mutate(Inc_obs_Q = factor(Inc_obs_Q, levels=c("0",">0-5", ">5-10",">10-20",">20-50", ">50-100",">100-200",">200-400",">400", "no data")))
    
plot_incidence <- ggplot()+
  # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
  #                 pattern_spacing = 0.03,pattern_size=0.5 )+
  geom_sf(data= bezirk_geo, aes(fill= Inc_obs_Q),alpha=1,col="black", size=0.1) +
  facet_wrap(~Year, ncol=3) +
  scale_fill_manual("",values = c("#C6DBEF",col8viridis, "grey"))+
  ggtitle("Incidence")+
  theme(
    panel.grid.major=element_blank(),
    strip.text.x=element_text(size=40),
    legend.text=element_text(size=40),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom")
# cowplot::save_plot("output/plot_excess.pdf",plot_excess,base_height=12,base_width=10)

return(plot_incidence)
}

