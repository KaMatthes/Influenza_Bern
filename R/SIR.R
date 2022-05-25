function_maps <- function(Year_Inf){

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

  # load("../data/fitted_values1918.RData")
  # fitted_values1918 <- mean.samples
  # load("../data/fitted_values1919.RData")
  # fitted_values1919 <- mean.samples
  # load("../data/fitted_values1920.RData")
  # fitted_values1920 <- mean.samples
  # load("../data/fitted_values1921.RData")
  # fitted_values1921 <- mean.samples
  # load("../data/fitted_values1922.RData")
  # fitted_values1922 <- mean.samples
  # load("../data/fitted_values1923.RData")
  # fitted_values1923 <- mean.samples
  # load("../data/fitted_values1924.RData")
  # fitted_values1924 <- mean.samples
  # load("../data/fitted_values1925.RData")
  # fitted_values1925 <- mean.samples
  # load("../data/fitted_values1926.RData")
  # fitted_values1926 <- mean.samples
  
  mean.samples <- rbind(fitted_values1918, fitted_values1919, fitted_values1920,
                        fitted_values1921, fitted_values1922, fitted_values1923,
                        fitted_values1924, fitted_values1925, fitted_values1926) %>%
    ungroup() %>%
    mutate(fit = ifelse(fit=="Inf", Num, fit),
           Inc = fit/Population*1000,
           Inc_obs = Num/Population*1000,
           SIR = Inc/Inc_obs,
           SIR = ifelse(is.na(SIR), 0, SIR),
           SIR = ifelse(SIR=="Inf", 25, SIR)) %>%
    filter(Year==Year_Inf) 
  
# sf::sf_use_s2(TRUE)
  
  
if(Year_Inf == 1918) {
    bezirk_geo <- read_sf("data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
      mutate(GEM_ID = as.character(GEM_ID)) %>%
      full_join(mean.samples)  %>%
      ungroup() %>%
      mutate(
        # Inc_Q = cut(Inc,
        #             breaks=quantile(Inc,
        #                             probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
        #             include.lowest = TRUE),
        # Inc_obs_Q = cut(Inc_obs,
        #                 breaks = c( -Inf,0,50, 100, 150, 200, 250,300, 400,Inf),
        #                 labels = c("0", ">0-50", ">50 - 100",
        #                            ">100-150", ">150-200",  ">200- 250",
        #                            ">250-300",">300-400",">400"),
        #                 include.lowest = TRUE, right = TRUE)) %>%
      SIR_Q = cut(SIR,  breaks = c(0, 1, 1.1, 1.2, 1.3, 1.4,2, 4,Inf),
                  labels = c("0", ">0-50", ">50 - 100",
                             ">100-150", ">150-200",  ">200- 250",
                             ">250-300",">300-400",">400"),
                  include.lowest = TRUE, right = TRUE)) %>%
      ungroup() 
  }
else if(Year_Inf == 1919) {
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
                         breaks = c( -Inf,0,10, 20, 30, 40, 50,100, 200,Inf), 
                         labels = c("0", ">0-10", ">10 - 20",
                                    ">20-30", ">30-40",  ">40- 50",
                                    ">50-100",">100-200",">200"),
                         include.lowest = TRUE, right = TRUE)) %>%
         # SIR_Q = cut(SIR,breaks = c(0, 1,1.1,1.3,1.4,1.5,2,2.5, Inf), 
         #               # labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
         #               #            "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
         #               #            "[25%, 30%)","[30%, 35%)",">35"),
         #               include.lowest = TRUE, right = FALSE)) %>%

  ungroup() 
}
 
 
  else if(Year_Inf == 1920) {
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
                        breaks = c( -Inf,0,5,10, 15, 20, 25,40,50,Inf), 
                        labels = c("0", ">0-5", ">5 - 10",
                                   ">10-15", ">15-20",  ">20- 25",
                                   ">25-40",">40-50",">50"),
                        include.lowest = TRUE, right = TRUE)) %>%
      # SIR_Q = cut(SIR,breaks = c(0, 1,1.1,1.3,1.4,1.5,2,2.5, Inf), 
      #               # labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
      #               #            "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
      #               #            "[25%, 30%)","[30%, 35%)",">35"),
      #               include.lowest = TRUE, right = FALSE)) %>%
      
      ungroup() 
  }
  
  else if(Year_Inf == 1921) {
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
                        breaks = c( -Inf,0,1,2, 3, 4, 5,6,7,Inf), 
                        labels = c("0", ">0-1", ">1 - 2",
                                   ">2-3", ">3-4",  ">4-5",
                                   ">5-6",">6-7",">7"),
                        include.lowest = TRUE, right = TRUE)) %>%
      # SIR_Q = cut(SIR,breaks = c(0, 1,1.1,1.3,1.4,1.5,2,2.5, Inf), 
      #               # labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
      #               #            "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
      #               #            "[25%, 30%)","[30%, 35%)",">35"),
      #               include.lowest = TRUE, right = FALSE)) %>%
      
      ungroup() 
  }
  
  else if(Year_Inf == 1922) {
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
                        breaks = c( -Inf,0,2,4,10,20,30,40,80,Inf), 
                        labels = c("0", ">0-2", ">2-4",
                                   ">4-10", ">10-20",">20-30",
                                   ">30-40",">40-80",">80"),
                        include.lowest = TRUE, right = TRUE)) %>%
      # SIR_Q = cut(SIR,breaks = c(0, 1,1.1,1.3,1.4,1.5,2,2.5, Inf), 
      #               # labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
      #               #            "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
      #               #            "[25%, 30%)","[30%, 35%)",">35"),
      #               include.lowest = TRUE, right = FALSE)) %>%
      
      ungroup() 
  }
  
  
  else if(Year_Inf == 1923) {
    bezirk_geo <- read_sf("../data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
      mutate(GEM_ID = as.character(GEM_ID)) %>%
      full_join(mean.samples)  %>%
      ungroup() %>%
      mutate(
        # Inc_Q = cut(Inc,
        #             breaks=quantile(Inc,
        #                             probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
        #             include.lowest = TRUE),
        #             include.lowest = TRUE),
        Inc_obs_Q = cut(Inc_obs,
                        breaks = c( -Inf,0,0.5,1,2, 3, 4, 5,6,Inf), 
                        labels = c("0", ">0-0.5", ">0.5 - 1",
                                   ">1-2", ">2-3",  ">3-4",
                                   ">4-5",">5-6",">6"),
                        include.lowest = TRUE, right = TRUE)) %>%
      # SIR_Q = cut(SIR,breaks = c(0, 1,1.1,1.3,1.4,1.5,2,2.5, Inf), 
      #               # labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
      #               #            "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
      #               #            "[25%, 30%)","[30%, 35%)",">35"),
      #               include.lowest = TRUE, right = FALSE)) %>%
      
      ungroup() 
  }
  
  else if(Year_Inf == 1924) {
    bezirk_geo <- read_sf("../data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
      mutate(GEM_ID = as.character(GEM_ID)) %>%
      full_join(mean.samples)  %>%
      ungroup() %>%
      mutate(
        # Inc_Q = cut(Inc,
        #             breaks=quantile(Inc,
        #                             probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
        #             include.lowest = TRUE),
        #             include.lowest = TRUE),
        Inc_obs_Q = cut(Inc_obs,
                        breaks = c( -Inf,0,1,2,4,8,10,15,20,Inf), 
                        labels = c("0", ">0-1", ">1-2",
                                   ">2-4", ">4-8",">8-10",
                                   ">10-15",">15-20",">20"),
                        include.lowest = TRUE, right = TRUE)) %>%
      # SIR_Q = cut(SIR,breaks = c(0, 1,1.1,1.3,1.4,1.5,2,2.5, Inf), 
      #               # labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
      #               #            "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
      #               #            "[25%, 30%)","[30%, 35%)",">35"),
      #               include.lowest = TRUE, right = FALSE)) %>%
      
      ungroup() 
  }
  
  else if(Year_Inf == 1925) {
    bezirk_geo <- read_sf("../data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
      mutate(GEM_ID = as.character(GEM_ID)) %>%
      full_join(mean.samples)  %>%
      ungroup() %>%
      mutate(
        # Inc_Q = cut(Inc,
        #             breaks=quantile(Inc,
        #                             probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
        #             include.lowest = TRUE),
        #             include.lowest = TRUE),
        Inc_obs_Q = cut(Inc_obs,
                        breaks = c( -Inf,0,2,4,8,10,15,20,100,Inf), 
                        labels = c("0", ">0-2", ">2-4",
                                   ">4-8", ">8-10",">10-15",
                                   ">15-20",">20-100",">100"),
                        include.lowest = TRUE, right = TRUE)) %>%
      # SIR_Q = cut(SIR,breaks = c(0, 1,1.1,1.3,1.4,1.5,2,2.5, Inf), 
      #               # labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
      #               #            "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
      #               #            "[25%, 30%)","[30%, 35%)",">35"),
      #               include.lowest = TRUE, right = FALSE)) %>%
      
      ungroup() 
  }
  
  
  else if(Year_Inf == 1926) {
    bezirk_geo <- read_sf("../data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
      mutate(GEM_ID = as.character(GEM_ID)) %>%
      full_join(mean.samples)  %>%
      ungroup() %>%
      mutate(
        # Inc_Q = cut(Inc,
        #             breaks=quantile(Inc,
        #                             probs = seq(0, 1, length.out = no_classes_map + 1), na.rm=TRUE),
        #             include.lowest = TRUE),
        #             include.lowest = TRUE),
        Inc_obs_Q = cut(Inc_obs,
                        breaks = c( -Inf,0,0.5,1,2, 3, 4, 5,6,Inf), 
                        labels = c("0", ">0-0.5", ">0.5 - 1",
                                   ">1-2", ">2-3",  ">3-4",
                                   ">4-5",">5-6",">6"),
                        include.lowest = TRUE, right = TRUE)) %>%
      # SIR_Q = cut(SIR,breaks = c(0, 1,1.1,1.3,1.4,1.5,2,2.5, Inf), 
      #               # labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
      #               #            "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
      #               #            "[25%, 30%)","[30%, 35%)",">35"),
      #               include.lowest = TRUE, right = FALSE)) %>%
      
      ungroup() 
  }
  
plot_incidence <- ggplot()+
  # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
  #                 pattern_spacing = 0.03,pattern_size=0.5 )+
  geom_sf(data= bezirk_geo, aes(fill= SIR_Q),alpha=1,col="black", size=0.1) +
  # facet_wrap(~Year, ncol=3) +
  scale_fill_manual("",values = c("#C6DBEF",col8viridis))+
  ggtitle(Year_Inf)+
  theme(
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.text=element_text(size=15),
    legend.position = "bottom")
# cowplot::save_plot("output/plot_excess.pdf",plot_excess,base_height=12,base_width=10)

return(plot_incidence)
}

p1918 <- function_maps(Year_Inf=1918)
p1919 <- function_maps(Year_Inf=1919)
p1920 <- function_maps(Year_Inf=1920)
p1921 <- function_maps(Year_Inf=1921)
p1922 <- function_maps(Year_Inf=1922)
p1923 <- function_maps(Year_Inf=1923)
p1924 <- function_maps(Year_Inf=1924)
p1925 <- function_maps(Year_Inf=1925)
p1926 <- function_maps(Year_Inf=1926)

cowplot::plot_grid(p1918, p1919, p1920, p1921, p1922, p1923, p1924,p1925, p1926, nrow=3, ncol=3)
