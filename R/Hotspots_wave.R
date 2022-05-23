function_hotspot <- function(Wave_Inf) {
  # load("data/fitted_values_1.RData")
  # fitted_values_1 <- mean.samples
  # load("data/fitted_values_2.RData")
  # fitted_values_2<- mean.samples
  # load("data/fitted_values_3.RData")
  # fitted_values_3 <- mean.samples
  
  load("../data/fitted_values_1.RData")
  fitted_values_1 <- mean.samples
  load("../data/fitted_values_2.RData")
  fitted_values_2<- mean.samples
  load("../data/fitted_values_3.RData")
  fitted_values_3 <- mean.samples
  
  mean.samples <- rbind(fitted_values_1, fitted_values_2, fitted_values_3) %>%
    ungroup() %>%
    dplyr::group_by(Region, Wave) %>%
    mutate(fit=sum(fit)) %>%
    ungroup() %>%
    mutate(fit = ifelse(fit=="Inf", Num, fit),
           Inc = as.numeric(round(fit/Population*100,2))) %>%
    filter(Wave == Wave_Inf)

# sf::sf_use_s2(TRUE)
  bezirk_geo <- read_sf("../data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
    mutate(GEM_ID = ifelse(Gemeinde=="MATTEN BEI INTERLAKEN", "243", GEM_ID)) %>%
    filter(!is.na(GEM_ID)) %>%
    mutate(GEM_ID = as.character(GEM_ID)) %>%
    full_join(mean.samples)  %>%
    ungroup() %>%
    filter(!Wave=="NA") %>%
    select(geometry,Inc) %>%
    mutate(Inc=ifelse(is.na(Inc), 0, Inc))
  
  if(Wave_Inf==2) {
  bezirk_geo <-   bezirk_geo %>%
    mutate(Inc=ifelse(Inc>200, 200, Inc))
  }
  else if(Wave_Inf==3) {
    bezirk_geo <-   bezirk_geo %>%
      mutate(Inc=ifelse(Inc>10, 10, Inc))
  }
  
  else if(Wave_Inf==1) {
    bezirk_geo <-   bezirk_geo
  }
  
# start here

  neighbours <- poly2nb(bezirk_geo$geometry)
  # listw <- nb2listw(neighbours)
  # gi.fixed <- localG(bezirk_geo$Inc, listw)
  # 

# print(nb2listw(neighbours = neighbours,  zero.policy = TRUE),  zero.policy = TRUE)
get.ZeroPolicyOption()
# [1] FALSE
set.ZeroPolicyOption(TRUE)
# [1] FALSE
get.ZeroPolicyOption()
# [1] TRUE
listw <- nb2listw(neighbours)

gi.fixed <- localG(bezirk_geo$Inc, listw)

# dnb_lw <- nb2listw(neighbours, style = 'B')
# gi.adapted <- localG(bezirk_geo$excess_percentage, dnb_lw)

bezirk_geo.gi <- cbind(bezirk_geo, as.matrix(gi.fixed)) %>%
  rename(gstat=as.matrix.gi.fixed.)


tm_shape(bezirk_geo.gi) +
  tm_fill(col = "gstat", 
          style = "pretty",
          palette="-RdBu",
          title = "local Gi",
          midpoint = 0) +
  tm_borders(alpha = 0.5) +
  tm_layout(
    main.title = paste0("Hot spot areas - Wave ", Wave_Inf), 
    main.title.position = "left")

}
