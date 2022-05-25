function_hotspot_year <- function(Year_Inf) {
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
    dplyr::group_by(Region, Year) %>%
    mutate(fit=sum(fit)) %>%
    ungroup() %>%
    mutate(fit = ifelse(fit=="Inf", Num, fit),
           Inc = as.numeric(round(fit/Population*1000,2)),
           Inc_obs = as.numeric(round(Num/Population*1000,2))) %>%
    filter(Year == Year_Inf)

# sf::sf_use_s2(TRUE)
  bezirk_geo <- read_sf("../data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
    mutate(GEM_ID = ifelse(Gemeinde=="MATTEN BEI INTERLAKEN", "243", GEM_ID)) %>%
    filter(!is.na(GEM_ID)) %>%
    mutate(GEM_ID = as.character(GEM_ID)) %>%
    full_join(mean.samples)  %>%
    ungroup() %>%
    select(geometry,Inc_obs) 
  # %>%
  #   mutate(Inc=ifelse(is.na(Inc), 0, Inc),
  #          Inc=ifelse(Inc>50, 50, Inc)
  # 
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

gi.fixed <- localG(bezirk_geo$Inc_obs, listw)

# dnb_lw <- nb2listw(neighbours, style = 'B')
# gi.adapted <- localG(bezirk_geo$excess_percentage, dnb_lw)

bezirk_geo.gi <- cbind(bezirk_geo, as.matrix(gi.fixed)) %>%
  rename(gstat=as.matrix.gi.fixed.)


plot_hotspot <- tm_shape(bezirk_geo.gi) +
  tm_fill(col = "gstat", 
          style = "pretty",
          palette="-RdBu",
          title = "local Gi",
          midpoint = 0,
          legend.show = FALSE) +
  tm_borders(alpha = 0.5) +
  tm_layout(
    main.title = Year_Inf, 
    main.title.position = "left",
    legend.text.size = 2,
    main.title.size = 2)

return(plot_hotspot)
}

p1918 <- function_hotspot_year(Year_Inf=1918)
p1919 <- function_hotspot_year(Year_Inf=1919)
p1920 <- function_hotspot_year(Year_Inf=1920)
p1921 <- function_hotspot_year(Year_Inf=1921)
p1922 <- function_hotspot_year(Year_Inf=1922)
p1923 <- function_hotspot_year(Year_Inf=1923)
p1924 <- function_hotspot_year(Year_Inf=1924)
p1925 <- function_hotspot_year(Year_Inf=1925)
p1926 <- function_hotspot_year(Year_Inf=1926)

cowplot::plot_grid(tmap_grob(p1918), tmap_grob(p1919), tmap_grob(p1920), tmap_grob(p1921), tmap_grob(p1922), 
             tmap_grob(p1923), tmap_grob(p1924),tmap_grob(p1925), tmap_grob(p1926), nrow=3, ncol=3)
