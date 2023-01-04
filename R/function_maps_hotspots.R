function_hotspot_wave <- function(pandemic_start, pandemic_end, Pandemic_Name) {
  load("data/data_bern.RData")

  data_region <- data_bern %>%
    ungroup() %>%
    filter(date_week >=ymd(pandemic_start) & date_week <= ymd(pandemic_end)) %>%
    filter(!Gemeinde_Name=="Gstaad") %>%
    filter(!Gemeinde_Name=="ganzer Amtsbezirk") %>%
    mutate( NumbCases  = ifelse(iso_week=="1918_28" & Gemeinde_Name =="Bern", 60, NumbCases),
            NumbCases  = ifelse(iso_week=="1918_29" & Gemeinde_Name =="Bern", 240,NumbCases)) %>%
    mutate(NumInc = NumbCases/Population*1000,
           iso_week = paste0(Year,"_",week)) %>%
    arrange(Year, week) %>%
    mutate(isoweek_date = paste0(Year,"-W",sprintf("%02d", week), "-4"),
           isoweek_date2 = ISOweek2date(isoweek_date),
           GEM_ID = as.character(GEM_ID)) %>%
    group_by(GEM_ID) %>%
    mutate(Sum_date = sum(NumbCases, na.rm=TRUE),
           Sum_Pop = sum(Population),
           Sum_Inc =Sum_date/Population*1000) %>%
    ungroup() %>%
    distinct(GEM_ID, .keep_all = TRUE)
  
# sf::sf_use_s2(TRUE)
  bezirk_geo <- read_sf("data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
    mutate(GEM_ID = ifelse(Gemeinde=="MATTEN BEI INTERLAKEN", "243", GEM_ID)) %>%
    filter(!is.na(GEM_ID)) %>%
    mutate(GEM_ID = as.character(GEM_ID)) %>%
    full_join( data_region)  %>%
    ungroup() %>%
    select(geometry,Sum_Inc) %>%
    filter(!st_is_empty(.)) %>%
    mutate(Sum_Inc = ifelse(is.na(Sum_Inc), 0, Sum_Inc))


  neighbours <- poly2nb(bezirk_geo$geometry)


get.ZeroPolicyOption()
set.ZeroPolicyOption(TRUE)
get.ZeroPolicyOption()
listw <- nb2listw(neighbours)

gi.fixed <- localG(bezirk_geo$Sum_Inc, listw)

bezirk_geo.gi <- cbind(bezirk_geo, as.matrix(gi.fixed)) %>%
  rename(gstat=as.matrix.gi.fixed.)

cita <- data.frame(city=c("Thun", "Biel","Bern"),
                   long = c( 614620,   585443,  600670),
                   lat = c(178664, 220664,199655))

cita_sf = st_as_sf(cita, coords = c('long', 'lat'), crs = st_crs(bezirk_geo.gi)$proj4string)


plot_hotspot_wave <- tm_shape(bezirk_geo.gi) +
  tm_fill(col = "gstat", 
          style = "pretty",
          palette="-RdBu",
          title = "local Gi",
          midpoint = 0) +
  tm_borders(alpha = 0.5) +
  tm_shape(cita_sf) +
  tm_dots(size = 0.5) +
  tm_text("city",size = 1.3, just = "top", ymod=0.8)+
  tm_layout(
    # main.title = "Hotspots",
    main.title = Pandemic_Name,
    main.title.position = "left",
    legend.text.size = legend_size_map,
    # legend.width = 5,
    # legend.height = 8,
    legend.position = c(0.75,0.5),
    legend.title.size=legend_size_title,
    main.title.size = main_size_map)

return(plot_hotspot_wave)

}
