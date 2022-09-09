function_plot_maps <- function(pandemic_start, pandemic_end, Pandemic_Name) {
  load("../data/data_bern.RData")

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
    mutate(Sum_date = sum(NumbCases),
           Sum_Pop = sum(Population),
           Sum_Inc =Sum_date/Sum_Pop*1000) %>%
    ungroup() %>%
    distinct(GEM_ID, .keep_all = TRUE)
  
# sf::sf_use_s2(TRUE)
  bezirk_geo <- read_sf("../data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
    mutate(GEM_ID = ifelse(Gemeinde=="MATTEN BEI INTERLAKEN", "243", GEM_ID)) %>%
    filter(!is.na(GEM_ID)) %>%
    mutate(GEM_ID = as.character(GEM_ID)) %>%
    full_join( data_region)  %>%
    ungroup() %>%
    select(geometry,Sum_Inc) %>%
    filter(!st_is_empty(.)) %>%
    mutate(Sum_Inc = ifelse(is.na(Sum_Inc), 0, Sum_Inc))
  
  
  data_maps <- st_as_sf(bezirk_geo)

  plot_maps <- tm_shape( data_maps ) + 
    tm_fill("Sum_Inc",
            palette = "Reds", 
            # legend.hist = TRUE,
            style = "jenks",
            # style = "kmeans",
            title = "Incidence") +
    tm_borders(alpha = 0.5)+
  tm_layout(
    main.title = "Incidence per 1'000 inhabitants",
    main.title.position = "left",
    legend.text.size = legend_size_map,
    # legend.width = 5,
    # legend.height = 8,
    legend.position = c(0.7,0.8),
    legend.title.size=legend_size_map,
    main.title.size = main_size_map)

return(  plot_maps)

}
