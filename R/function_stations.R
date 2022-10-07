function_stations <- function(Type,pandemic_start, pandemic_end,Pandemic_Name) {

data_stations <- read_sf("../data_raw/Railway_Stations_x/data/railway_stations_bern.shp") %>%
  rename(Gemeinde_Name =gdename ) %>%
  mutate( Gemeinde_Name = recode( Gemeinde_Name,"St. Beatenberg" ="Beatenberg",
                                  "St. Imier" ="St-Imier",
                                  "Gündlischwand"="Gründlischwand",
                                  "Tramelan-dessus" = "Tramelan",
                                  "Meirisberg" = "Meinisberg",
                                  "Martiswil" = "Madiswil",
                                  "Musligen" = "Merzligen",
                                  "Mettstetten" = "Mattstetten",
                                  "Seehof" = "Seedorf",
                                  "Büren" = "Büren an der Aare",
                                  "Büren z. Hof" = "Büren zum Hof",
                                  "Tramelan-dessous" = "Tramelan",
                                  "Les Genevez" = "Genevez",
                                  "Delsberg" = "Delémont",
                                  "Tüscherz-Alfermée" = "Tüscherz",
                                  "St. Ursanne" = "St-Ursanne",
                                  "St. Brais" = "St-Brais",
                                  "Tort" = "Port",
                                  "Niderwahlern" = "Niedermuhlern",
                                  "Thurnen" = "Kirchenthurnen",
                                  "Valbirse" = "Bévilard",
                                  "Haute-Sorne" = "Bassecourt",
                                  "Wichtrach" = "Niederwichtrach",
                                  "Teuffenthal" = "Unterlangenegg",
                                  "Montignez" = "Buix",
                                  "Sutz" = "Sutz-Lattrigen",
                                  "Hasliberg" = "Hasleberg",
                                  "Wiler" = "Wiler bei Utzenstorf",
                                  "Les Enfers" = "Enfers",
                                  "Walliswil" = "Walliswil b. Wangen",
                                  "Belpahorn" = "Belprahon",
                                  "Büren" = "Büren an der Aare",
                                  "Busswil" = "Busswil b. Büren",
                                  "Busswil bei Büren" = "Busswil b. Büren",
                                  "Busswil bei Melchnau" = "Busswil b. Melchnau",
                                  "Les Enfers" = "Enfers",
                                  "Niederried bei Interlaken" ="Niederried b. Interlaken",
                                  "Niederried bei Kallnach" = "Niederried b. Kallnach",
                                  "La Ferrière" = "Courtelary",
                                  "Montignez" = "Buix",
                                  "Niderwichtrach" = "Niederwichtrach",
                                  "Les Genevez" = "Genevez",
                                  "Oberwil" = "Oberwil b. Büren",
                                  "Langnau i. E." = "Langnau",
                                  "Büren a. A." = "Büren an der Aare",
                                  "Oberwil bei Büren" = "Oberwil b. Büren",
                                  "Busswil b. B." ="Busswil b. Büren",
                                  "Busswil bei Büren" ="Busswil b. Büren",
                                  "Herbligen" ="Steffisburg",
                                  "Oberried a. Brienzersee" = "Oberried",
                                  "Wangen a. A." ="Wangen",
                                  "Stalden i. E." = "Stalden",
                                  "Tramelan-Dessous" ="Tramelan",
                                  "Herbligen" ="Steffisburg",
                                  "Tramelan-Dessus" ="Tramelan",
                                  "Porrentruy" = "Pruntrut",
                                  "Sonviller" = "Sonvilier",
                                  "Sonceboz-Sombeval" = "Sonceboz",
                                  "Les Pommerals" ="Saignelégier",
                                  "Mallcray" =  "Malleray",
                                  "Röthenbach bei Herzogenbuchsee" = "Röthenbach im Emmenthal",
                                  "Rüti" = "Rüti b. Büren",
                                  "Brienz (BE)"  = "Brienz",
                                  "Biel (BE)"  = "Biel",
                                  "Roggwil (BE)" = "Roggwil",
                                  "Renan (BE)" = "Renan",
                                  "Lengnau (BE)" = "Lengnau",
                                  "Erlenbach im Simmental" = "Erlenbach",
                                  "Bargen (BE)"  ="Bargen",
                                  "Muri bei Bern" = "Muri",
                                  "Langnau im Emmental" = "Langnau",
                                  "Wangen an der Aare" = "Wangen",
                                  "Roggwil (BE)" ="Roggwil",
                                  "Saint-Ursanne" = "St-Ursanne",
                                  "Hasle bei Burgdorf" = "Hasle",
                                  "Le Noirmont" = "Noirmont",
                                  "Saint-Imier" = "St-Imier")) %>%
  select(Gemeinde_Name) %>%
  group_by(Gemeinde_Name) %>%
  tally() %>%
  data.frame() %>%
  select(Gemeinde_Name,n) %>%
  filter(!is.na(Gemeinde_Name))

load("../data/data_bern.RData")

data_inc <- data_bern %>%
  filter(date_week >=ymd(pandemic_start) & date_week <= ymd(pandemic_end))  %>%
  filter(!Gemeinde_Name=="Gstaad") %>%
  filter(!Gemeinde_Name=="ganzer Amtsbezirk") %>%
  filter(!Gemeinde_Name=="Wengen") %>%
  mutate( NumbCases  = ifelse(iso_week=="1918_28" & Gemeinde_Name =="Bern", 60, NumbCases),
          NumbCases  = ifelse(iso_week=="1918_29" & Gemeinde_Name =="Bern", 240,NumbCases)) %>%
  group_by(GEM_ID) %>%
  mutate(Sum_date = sum(NumbCases, na.rm=TRUE))%>%
  ungroup() %>%
  distinct(GEM_ID, .keep_all = TRUE) %>%
  left_join(data_stations) %>%
  mutate(Sum_Inc =Sum_date/Population*1000,
         GEM_ID = as.character(GEM_ID),
         stations_group = ifelse(is.na(n), "no", "yes"),
         tempx= 1)


if(Type=="Regression") {
  # poisson2 <- glm(Sum_date ~ TB_mor+offset(log(Population)),data = data_inc,  family=poisson)
  
  glmNB_r <-  robmixglm(Sum_date ~  stations_group+offset(log(Population)),data = data_inc, family="nbinom")
  summary(glmNB_r)
  
  est <-round(coef(glmNB_r)[2],6)
  se <- round(coefficients(summary(glmNB_r))[2,2],6)
  Cl <- est - 1.96*se
  Cu <- est + 1.96*se
  
  res <- data.frame(Pandemic=Pandemic_Name,est=est, Cl=Cl, Cu=Cu) %>%
    mutate(Var = row.names(.))%>%
    remove_rownames(.) %>%
    select(Pandemic, Var, est, Cl, Cu) 
  return(res)
  
}

else if(Type=="Figure") {
  plot_co <- ggplot(data=data_inc,aes(x=tempx, y=Sum_Inc, fill = stations_group)) +
    geom_split_violin() +
    stat_summary(fun = median,
                 width = 0.25,
                 position = position_dodge(width = .25),
                 colour = "black",
                 geom = "crossbar",
                 show.legend=FALSE) +
    scale_fill_manual("",values = c(16,15))  +
    ggtitle(Pandemic_Name)+
    ylab("Incidence")+
    xlab("") +
    theme_bw() +
    theme(aspect.ratio = 1,
          axis.text.y = element_text(color="black",size=axis_size),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title=element_text(size=size_axis_title),
          legend.text=element_text(size=legend_size),
          legend.title =element_text(size=legend_size_title),
          plot.title = element_text(size=plot_title_size ),
          legend.position = "bottom")
  
  return(plot_co)
  
  # cowplot::save_plot("output/plot_SEP.pdf",plot_SEP,ba
  
}

else if(Type=="Maps") {
  
  source("../R/bivariate_tmap_fabric.R")
  
  bezirk_geo <- read_sf("../data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
    mutate(GEM_ID = ifelse(Gemeinde=="MATTEN BEI INTERLAKEN", "243", GEM_ID)) %>%
    filter(!is.na(GEM_ID)) %>%
    mutate(GEM_ID = as.character(GEM_ID)) %>%
    select(GEM_ID, geometry) %>%
    full_join( data_inc)  %>%
    ungroup() %>%
    # select(geometry,TB_mor,Sum_Inc) %>%
    filter(!st_is_empty(.)) %>%
    mutate(Sum_Inc = ifelse(is.na(Sum_Inc), 0, Sum_Inc))%>%
    select(GEM_ID, geometry,Sum_Inc,n)
  
  bezirk_geo <-as(bezirk_geo, "Spatial")
  
  bivariate_choropleth(bezirk_geo, c("n", "Sum_Inc"), c("Stations", "Influenza"), bivmap_title="Relation Stations and Influenza")
  
}

}
