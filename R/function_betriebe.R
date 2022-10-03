function_betriebe <- function(Type) {

data_betriebe <- read.csv("../data_raw/Fabrik_Statistik_1929.csv", sep=";")  %>%
  rename(Gemeinde_Name = Gemeinde) %>%
  # rename(Gemeinde_Name = Gemeinde) %>%
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
                                  "Oberwil" = "Oberwil b. Büren",
                                  "Oberwil bei Büren" = "Oberwil b. Büren",
                                  "Röthenbach bei Herzogenbuchsee" = "Röthenbach im Emmenthal",
                                  "Rüti" = "Rüti b. Büren")) %>%
  select(Gemeinde_Name, Betriebe)

load("../data/data_bern.RData")

data_inc <- data_bern %>%
  filter(Year==1918 | Year==1919) %>%
  filter(!Gemeinde_Name=="Gstaad") %>%
  filter(!Gemeinde_Name=="ganzer Amtsbezirk") %>%
  filter(!Gemeinde_Name=="Wengen") %>%
  mutate( NumbCases  = ifelse(iso_week=="1918_28" & Gemeinde_Name =="Bern", 60, NumbCases),
          NumbCases  = ifelse(iso_week=="1918_29" & Gemeinde_Name =="Bern", 240,NumbCases)) %>%
  group_by(GEM_ID) %>%
  mutate(Sum_date = sum(NumbCases, na.rm=TRUE))%>%
  ungroup() %>%
  distinct(GEM_ID, .keep_all = TRUE) %>%
  left_join(data_betriebe ) %>%
  mutate(Sum_Inc =Sum_date/Population*1000,
         GEM_ID = as.character(GEM_ID),
         Betriebe=ifelse(is.na(Betriebe),0, Betriebe),
         Betriebe_prop= Betriebe/Population*1000,
         Betriebe_group2= ifelse(Betriebe==0, 0, 1),
         Betriebe_group2 = as.factor(Betriebe_group2))
         

if(Type=="Regression") {
# poisson2 <- glm(Sum_date ~ TB_mor+offset(log(Population)),data = data_inc,  family=poisson)
glmNB <- glm.nb(Sum_date ~    Betriebe_prop+offset(log(Population)),data = data_inc, link = "log")
summary(glmNB)
}

else if(Type=="Figure") {
plot_co <- ggplot(data=data_inc) +
  geom_point(aes(x= Betriebe_prop, y= Sum_Inc), lwd=lwd_size_points ) +
  geom_smooth(aes(x= Betriebe_prop,y= Sum_Inc),  method='lm',se=TRUE,lwd=lwd_size, col=col_line) +
  ggtitle(" Relation Altitude vs Influenza")+
  ylab("Incidence per 1'000 inhabitants")+
  xlab("Betriebe per 1'000 inhabitants") +
  xlim(0,8) +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=axis_size),
        plot.title = element_text(size=size_title))

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
    mutate(Sum_Inc = ifelse(is.na(Sum_Inc), 0, Sum_Inc)) %>%
    select(GEM_ID, geometry,Sum_Inc,Betriebe_prop)
  
  bezirk_geo <-as(bezirk_geo, "Spatial")
  
  bivariate_choropleth(bezirk_geo, c("Betriebe_prop", "Sum_Inc"), c("Betriebe", "Influenza"), bivmap_title="Relation Betriebe and Influenza")
  
}

}