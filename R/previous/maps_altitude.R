source("R/bivariate_tmap.R")
data_altitud <- read.csv("data_raw/Cofactors_1918.csv", sep=";")  %>%
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
  select(GEM_ID,Gemeinde_Name, HöheüM) %>%
  add_row(GEM_ID=243, Gemeinde_Name="Matten", HöheüM=575)

load("data/data_bern.RData")

data_inc <- data_bern %>%
  filter(Year==1918 | Year==1919) %>%
  filter(!Gemeinde_Name=="Gstaad") %>%
  filter(!Gemeinde_Name=="ganzer Amtsbezirk") %>%
  filter(!Gemeinde_Name=="Wengen") %>%
  mutate( NumbCases  = ifelse(iso_week=="1918_28" & Gemeinde_Name =="Bern", 60, NumbCases),
          NumbCases  = ifelse(iso_week=="1918_29" & Gemeinde_Name =="Bern", 240,NumbCases)) %>%
  group_by(GEM_ID) %>%
  mutate(Sum_date = sum(NumbCases)) %>%
  ungroup() %>%
  distinct(GEM_ID, .keep_all = TRUE) %>%
  left_join(data_altitud ) %>%
  mutate(Sum_Inc =Sum_date/Population*1000,
         GEM_ID = as.character(GEM_ID))


bezirk_geo <- read_sf("data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
  mutate(GEM_ID = ifelse(Gemeinde=="MATTEN BEI INTERLAKEN", "243", GEM_ID)) %>%
  filter(!is.na(GEM_ID)) %>%
  mutate(GEM_ID = as.character(GEM_ID)) %>%
  select(GEM_ID, geometry) %>%
  full_join( data_inc)  %>%
  ungroup() %>%
  # select(geometry,TB_mor,Sum_Inc) %>%
  filter(!st_is_empty(.)) %>%
  mutate(Sum_Inc = ifelse(is.na(Sum_Inc), 0, Sum_Inc))%>%
  select(GEM_ID, geometry,Sum_Inc,HöheüM)

bezirk_geo <-as(bezirk_geo, "Spatial")

bivariate_choropleth(bezirk_geo, c("HöheüM", "Sum_Inc"), c("Altitude", "Influenza incidence"), bivmap_title="Relation Altitude and Influenza")

  
  

