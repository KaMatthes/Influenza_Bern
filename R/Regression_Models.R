function_regression <- function(pandemic_start, pandemic_end){
  
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
    distinct(GEM_ID, .keep_all = TRUE) 
  
  
  data_cofactor <- read.csv("../data_raw/Cofactors_1918.csv", sep=";")  %>%
    # rename(Gemeinde_Name = Gemeinde) %>%
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
                                    "Rüti" = "Rüti b. Büren")) 
  

  data_tb <- read.xlsx("../data_raw/Tb_death.xlsx")  %>%
    # rename(Gemeinde_Name = Gemeinde) %>%
    rename(Gemeinde_Name = Gemeinde) %>%
    mutate( Gemeinde_Name = recode( Gemeinde_Name,"St. Beatenberg" ="Beatenberg",
                                    "St. Imier" ="St-Imier",
                                    "Aeschlen"  = "Oberdiessbach",
                                    "Gündlischwand"="Gründlischwand",
                                    "Tramelan-dessus" = "Tramelan",
                                    "Meirisberg" = "Meinisberg",
                                    "Martiswil" = "Madiswil",
                                    "Musligen" = "Merzligen",
                                    "Bickingen-Schwanden" ="Wynigen",
                                    "Bleiken" ="Oberdiessbach",
                                    "Bourrignon" ="Delémont",
                                    "Bözingen" ="Biel",
                                    "Busswil b. M." = "Busswil b. Melchnau",
                                    "Busswil im S." = "Busswil b. Büren",
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
                                    "Corban" ="Delémont",
                                    "Courchapoix" ="Delémont",
                                    "Ederswiler" = "Delémont",
                                    "Clavaleyres" = "Moutier",
                                    "Ebligen" ="Oberried",
                                    "Goldiwil" ="Thun",
                                    "Herbligen" ="Steffisburg",
                                    "La Ferrière" = "Courtelary",
                                    "Le Peuchapatte" = "Muriaux",
                                    "Madretsch" ="Biel",
                                    "Messen-Scheunen" ="Scheunen",
                                    "Mett" ="Biel",
                                    "Mötschwil-Schleumen" = "Mötschwil",
                                    "Hermiswil" ="Seeberg",
                                    "Niederried b. K." = "Niederried b. Kallnach",
                                    "Oberscheunen" ="Scheunen",
                                    "Oberwil b. B." = "Oberwil b. Büren",
                                    "Sonceboz-Sombeval" ="Sonceboz",
                                    "Strättligen" = "Thun",
                                    "Walliswil-Wangen" ="Walliswil b. Wangen",
                                    "Horrenbach-Buchen" = "Horrenbach",
                                    "Gäserz" ="Brüttelen",
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
    group_by(Gemeinde_Name) %>%
    mutate(TB=mean(TB_death_10000)) %>%
    ungroup() %>%
    distinct(Gemeinde_Name, .keep_all = TRUE) %>%
    select(Gemeinde_Name,TB)
  
  data_fabric <- read.xlsx("../data_raw/Fabrik_Statistik_1929.xlsx")  %>%
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
                                    "La Ferrière" = "Courtelary",
                                    "Montignez" = "Buix",
                                    "Niderwichtrach" = "Niederwichtrach",
                                    "Les Genevez" = "Genevez",
                                    "Oberwil" = "Oberwil b. Büren",
                                    "Langnau i. E." = "Langnau",
                                    "Büren a. A." = "Büren an der Aare",
                                    "Oberwil bei Büren" = "Oberwil b. Büren",
                                    "Busswil b. B." ="Busswil b. Büren",
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
                                    "Rüti" = "Rüti b. Büren")) %>%
    group_by(Gemeinde_Name) %>%
    mutate(Arbeiter=sum(Arbeiter.Total),
           PS=sum(PS),
           Betriebe=sum(Betriebe)) %>%
    ungroup() %>%
    distinct(Gemeinde_Name, .keep_all = TRUE) 
  
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
  
data_total <- data_inc %>%
  left_join(data_cofactor) %>%
  left_join(  data_tb) %>%
  left_join(  data_fabric) %>%
  left_join(    data_stations) %>%
  mutate(Sum_Inc =Sum_date/Population*1000,
         Betriebe=ifelse(is.na(Betriebe),0, Betriebe),
         Betriebe_prop= Betriebe/Population*100,
         PS=ifelse(is.na(PS),0, PS),
         PS_prop= PS/Population*100,
         Arbeiter=ifelse(is.na(Arbeiter),0, Arbeiter),
         Arbeiter_prop=Arbeiter/Population*100,
         Gew_prop = Gew/Population*100,
         Lws_prop = LWS/Population*100,
         Lws_prop = ifelse(Lws_prop>70, 55, Lws_prop),
         median_denspop = median(PopDens, na.rm=TRUE),
         dens_group = ifelse( PopDens <median_denspop, "small", "large" ),
         dens_group = factor( dens_group, levels = c("small", "large")),
         stations_group = ifelse(is.na(n), "no", "yes"),
         tempx= 1) 


Mod1 <- coef(summary(robmixglm(Sum_date ~   dens_group+offset(log(Population)),data = data_total, family="nbinom")))[1:2,]
Mod2 <- coef(summary(robmixglm(Sum_date ~   HöheüM+offset(log(Population)),data = data_total, family="nbinom")))[1:2,]
Mod3 <- coef(summary(robmixglm(Sum_date ~   TB+offset(log(Population)),data = data_total, family="nbinom")))[1:2,]
Mod4 <- coef(summary(robmixglm(Sum_date ~   Lws_prop+offset(log(Population)),data = data_total, family="nbinom")))[1:2,]
Mod5 <- coef(summary(robmixglm(Sum_date ~   Gew_prop+offset(log(Population)),data = data_total, family="nbinom")))[1:2,]
Mod6 <- coef(summary(robmixglm(Sum_date ~   Arbeiter_prop+offset(log(Population)),data = data_total, family="nbinom")))[1:2,]
Mod7 <- coef(summary(robmixglm(Sum_date ~   Betriebe_prop+offset(log(Population)),data = data_total, family="nbinom")))[1:2,]
Mod8 <- coef(summary(robmixglm(Sum_date ~   PS_prop+offset(log(Population)),data = data_total, family="nbinom")))[1:2,]
Mod9 <- coef(summary(robmixglm(Sum_date ~   stations_group+offset(log(Population)),data = data_total, family="nbinom")))[1:2,]


results_regression <- rbind(Mod1, Mod2,Mod3, Mod4, Mod5, Mod6, Mod7, Mod8, Mod9) %>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  filter( Cofactor=="dens_grouplarge" | Cofactor=="HöheüM" |   Cofactor=="TB"| 
            Cofactor=="Lws_prop" | Cofactor=="Gew_prop" |   Cofactor=="Arbeiter_prop"| 
            Cofactor=="Betriebe_prop" |   Cofactor=="PS_prop" |   Cofactor=="stations_groupyes") %>%
  mutate(est= round(exp(Estimate),5),
         Cl = round(exp(Estimate - 1.96* Std..Error),5),
         Cu = round(exp(Estimate + 1.96* Std..Error),5),
         Univariate = paste0(est," (",Cl,"-",Cu, ")"))%>%
  select(Cofactor, Univariate)


# results_regression <- coef(summary(robmixglm(Sum_date ~ dens_group+HöheüM+TB+stations_group+Betriebe_prop+Lws_prop+offset(log(Population)),data = data_total, family="nbinom")))[1:8,] %>%
#   data.frame(.) %>%
#   mutate( Cofactor = row.names(.)) %>%
#   filter(!Cofactor == "(Intercept)") %>%
#   mutate(est= round(exp(Estimate),5),
#          Cl = round(exp(Estimate - 1.96* Std..Error),5),
#          Cu = round(exp(Estimate + 1.96* Std..Error),5),
#          Multivariate = paste0(est," (",Cl,"-",Cu, ")"))%>%
#   select(Cofactor,  Multivariate) %>%
#   full_join(res_uni) %>%
#   select(Cofactor, Univariate, Multivariate)

return(results_regression)

# write.xlsx(results_regression,file=paste0("output/results_regression_",Pandemic_Name,".xlsx"),row.names=FALSE, overwrite = TRUE)

}

