gemeinde <- read.xlsx("data_raw/Daten_SpanischeGrippe.xlsx",detectDates = TRUE, sheet="Gemeinden") %>%
  select(GEM_ID, Gemeinde_Name, Bez_Name, Bez_ID, Region_ID, Region_Name, E, N, Population =Wohnb)


data_s <- read.xlsx("data_raw/Masterarbeit_Rohdaten_Gina_23_3_17.xlsx", sheet="Rohdaten",detectDates = TRUE) %>%
  rename(Year = Jahr,
         Med_DIN_Week = Date_2,
         NumbCases =Anzahl_2,
         DummyNumbCasesAdjust = korrAnzDummy,
         Gemeinde_Name=Gemeinde_korr) %>%
  mutate(DummyNumbCasesAdjust=ifelse(is.na(DummyNumbCasesAdjust), 0, DummyNumbCasesAdjust),
         Gemeinde_Name = ifelse(Gemeinde_Name==" Bern", "Bern", Gemeinde_Name),
         Gemeinde_Name = ifelse(Gemeinde_Name=="Bern ", "Bern", Gemeinde_Name),
         Wochennummer = ifelse(Year==1918 & Wochennummer == 53, 1,Wochennummer),
         Year = ifelse(Year==1918 & Wochennummer== 53, 1919, Year),
         Wochennummer = ifelse(Year==1919 & Wochennummer == 53, 1,Wochennummer),
         Year = ifelse(Year==1919 & Wochennummer== 53, 1920, Year))%>%
  # filter(!DummyNumbCasesAdjust==1) %>%
  arrange(Year, Gemeinde_Name,Wochennummer) %>%
  # group_by(Year,District,BEZ_ID, Municipality, GEM_ID) %>%
  # complete(week, fill=list(n=0))
  # complete( week,  GEM_ID, nesting(Year)) %>%
  complete(Wochennummer,  Gemeinde_Name, nesting(Year)) %>%
  filter(!is.na(Wochennummer)) %>%
  filter(!is.na(Year)) %>%
 mutate( NumbCases=ifelse(is.na(NumbCases), 0, NumbCases),
         iso_week = paste0(Year,"_",Wochennummer),
         iso_week2 = sprintf("%02d",as.numeric(Wochennummer)),
         iso_week3= paste0("W",iso_week2),
         iso_week_year = paste0(Year,"-", iso_week3,"-1"),
         date_week =  ISOweek2date(iso_week_year) +4,
         week=as.numeric(substr(iso_week, start = 6, stop = 7))) %>%
           # select(-Med_DIN_Week, -Gemeinde_Name) %>%
  mutate(NumbCases = as.numeric(NumbCases)) %>%
  # filter(Disease=="Influenza") %>%
  # mutate(Disease = ifelse(Disease=="influenza", "Influenza", Disease)) %>%
  # select(Year,Med_DIN_Week,NumbCases=NumbCasesAdjust2,GEM_ID,Gemeinde_Name="Place_korr") %>%
  select(Year,iso_week,NumbCases,Gemeinde_Name,iso_week_year,date_week) %>%
  # group_by(Year,Med_DIN_Week,GEM_ID,Gemeinde_Name) %>%
  group_by(iso_week,Gemeinde_Name) %>%
  mutate(NumbCases = sum(NumbCases)) %>%
  # distinct(Year, Med_DIN_Week, GEM_ID, .keep_all = TRUE) %>%
  distinct(Year, iso_week, Gemeinde_Name, .keep_all = TRUE) %>%
  ungroup()  %>%
  mutate(Gemeinde_Name= recode(Gemeinde_Name, "Belpahorn" = "Belprahon",
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

data_bern <- read.xlsx("data_raw/Influenza_Bern_korrigiert.xlsx",detectDates = TRUE) %>%
  mutate(Krankheit2 = ifelse(Krankheit2=="influenza", "Influenza", Krankheit2)) %>%
  filter(Krankheit2=="Influenza") %>%
  select(Jahr, Monat, Day_early, Anzahl3, Gemeinde3) %>%
  rename(Year=Jahr,
         NumbCases=Anzahl3,
         Gemeinde_Name =  Gemeinde3) %>%
  mutate(Wochennummer = isoweek(Day_early)) %>%
  complete(Wochennummer,  Gemeinde_Name, nesting(Year)) %>%
  
  mutate(  Wochennummer = ifelse(Year==1921 & Wochennummer == 53, 1,Wochennummer),
           Year = ifelse(Year==1921 & Wochennummer== 53, 1922, Year),
           Wochennummer = ifelse(Year==1922 & Wochennummer == 53, 1,Wochennummer),
           Year = ifelse(Year==1922 & Wochennummer== 53, 1923, Year),
           Wochennummer = ifelse(Year==1923 & Wochennummer == 53, 1,Wochennummer),
           Year = ifelse(Year==1923 & Wochennummer== 53, 1924, Year),
           Wochennummer = ifelse(Year==1924 & Wochennummer == 53, 1,Wochennummer),
           Year = ifelse(Year==1924 & Wochennummer== 53, 1925, Year),
         iso_week = paste0(Year,"_",Wochennummer),
         iso_week2 = sprintf("%02d",as.numeric(Wochennummer)),
         iso_week3= paste0("W",iso_week2),
         iso_week_year = paste0(Year,"-", iso_week3,"-1"),
         date_week =  ISOweek2date(iso_week_year) +4) %>%
  filter(Year!=1926) %>%
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
  select(-Monat, -Day_early) %>%
  # full_join(gemeinde[,c("Gemeinde_Name", "GEM_ID")] )  %>%
  # group_by(Year,Med_DIN_Week,GEM_ID,Gemeinde_Name) %>%
  full_join(gemeinde)  %>%
  group_by(Year,iso_week,NumbCases,Gemeinde_Name) %>%
  mutate(NumbCases = sum(as.numeric(NumbCases))) %>%
  # distinct(Year, Med_DIN_Week, GEM_ID, .keep_all = TRUE) %>%
  distinct(Year, iso_week, Gemeinde_Name, .keep_all = TRUE) %>%
  ungroup() %>%
  select(Year, NumbCases, iso_week,Gemeinde_Name, iso_week_year, date_week) %>%
  rbind(data_s) %>%
  # filter(!is.na(GEM_ID)) %>%
  mutate(week=as.numeric(substr(iso_week, start = 6, stop = 7))) %>%
  # select(-Med_DIN_Week, -Gemeinde_Name) %>%
  arrange(Year, Gemeinde_Name, week) %>%
  # group_by(Year,District,BEZ_ID, Municipality, GEM_ID) %>%
  # complete(week, fill=list(n=0))
  # complete( week,  GEM_ID, nesting(Year)) %>%
  complete( week,  Gemeinde_Name, nesting(Year)) %>%
  filter(!is.na(week)) %>%
  filter(!is.na(Year)) %>%
  mutate(NumbCases=ifelse(is.na(NumbCases), 0, NumbCases),
         Year = as.numeric(Year)) %>%
  full_join(gemeinde) 


#   
save(data_bern ,file=paste0("data/data_bern.RData"))
write.xlsx(data_bern,file=paste0("data/data_bern.xlsx"),row.names=FALSE, overwrite = TRUE)
