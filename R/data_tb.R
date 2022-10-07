
data_tb <- read.xlsx("data_raw/Tb_data.xlsx",detectDates = TRUE)  %>%
  rename(Gemeinde_Name = Gemeinde) %>%
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
