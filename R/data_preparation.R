gemeinde <- read.xlsx("data_raw/Daten_SpanischeGrippe.xlsx",detectDates = TRUE, sheet="Gemeinden") %>%
  select(GEM_ID, Gemeinde_Name, Bez_Name, Bez_ID, Region_ID, Region_Name, E, N, Population =Wohnb)
  


data_s <-read.csv("data_raw/Data_Spanische_Grippe.csv", sep=";") %>%
  mutate(Disease = ifelse(Disease=="influenza", "Influenza", Disease)) %>%
  filter(Disease=="Influenza") %>%
  select(Year,Med_DIN_Week,NumbCases=NumbCasesAdjust2,GEM_ID,Gemeinde_Name="Place_korr", Wave) %>%
  group_by(Year,Med_DIN_Week,GEM_ID,Gemeinde_Name) %>%
  mutate(NumbCases = sum(NumbCases)) %>%
  distinct(Year, Med_DIN_Week, GEM_ID, .keep_all = TRUE) %>%
  ungroup() 

data_bern <-read.xlsx("data_raw/Influenza_Bern.xlsx",detectDates = TRUE) %>%
  mutate(Krankheit2 = ifelse(Krankheit2=="influenza", "Influenza", Krankheit2),
         Wave = NA) %>%
  filter(Krankheit2=="Influenza") %>%
  select(Jahr, Monat, Day_early, Anzahl3, Gemeinde3, Wave) %>%
  mutate(Med_DIN_Week = paste0(Jahr,"_",isoweek(Day_early ))) %>%
  rename(Year=Jahr,
         NumbCases=Anzahl3,
         Gemeinde_Name =  Gemeinde3) %>%
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
                               "Walliswil" = "Walliswil b. Wangen")) %>%
  select(-Monat, -Day_early) %>%
  full_join(gemeinde[,c("Gemeinde_Name", "GEM_ID")] )  %>%
  group_by(Year,Med_DIN_Week,GEM_ID,Gemeinde_Name) %>%
  mutate(NumbCases = sum(as.numeric(NumbCases))) %>%
  distinct(Year, Med_DIN_Week, GEM_ID, .keep_all = TRUE) %>%
  ungroup() %>%
  rbind(data_s) %>%
  filter(!is.na(GEM_ID)) %>%
  mutate(week=as.numeric(substr(Med_DIN_Week, start = 6, stop = 7))) %>%
  select(-Med_DIN_Week, -Gemeinde_Name) %>%
  arrange(Year, GEM_ID, week) %>%
  # group_by(Year,District,BEZ_ID, Municipality, GEM_ID) %>%
  # complete(week, fill=list(n=0))
  complete( week,  GEM_ID, nesting(Year, Wave)) %>%
  filter(!is.na(week)) %>%
  filter(!is.na(Year)) %>%
  mutate(NumbCases=ifelse(is.na(NumbCases), 0, NumbCases)) %>%
  full_join(gemeinde)
#   
save(data_bern ,file=paste0("data/data_bern.RData"))
write.xlsx(data_bern,file=paste0("data/data_bern.xlsx"),row.names=FALSE, overwrite = TRUE)
