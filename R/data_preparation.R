data_s <-read.xlsx("data_raw/Daten_SpanischeGrippe.xlsx",detectDates = TRUE) %>%
  select(Year,Med_DIN_Week,NumbCases=NumbCasesAdjust2,GEM_ID, Municipality=PlaceOrig,GEM_ID,District, BEZ_ID)

data_mun_id <- data_s %>%
  select(GEM_ID, Municipality)

data_b <-read.xlsx("data_raw/Influenza_Bern.xlsx",detectDates = TRUE) %>%
  mutate(Krankheit2 = ifelse(Krankheit2=="influenza", "Influenza", Krankheit2)) %>%
  filter(Krankheit2=="Influenza") %>%
  select(Jahr, Monat, Day_early, Anzahl3,Amtsbezirk, Bezirksnummer, Gemeinde3) %>%
  mutate(Med_DIN_Week = paste0(Jahr,"_",isoweek(Day_early ))) %>%
  rename(Year=Jahr,
         NumbCases=Anzahl3,
         District=Amtsbezirk,
         BEZ_ID = Bezirksnummer,
         Municipality =  Gemeinde3) %>%
  mutate(Municipality = recode("St. Beatenberg" ="Beatenberg",
                               "St. Imier" ="St-Imier",
                               "Gündlischwand"="Gründlischwand",
                               "Tramelan-dessus" = "Tramelan"))
  select(-Monat, -Day_early) %>%
  left_join(data_mun_id) %>%
  distinct(Municipality, .keep_all = TRUE)
  
write.table(data_b,file=paste0("data/data_test.csv"),row.names=FALSE, sep=";")