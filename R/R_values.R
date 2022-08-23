load("data/data_bern.RData")



data_region <- data_bern %>%
  mutate(Region_Name = ifelse(Region_Name=="Mittelland Bern" & Gemeinde_Name =="Bern", "Bern", Region_Name),
         Region_Name = ifelse(Region_Name=="Voralpen" & Gemeinde_Name =="Thun", "Thun", Region_Name),
         Region_Name = ifelse(Region_Name=="Seeland" & Gemeinde_Name =="Biel", "Biel", Region_Name),
         Region_Name = ifelse(Region_Name=="Laufental", "Jura", Region_Name)) %>%
  filter(!Gemeinde_Name=="Gstaad") %>%
  mutate(
         iso_week = paste0(Year,"_",week),
         iso_week2 = sprintf("%02d",as.numeric(week)),
         iso_week3= paste0("W",iso_week2),
         iso_week_year = paste0(Year,"-", iso_week3,"-1"),
         date_week =  ISOweek2date(iso_week_year) +4) %>%
  dplyr::group_by(Region_Name,Year, date_week,iso_week ) %>%
  summarise(NumRegion = sum(NumbCases),
            PopRegion = sum(Population)) %>%
  ungroup() %>%
  mutate(NumInc = NumRegion/PopRegion*1000)%>%
  arrange(  Region_Name, Year, date_week) %>%
  mutate(Region_Name = factor(Region_Name, levels = c("Biel","Bern","Thun","Jura","Seeland","Oberaargau","Mittelland Bern","Voralpen","Oberland")),
         Region_Name = recode(Region_Name, 
                              "Biel" ="City of Biel",
                              "Bern" ="City of Bern",
                              "Thun" = "City of Thun",
                              "Mittelland Bern" = "Mittelland"))

data_region1 <- data_region %>%
  filter(Region_Name=="City of Thun") %>%
  slice(27:n())


t_start<- seq(2, nrow(data_region1)-1)   
t_end = t_start +1

res_parametric_si <- estimate_R(data_region1$NumRegion, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = mean_serial_interval, 
                                  std_si = std_serial_interval,
                                  t_start=t_start,
                                  t_end = t_end)))


write.xlsx(data_region,file=paste0("data/data_region_test2.xlsx"),row.names=FALSE, overwrite = TRUE)