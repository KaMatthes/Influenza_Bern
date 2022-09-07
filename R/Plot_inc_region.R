function_R__inc_plot<- function() {
  
  load("data/data_bern.RData")

data_region <- data_bern %>%
  filter(!Gemeinde_Name=="ganzer Amtsbezirk")%>%
  mutate(Region_Name = ifelse(Region_Name=="Mittelland Bern" & Gemeinde_Name =="Bern", "Bern", Region_Name),
         Region_Name = ifelse(Region_Name=="Voralpen" & Gemeinde_Name =="Thun", "Thun", Region_Name),
         Region_Name = ifelse(Region_Name=="Seeland" & Gemeinde_Name =="Biel", "Biel", Region_Name),
         Region_Name = ifelse(Region_Name=="Laufental", "Jura", Region_Name)) %>%
  filter(!Gemeinde_Name=="Gstaad") %>%
  filter(!iso_week=="1918_53") %>%
  dplyr::group_by(Region_Name,Year, date_week,iso_week ) %>%
  summarise(NumRegion = sum(NumbCases),
            PopRegion = sum(Population)) %>%
  ungroup() %>%
  mutate( NumRegion  = ifelse(iso_week=="1918_28" & Region_Name=="Bern", 60, NumRegion),
          NumRegion  = ifelse(iso_week=="1918_29" & Region_Name=="Bern", 240, NumRegion),
     NumInc = NumRegion/PopRegion*1000)%>%
  arrange(Region_Name, Year, date_week) %>%
  dplyr::group_by(Region_Name) %>%
  mutate(roll_inc =rollmean(NumInc,3, na.pad=TRUE, align="right"),
         roll_num =rollmean(NumRegion,3, na.pad=TRUE, align="right")) %>%
  ungroup() %>%
  mutate(roll_inc = ifelse(is.na(roll_inc),0, roll_inc),
         Region_Name = factor(Region_Name, levels = c("Biel","Bern","Thun","Jura","Seeland","Oberaargau","Mittelland Bern","Voralpen","Oberland")),
         Region_Name = recode(Region_Name,
                              # "Biel" ="City of Biel",
                              # "Bern" ="City of Bern",
                              # "Thun" = "City of Thun",
                              "Mittelland Bern" = "Mittelland")) %>%
  arrange(Region_Name, Year, date_week) %>%
  mutate( Region_Name = recode(Region_Name,
                               "Biel" ="City of Biel",
                               "Bern" ="City of Bern",
                               "Thun" = "City of Thun"))

plot_inc <- ggplot(data=data_region) +
  geom_bar(aes(x = as.POSIXct(date_week), y = NumInc),stat="identity", alpha=1,fill = "grey") +
  geom_line(aes(x = as.POSIXct(date_week), y = roll_inc), lwd = 0.8, col="grey17") +
  # geom_line(aes(x = date_week, y = R_eff), lwd = 1, col="green") +
  # geom_hline(yintercept=1)+
  facet_wrap(Region_Name~., ncol=1) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  # scale_y_continuous(
  #   name = "Influenza Cases in xx",
  #   sec.axis = sec_axis(~./coeff, name = "R effectiv")
  # ) +
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  theme_bw()

}