function_R_inc_plot<- function() {
  
  load("../data/data_bern.RData")

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
         Region_Name = recode(Region_Name,
                              # "Biel" ="City of Biel",
                              # "Bern" ="City of Bern",
                              # "Thun" = "City of Thun",
                              "Mittelland Bern" = "Mittelland")) %>%
  arrange(Region_Name, Year, date_week) %>%
  mutate( Region_Name = recode(Region_Name,
                               "Biel" ="City of Biel",
                               "Bern" ="City of Bern",
                               "Thun" = "City of Thun"),
          Region_Name = factor(Region_Name, 
                               levels = c("Jura","City of Biel","Seeland","Oberaargau","Mittelland","City of Bern","Voralpen","City of Thun","Oberland")))

plot_inc <- ggplot(data=data_region) +
  geom_vline(xintercept=datlim1, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim3, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim5, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim7, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim9, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim11, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim13, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_bar(aes(x = as.POSIXct(date_week), y = NumInc, fill=Region_Name),stat="identity") +
  geom_line(aes(x = as.POSIXct(date_week), y = roll_inc, col=Region_Name), lwd = lwdline) +
  xlab("")+
  ylab("per 1,000 inhab.")+
  facet_wrap(Region_Name~., ncol=1) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(values=colAnnals)+
  scale_fill_manual(values=colAnnals)+
  theme_bw()+
  theme_bw()+
  theme(axis.text = element_text(size=text_size),
        axis.title = element_text(size=text_size),
        legend.position = "none")

return(plot_inc)

}