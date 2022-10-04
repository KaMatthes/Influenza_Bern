
load("data/data_bern.RData")

data_region <- data_bern %>%
  filter(!Gemeinde_Name=="ganzer Amtsbezirk")%>%
  mutate(Region_Name = ifelse(Region_Name=="Mittelland Bern" & Gemeinde_Name =="Bern", "Bern", Region_Name),
         Region_Name = ifelse(Region_Name=="Voralpen" & Gemeinde_Name =="Thun", "Thun", Region_Name),
         Region_Name = ifelse(Region_Name=="Seeland" & Gemeinde_Name =="Biel", "Biel", Region_Name),
         Region_Name = ifelse(Region_Name=="Laufental", "Jura", Region_Name)) %>%
  filter(!Gemeinde_Name=="Gstaad") %>%
  filter(!iso_week=="1918_53") %>%
  dplyr::group_by(Gemeinde_Name,iso_week) %>%
  dplyr::mutate(NumbCases= sum(NumbCases)) %>%
  ungroup() %>%
  distinct(Gemeinde_Name, iso_week,.keep_all = TRUE) %>%
  dplyr::group_by(Region_Name,iso_week) %>%
  dplyr::mutate(NumRegion = sum(NumbCases),
                PopRegion = sum(Population)) %>%
  ungroup() %>%
  distinct(Region_Name, iso_week,.keep_all = TRUE) %>%
  select(Year,iso_week, date_week, Region_Name, Population, NumRegion, PopRegion) %>%
  mutate( NumRegion  = ifelse(iso_week=="1918_28" & Region_Name=="Bern", 60, NumRegion),
          NumRegion  = ifelse(iso_week=="1918_29" & Region_Name=="Bern", 240, NumRegion))


data_pandemic1 <-data_region %>%
  filter(date_week >= as.Date(datlim1) & date_week <= as.Date(datlim2)) %>%
  mutate(pandemic  = "1918") %>%
  group_by(Region_Name) %>%
  mutate(cases_total = sum(NumRegion)) %>%
  ungroup() %>%
  mutate(inc_total = cases_total/PopRegion*1000)

data_pandemic2 <- data_region %>%
  filter(date_week >= as.Date(datlim3) & date_week <= as.Date(datlim6)) %>%
  mutate(pandemic  = "1918_1919")%>%
  group_by(Region_Name) %>%
  mutate(cases_total = sum(NumRegion)) %>%
  ungroup() %>%
  mutate(inc_total = cases_total/PopRegion*1000)


data_pandemic3 <- data_region %>%
  filter(date_week >= as.Date(datlim7) & date_week <= as.Date(datlim8)) %>%
  mutate(pandemic  = "1920") %>%
  group_by(Region_Name) %>%
  mutate(cases_total = sum(NumRegion)) %>%
  ungroup() %>%
  mutate(inc_total = cases_total/PopRegion*1000)


data_pandemic4 <- data_region %>%
  filter(date_week >= as.Date(datlim9) & date_week <= as.Date(datlim10)) %>%
  mutate(pandemic  = "1922") %>%
  group_by(Region_Name) %>%
  mutate(cases_total = sum(NumRegion)) %>%
  ungroup() %>%
  mutate(inc_total = cases_total/PopRegion*1000) %>%
  arrange(date_week)


data_pandemic5 <- data_region %>%
  filter(date_week >= as.Date(datlim11) & date_week <= as.Date(datlim12)) %>%
  mutate(pandemic  = "1924")%>%
  group_by(Region_Name) %>%
  mutate(cases_total = sum(NumRegion)) %>%
  ungroup() %>%
  mutate(inc_total = cases_total/PopRegion*1000)


data_pandemic6 <- data_region %>%
  filter(date_week >= as.Date(datlim13) & date_week <= as.Date(datlim14)) %>%
  mutate(pandemic  = "1925") %>%
  group_by(Region_Name) %>%
  mutate(cases_total = sum(NumRegion)) %>%
  ungroup() %>%
  mutate(inc_total = cases_total/PopRegion*1000)



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
        strip.text= element_text(colour ="black",size=strip_text,face="bold"),
        legend.position = "none")

return(plot_inc)

}