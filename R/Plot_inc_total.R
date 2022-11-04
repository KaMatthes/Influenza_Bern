function_inc_all_plot<- function() {
  
  load("data/data_bern.RData")

data_region <- data_bern %>%
  filter(!Gemeinde_Name=="ganzer Amtsbezirk")%>%
  filter(!Year==1925) %>%
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
          NumRegion  = ifelse(iso_week=="1918_29" & Region_Name=="Bern", 240, NumRegion)) %>%
  group_by(date_week) %>%
  mutate(Numall = sum(NumRegion),
         Popall = sum(PopRegion)) %>%
  ungroup() %>%
  distinct(date_week, .keep_all = TRUE) %>%
  mutate(NumInc = NumRegion/PopRegion*1000)%>%
  arrange(date_week) %>%
  select(-Region_Name) %>%
  mutate(roll_inc =rollmean(NumInc,3, na.pad=TRUE, align="right"),
         roll_inc = ifelse(is.na(roll_inc),0, roll_inc))


plot_inc_all <- ggplot(data=data_region) +
  # geom_vline(xintercept=datlim1, col="grey", linetype = line_type, lwd=lwd_date)+
  # geom_vline(xintercept=datlim3, col="grey", linetype = line_type, lwd=lwd_date)+
  # geom_vline(xintercept=datlim5, col="grey", linetype = line_type, lwd=lwd_date)+
  # geom_vline(xintercept=datlim7, col="grey", linetype = line_type, lwd=lwd_date)+
  # geom_vline(xintercept=datlim9, col="grey", linetype = line_type, lwd=lwd_date)+
  # geom_vline(xintercept=datlim11, col="grey", linetype = line_type, lwd=lwd_date)+
  # geom_vline(xintercept=datlim13, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_bar(aes(x = as.POSIXct(date_week), y = NumInc,fill="Incidence"),stat="identity") +
  geom_line(aes(x = as.POSIXct(date_week), y = roll_inc, col="Moving average"),lwd = lwdline) +
  xlab("")+
  ylab("Incidence per 1'000 inhabitants")+
  ggtitle("Canton of Bern") +
  scale_fill_manual("",values="grey70")+
  scale_colour_manual("", values="grey30")+
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  theme_bw()+
  theme(axis.text = element_text(size=text_size),
        axis.title = element_text(size=text_size),
        plot.title = element_text(size = plot_title),
        legend.text=element_text(size=legend_size),
        legend.title =element_text(size=legend_size),
        legend.position = c(.2, .8))

# cowplot::save_plot("output/Figure_Incidence.pdf", plot_inc_all ,base_height=10,base_width=25)


return(plot_inc_all)

}