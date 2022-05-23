load("data/data_bern.RData")

data_region <- data_bern %>%
  mutate(Region_Name = ifelse(Region_Name=="Mittelland Bern" & Gemeinde_Name =="Bern", "Bern", Region_Name),
         Region_Name = ifelse(Region_Name=="Voralpen" & Gemeinde_Name =="Thun", "Thun", Region_Name),
         Region_Name = ifelse(Region_Name=="Seeland" & Gemeinde_Name =="Biel", "Biel", Region_Name),
         Region_Name = ifelse(Region_Name=="Laufental", "Jura", Region_Name)) %>%
  filter(!Gemeinde_Name=="Gstaad") %>%
  dplyr::group_by(Region_Name, Year, week) %>%
  summarise(NumRegion = sum(NumbCases),
         PopRegion = sum(Population)) %>%
  ungroup() %>%
  mutate(NumInc = NumRegion/PopRegion*1000,
         iso_week = paste0(Year,"_",week)) %>%
  arrange(Year, week) %>%
  mutate(Region_Name = factor(Region_Name, levels = c("Biel","Bern","Thun","Jura","Seeland","Oberaargau","Mittelland Bern","Voralpen","Oberland"))) %>%
  filter(Year >1919)
                                                      
                                                      # %>%
#   filter(Region_Name=="Voralpen")


ggplot(data=data_region) +
  geom_bar(data=data_region, aes(x=iso_week , y=NumInc,fill=Region_Name),stat="identity") +
  # geom_line(data=data_region, aes(x=as.numeric(iso_week), y=NumInc,col=factor(Region_Name))) +
  # geom_vline(xintercept="1918_30") +
  # geom_vline(xintercept="1920_50") +
  facet_wrap(~Region_Name, ncol=1) +
  theme_bw()
  theme(aspect.ratio=1,
        strip.background = element_rect("white"),
        strip.text= element_text(colour ="black",size=10,face="bold"),
        legend.position='hidden',
        legend.justification = c(2.0, -1),
        legend.text = element_text( size=size_axis),
        axis.text.y=element_text(size=size_axis_title),
        axis.title=element_text(size=size_axis_title),
        axis.text.x=element_text(size=size_axis_title))



data_w_ts <- data_region %>%
  filter(Region_Name=="Bern") %>%
  dplyr::select(NumInc)%>%
  ts(frequency = 53, start = 1920)

# bp_w_ts <- breakpoints(data_w_ts ~ 1)

 data_w_ts %>%
  decompose(type = "additive") %>%
  autoplot(range.bars = FALSE) 

  # geom_vline(xintercept = 1919.500, linetype="dashed")