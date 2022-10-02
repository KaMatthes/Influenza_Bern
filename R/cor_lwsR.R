data_Gew <- read.csv("data_raw/Cofactors_1918.csv", sep=";")  %>%
  # rename(Gemeinde_Name = Gemeinde) %>%
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
  select(GEM_ID,Gemeinde_Name,LWS) 
# %>%
#   add_row(GEM_ID=243, Gemeinde_Name="Matten", HöheüM=575)

load("data/data_bern.RData")

data_inc <- data_bern %>%
  filter(Year==1918 | Year==1919) %>%
  filter(!Gemeinde_Name=="Gstaad") %>%
  filter(!Gemeinde_Name=="ganzer Amtsbezirk") %>%
  filter(!Gemeinde_Name=="Wengen") %>%
  mutate( NumbCases  = ifelse(iso_week=="1918_28" & Gemeinde_Name =="Bern", 60, NumbCases),
          NumbCases  = ifelse(iso_week=="1918_29" & Gemeinde_Name =="Bern", 240,NumbCases)) %>%
  group_by(GEM_ID) %>%
  mutate(Sum_date = sum(NumbCases)) %>%
  ungroup() %>%
  distinct(GEM_ID, .keep_all = TRUE) %>%
  left_join(data_Gew) %>%
  mutate(Sum_Inc =Sum_date/Population*1000,
         Lws_prop = LWS/Population*100) 



# poisson2 <- glm(Sum_date ~ TB_mor+offset(log(Population)),data = data_inc,  family=poisson)
glmNB <- glm.nb(Sum_date ~    Lws_prop + offset(log(Population)),data = data_inc, link = "log")
summary(glmNB)



plot_lws <- ggplot(data=data_inc) +
  geom_point(aes(x=   Lws_prop, y= Sum_Inc), lwd=lwd_size_points ) +
  geom_smooth(aes(x=  Lws_prop,y= Sum_Inc),  method='lm',se=TRUE,lwd=lwd_size, col=col_line) +
  ggtitle("Altitude vs Influenza")+
  ylab("Incidence Influenza per 1'000 inhabitants")+
  xlab("Altitude") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

# plot_household <- ggplot(data=data_inc) +
#   geom_point(aes(x= House_nr, y= Sum_Inc), lwd=lwd_size_points ) +
#   geom_smooth(aes(x=  House_nr,y= Sum_Inc),  method='lm',se=TRUE,lwd=lwd_size, col=col_line) +
#   ggtitle("Number of households vs Influenza")+
#   ylab("Incidence Influenza per 1'000 inhabitants")+
#   xlab("Households") +
#   # xlim(0,2200)+
#   theme_bw() +
#   theme(aspect.ratio = 1,
#         strip.text.x=element_text(size=15),
#         axis.text.x=element_text(color="black",size=10),
#         axis.title=element_text(size=15),
#         legend.text=element_text(size=15),
#         legend.title =element_text(size=15),
#         plot.title = element_text(size=15),
#         legend.position = "bottom")



# cowplot::save_plot("output/plot_SEP.pdf",plot_SEP,ba
