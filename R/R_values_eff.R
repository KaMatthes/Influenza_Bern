function_R_eff_plot<- function() {
  
  load("data/res_R_eff.RData")
  
  
  res_R_eff <- res_R_eff %>%
  mutate(R_eff=ifelse(is.na(R_eff), 0, R_eff),
         Cl=ifelse(is.na(Cl), 0, Cl),
         Cu=ifelse(is.na(Cu), 0, Cu),
         # R_eff=ifelse(R_eff>10, 0, R_eff),
         # Cl=ifelse(R_eff>10, 0, Cl),
         # Cu=ifelse(R_eff>10, 0, Cu),
         roll_num=ifelse(is.na(roll_num), 0, roll_num),
         R_eff=ifelse(roll_num==0, 0, R_eff),
         Cl=ifelse(roll_num==0, 0, Cl),
         Cu=ifelse(roll_num==0, 0, Cu)) %>%
  distinct(Region_Name,date_week, .keep_all = TRUE) %>%
  mutate( Region_Name = recode(Region_Name,
                               "Biel" ="City of Biel",
                               "Bern" ="City of Bern",
                               "Thun" = "City of Thun"),
          Cl2 = R_eff -1.96*Std,
          Cu2 = R_eff +1.96*Std )

plot_R_eff <- ggplot(data=res_R_eff) +
  geom_ribbon(aes(ymin =  Cl, ymax = Cu,x = as.POSIXct(date_week)), fill = "grey70")+
  geom_line(aes(x = as.POSIXct(date_week), y = R_eff), lwd = 1, col="green") +
  geom_hline(yintercept=1)+
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