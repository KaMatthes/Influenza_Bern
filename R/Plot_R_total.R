function_R_eff_all_plot <- function() {
  
  load("../data/res_R_eff_all.RData")
  
  
  res_R_eff <- res_R_eff_all %>%
  mutate(R_eff=ifelse(is.na(R_eff), 0, R_eff),
         Cl=ifelse(is.na(Cl), 0, Cl),
         Cu=ifelse(is.na(Cu), 0, Cu),
         # R_eff=ifelse(R_eff>10, 0, R_eff),
         # Cl=ifelse(R_eff>10, 0, Cl),
         # Cu=ifelse(R_eff>10, 0, Cu),
         roll_num=ifelse(is.na(roll_num), 0, roll_num),
         R_eff=ifelse(roll_num==0, 0, R_eff),
         Cl=ifelse(roll_num==0, 0, Cl),
         Cu=ifelse(roll_num==0, 0, Cu)) 

plot_R_eff <- ggplot(data=res_R_eff) +
  geom_hline(yintercept=1, col="grey",lwd=lwd_date)+
  geom_vline(xintercept=datlim1, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim3, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim5, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim7, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim9, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim11, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_vline(xintercept=datlim13, col="grey", linetype = line_type, lwd=lwd_date)+
  geom_ribbon(aes(ymin =  Cl, ymax = Cu,x = as.POSIXct(date_week)), fill = "grey70")+
  geom_line(aes(x = as.POSIXct(date_week), y = R_eff),col="black", lwd = lwdline) +
  xlab("")+
  ylab("R effective")+
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  theme_bw()+
  theme(axis.text = element_text(size=text_size),
        axis.title = element_text(size=text_size),
        legend.position = "none")

return(plot_R_eff)

}