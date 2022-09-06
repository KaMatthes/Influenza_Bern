function_R_eff<- function() {
  
  load("data/data_bern.RData")

data_region <- data_bern %>%
  filter(!Gemeinde_Name=="ganzer Amtsbezirk")%>%
  mutate(Region_Name = ifelse(Region_Name=="Mittelland Bern" & Gemeinde_Name =="Bern", "Bern", Region_Name),
         Region_Name = ifelse(Region_Name=="Voralpen" & Gemeinde_Name =="Thun", "Thun", Region_Name),
         Region_Name = ifelse(Region_Name=="Seeland" & Gemeinde_Name =="Biel", "Biel", Region_Name),
         Region_Name = ifelse(Region_Name=="Laufental", "Jura", Region_Name)) %>%
  filter(!Gemeinde_Name=="Gstaad") %>%
  # mutate(
  #        # iso_week = paste0(Year,"_",week),
  #        # iso_week2 = sprintf("%02d",as.numeric(week)),
  #        # iso_week3= paste0("W",iso_week2),
  #        # iso_week_year = paste0(Year,"-", iso_week3,"-1"),
  #        date_week =  ISOweek2date(iso_week_year) +4) %>%
  dplyr::group_by(Region_Name,Year, date_week,iso_week ) %>%
  filter(!iso_week=="1918_53") %>%
  summarise(NumRegion = sum(NumbCases),
            PopRegion = sum(Population)) %>%
  ungroup() %>%
  mutate(NumInc = NumRegion/PopRegion*1000)%>%
  arrange(  Region_Name, Year, date_week) %>%
  mutate(Region_Name = factor(Region_Name, levels = c("Biel","Bern","Thun","Jura","Seeland","Oberaargau","Mittelland Bern","Voralpen","Oberland")),
         Region_Name = recode(Region_Name,
                              # "Biel" ="City of Biel",
                              # "Bern" ="City of Bern",
                              # "Thun" = "City of Thun",
                              "Mittelland Bern" = "Mittelland"),
         NumRegion  = ifelse(iso_week=="1918_28" & Region_Name=="Bern", 60, NumRegion),
         NumRegion  = ifelse(iso_week=="1918_29" & Region_Name=="Bern", 240, NumRegion))%>%
  arrange(Region_Name, Year, date_week) 

  
data_region1 <- data_region %>%
  filter(Region_Name==Region) %>%
  group_by(date_week) %>%
  mutate(NumRegion = sum(NumRegion)) %>%
  ungroup() %>%
  distinct(date_week, .keep_all = TRUE) %>%
  mutate(roll_num =rollmean(NumRegion,2, na.pad=TRUE, align="right")) %>%
  filter(!is.na(roll_num)) %>%
  filter(date_week >=ymd(pandemic_start) & date_week <= ymd(pandemic_end) ) %>%
  mutate(start= 1:n())%>%
  arrange(Region_Name,date_week)

  
  t_start<- seq(2, nrow(data_region1)-1)   
  t_end = t_start +1
  
  res_R_eff <- estimate_R(data_region1$roll_num , 
                                  method="parametric_si",
                                  config = make_config(list(
                                    mean_si = mean_serial_interval, 
                                    std_si = std_serial_interval,
                                    t_start=t_start,
                                    t_end = t_end)))
  
  save(res_R_eff,file=paste0("data/R_eff_data/res_R_eff_",Region,"_",pandemic_start,".RData"))
  
}

function_R_effectiv(Region="Bern", pandemic_start="1918-07-05",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Biel", pandemic_start="1918-07-05",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Thun", pandemic_start="1918-07-05",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Jura", pandemic_start="1918-07-05",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Seeland", pandemic_start="1918-07-05",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Oberaargau", pandemic_start="1918-07-05",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Mittelland", pandemic_start="1918-07-05",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Voralpen", pandemic_start="1918-07-05",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Oberland", pandemic_start="1918-07-05",  pandemic_end="1919-06-20")


function_R_effectiv(Region="Bern", pandemic_start="1920-01-09",  pandemic_end="1920-06-18")
function_R_effectiv(Region="Biel", pandemic_start="1920-01-09",  pandemic_end="1920-06-18")
function_R_effectiv(Region="Thun", pandemic_start="1920-01-09",  pandemic_end="1920-06-18")
function_R_effectiv(Region="Jura", pandemic_start="1920-01-09",  pandemic_end="1920-06-18")
function_R_effectiv(Region="Seeland", pandemic_start="1920-01-09",  pandemic_end="1920-06-18")
function_R_effectiv(Region="Oberaargau", pandemic_start="1920-01-09",  pandemic_end="1920-06-18")
function_R_effectiv(Region="Mittelland", pandemic_start="1920-01-09",  pandemic_end="1920-06-18")
function_R_effectiv(Region="Voralpen", pandemic_start="1920-01-09",  pandemic_end="1920-06-18")
function_R_effectiv(Region="Oberland", pandemic_start="1920-01-09",  pandemic_end="1920-06-18")

function_R_effectiv(Region="Bern", pandemic_start="1922-01-06",  pandemic_end="1922-04-21")
function_R_effectiv(Region="Biel", pandemic_start="1922-01-06",  pandemic_end="1922-04-21")
function_R_effectiv(Region="Thun", pandemic_start="1922-01-06",  pandemic_end="1922-04-21")
function_R_effectiv(Region="Jura", pandemic_start="1922-01-06",  pandemic_end="1922-04-21")
function_R_effectiv(Region="Seeland", pandemic_start="1922-01-06",  pandemic_end="1922-04-21")
function_R_effectiv(Region="Oberaargau", pandemic_start="1922-01-06",  pandemic_end="1922-04-21")
function_R_effectiv(Region="Mittelland", pandemic_start="1922-01-06",  pandemic_end="1922-04-21")
function_R_effectiv(Region="Voralpen", pandemic_start="1922-01-06",  pandemic_end="1922-04-21")
function_R_effectiv(Region="Oberland", pandemic_start="1922-01-06",  pandemic_end="1922-04-21")


function_R_effectiv(Region="Bern", pandemic_start="1924-01-04",  pandemic_end="1924-06-06")
function_R_effectiv(Region="Biel", pandemic_start="1924-01-04",  pandemic_end="1924-06-06")
function_R_effectiv(Region="Thun", pandemic_start="1924-01-04",  pandemic_end="1924-06-06")
function_R_effectiv(Region="Jura", pandemic_start="1924-01-04",  pandemic_end="1924-06-06")
function_R_effectiv(Region="Seeland", pandemic_start="1924-01-04",  pandemic_end="1924-06-06")
function_R_effectiv(Region="Oberaargau", pandemic_start="1924-01-04",  pandemic_end="1924-06-06")
function_R_effectiv(Region="Mittelland", pandemic_start="1924-01-04",  pandemic_end="1924-06-06")
function_R_effectiv(Region="Voralpen", pandemic_start="1924-01-04",  pandemic_end="1924-06-06")
function_R_effectiv(Region="Oberland", pandemic_start="1924-01-04",  pandemic_end="1924-06-06")


function_R_effectiv(Region="Bern", pandemic_start="1924-11-21",  pandemic_end="1925-05-08")
function_R_effectiv(Region="Biel", pandemic_start="1924-11-21",  pandemic_end="1925-05-08")
function_R_effectiv(Region="Thun", pandemic_start="1924-11-21",  pandemic_end="1925-05-08")
function_R_effectiv(Region="Jura", pandemic_start="1924-11-21",  pandemic_end="1925-05-08")
function_R_effectiv(Region="Seeland", pandemic_start="1924-11-21",  pandemic_end="1925-05-08")
function_R_effectiv(Region="Oberaargau", pandemic_start="1924-11-21",  pandemic_end="1925-05-08")
function_R_effectiv(Region="Mittelland", pandemic_start="1924-11-21",  pandemic_end="1925-05-08")
function_R_effectiv(Region="Voralpen", pandemic_start="1924-11-21",  pandemic_end="1925-05-08")
function_R_effectiv(Region="Oberland", pandemic_start="1924-11-21",  pandemic_end="1925-05-08")