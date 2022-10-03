function_R_effectiv <- function(Region,pandemic_start,  pandemic_end ) {
  
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
  summarise(NumRegion = sum(NumbCases, na.rm=TRUE),
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
         NumRegion  = ifelse(iso_week=="1918_29" & Region_Name=="Bern", 240, NumRegion)) %>%
  arrange(Region_Name, Year, date_week) %>%
  group_by(Region_Name,date_week) %>%
  mutate(NumRegion = sum(NumRegion)) %>%
  ungroup() %>%
  distinct(date_week,Region_Name, .keep_all = TRUE) %>%
  group_by(Region_Name) %>%
  mutate(roll_num =rollmean(NumRegion,3, na.pad=TRUE, align="right")) %>%
  ungroup() %>%
  mutate(roll_num  = ifelse(roll_num<6,0, roll_num))

data_region1 <- data_region %>%
  filter(Region_Name==Region) %>%
  filter(!is.na(roll_num)) %>%
  filter(date_week >=ymd(pandemic_start) & date_week <= ymd(pandemic_end) ) %>%
  mutate(start= 1:n())%>%
  arrange(Region_Name,date_week) 


# Incubation period - gamma distribution parameters

shape_incubation = 1
scale_incubation = 1
incubation <- list(name="gamma", shape = shape_incubation, scale = scale_incubation)

# Delay from onset of symptoms to case observation - gamma distribution parameters
shape_onset_to_report = 1
scale_onset_to_report = 1
onset_to_report <- list(name="gamma", shape = shape_onset_to_report, scale = scale_onset_to_report)

# We specify these parameters in the same unit as the time steps in the original observation data.
# For instance, if the original data represents daily reports,
# the parameters below must be specified in days (this is the case in this toy example).
mean_serial_interval = 1.000001
std_serial_interval = 1



  t_start<- seq(2, nrow(data_region1)-3)   
  t_end = t_start +3
  
  res_R_eff <- estimate_R(data_region1$roll_num , 
                                  method="parametric_si",
                                  config = make_config(list(
                                    mean_si = mean_serial_interval,
                                    # mean_prior  = 1,
                                    std_si = std_serial_interval,
                                    t_start=t_start,
                                    t_end = t_end)))$R
  
  res_R_eff <-   res_R_eff %>%
    mutate(R_eff = `Mean(R)`,
           Std = `Std(R)`,
           Cl = `Quantile.0.025(R)`,
           Cu =`Quantile.0.975(R)`) %>%
    select(start=t_start, R_eff,Std,Cl,Cu) %>%
    # mutate(start = start +1) %>%
    full_join(data_region1) %>%
    arrange(date_week)
  # 
# return(  res_R_eff)
save(res_R_eff,file=paste0("data/R_eff_data/res_R_eff_",Region,"_",pandemic_start,".RData"))
  
}

function_R_effectiv(Region="Bern", pandemic_start="1918-06-28",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Biel", pandemic_start="1918-06-28",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Thun", pandemic_start="1918-06-28",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Jura", pandemic_start="1918-06-28",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Seeland", pandemic_start="1918-06-28",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Oberaargau", pandemic_start="1918-06-28",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Mittelland", pandemic_start="1918-06-28",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Voralpen", pandemic_start="1918-06-28",  pandemic_end="1919-06-20")
function_R_effectiv(Region="Oberland", pandemic_start="1918-06-28",  pandemic_end="1919-06-20")

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



path <- "data/R_eff_data"
setwd(path)
files <- list.files("C:/Users/kmatth/Dropbox/Influenza_Bern/data/R_eff_data")
results <- sapply(files, function(x) mget(load(x)), simplify = TRUE) 
res_R_eff <-   as.data.frame(do.call(rbind,   results)) %>%
  select(-start)

save(res_R_eff,file=paste0("res_R_eff.RData"))
