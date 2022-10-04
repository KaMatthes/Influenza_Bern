.libPaths(c("H:/Documents/R/win-library/4.1", "C:/Program Files/R/R-4.2.1/library"))

library(plyr)
library(readxl)
library(openxlsx)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(zoo)
library(foreach)
library(arsenal)
library(rmarkdown)
library(kableExtra)
library(sf)
library(sp)
library(spdep)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
library(ggpattern)
library(INLA)
library(maptools)
library(colorspace)
library(viridis)
library(RColorBrewer)
library(tsoutliers)
library(gganimate)
library(ISOweek)
library(cowplot)
library(av)
library(magick)
# library(EpiEstim)
library(scales)
library(MASS)
library(conflicted)
# library(estimateR)
library(introdataviz)


conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("arrange", "dplyr")

no_classes_map <-5
col5viridis <- viridis(5, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
col8viridis <- viridis(8, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
colAnnals<- c("#477DB5","black","#BAD2BA","#C46263","#FDF8D7","black","#7AABA4","black","#84243D")


text_size <-80
strip_text <- 80
size_axis_title <- 20
lwd_size_stillbirth <- 5
lwd_size <- 0.8
pch_type <- 19
lwdline <- 6
lwd_date <- 4
size_legend <- 15
size_legend_title<- 15
pd <-position_dodge(width=0.8)
plot_title <- 25
line_type <-  "solid"
legend_size_map <- 3
legend_title_map <- 4
main_size_map <- 5
lwd_size_points  <- 3
lwd_size <- 1.5

col_line <- "grey40"
lwd_size <- 1.5
axix_size_legend <- 6
axis_size  <- 15
axis_size_title  <- 15
legend_size <- 15
legend_size_title <- 15
size_title <- 15
# legend_size_map <-1
panel_size_map <- 1.5
legend_size_title_map <- 1.5
plot_title_size <- 15



lims1 <- as.POSIXct(ymd("1918-01-04"))    
lims2 <- as.POSIXct(ymd("1925-12-25"))   


datlim1 <- as.POSIXct(ymd("1918-06-21"))
datlim2 <- as.POSIXct(ymd("1918-09-20"))

datlim3 <- as.POSIXct(ymd("1918-09-27"))
datlim4 <- as.POSIXct(ymd("1918-12-21"))

datlim5 <- as.POSIXct(ymd("1919-02-08"))
datlim6 <- as.POSIXct(ymd("1919-06-20"))

datlim7 <- as.POSIXct(ymd("1920-01-02"))
datlim8 <- as.POSIXct(ymd("1920-06-18"))

datlim9 <- as.POSIXct(ymd("1921-12-23"))
datlim10 <- as.POSIXct(ymd("1922-05-05"))

datlim11 <- as.POSIXct(ymd("1924-01-11"))
datlim12 <- as.POSIXct(ymd("1924-05-30"))

datlim13 <- as.POSIXct(ymd("1924-12-12"))
datlim14 <- as.POSIXct(ymd("1925-05-08"))



# Parameter R effective

mean_serial_interval <- 1.00001
std_serial_interval <- 1
# load R sources
# 
# source("R/maps.R")
# source("R/maps_all.R")
# source("R/maps_wave.R")
# source("R/Hotspots_wave.R")
# source("R/Hotspots_year.R")

source("R/Plot_inc_total.R")
source("R/Plot_R_total.R")
source("R/Plot_inc_regions.R")
source("R/Plot_R_regions.R")
source("R/Hotspots_wave_regions.R")
source("R/Maps_Jenks.R")
source("R/function_popdensity.R")
source("R/function_altitude.R")
source("R/function_rain.R")
source("R/function_lws.R")
source("R/function_gew.R")
source("R/function_tb.R")
source("R/function_betriebe.R")
source("R/function_arbeiter.R")
source("R/function_ps.R")

# render(paste0("R/Influenza_Bern.Rmd"), output_file = paste0("../output/",today(),"_Influenza_Bern.html"))
# render(paste0("R/Influenza_Bern_Region.Rmd"), output_file = paste0("../output/",today(),"_Influenza_Bern_Region.html"))

render(paste0("R/Influenza_Bern_Marco.Rmd"), output_file = paste0("../output/",today(),"_Influenza_Bern_Marco.html"))