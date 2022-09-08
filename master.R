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
library(EpiEstim)
library(scales)
# library(estimateR)

no_classes_map <-5
col5viridis <- viridis(5, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
col8viridis <- viridis(8, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
colAnnals<- c("#477DB5","black","#BAD2BA","#C46263","#FDF8D7","black","#7AABA4","black","#84243D")


text_size <-80
strip_text <- 20
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


lims1 <- as.POSIXct(ymd("1918-01-04"))    
lims2 <- as.POSIXct(ymd("1925-12-25"))   



datlim1 <- as.POSIXct(ymd("1918-07-06"))
datlim2 <- as.POSIXct(ymd("1918-08-17"))

datlim3 <- as.POSIXct(ymd("1918-09-14"))
datlim4 <- as.POSIXct(ymd("1918-12-21"))

datlim5 <- as.POSIXct(ymd("1919-02-08"))
datlim6 <- as.POSIXct(ymd("1919-05-10"))

datlim7 <- as.POSIXct(ymd("1920-01-10"))
datlim8 <- as.POSIXct(ymd("1920-05-22"))

datlim9 <- as.POSIXct(ymd("1922-01-06"))
datlim10 <- as.POSIXct(ymd("1922-04-08"))

datlim11 <- as.POSIXct(ymd("1924-01-19"))
datlim12 <- as.POSIXct(ymd("1924-05-10"))

datlim13 <- as.POSIXct(ymd("1925-01-31"))
datlim14 <- as.POSIXct(ymd("1925-05-16"))



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
# 
# render(paste0("R/Influenza_Bern.Rmd"), output_file = paste0("../output/",today(),"_Influenza_Bern.html"))
render(paste0("R/Influenza_Bern_Region.Rmd"), output_file = paste0("../output/",today(),"_Influenza_Bern_Region.html"))