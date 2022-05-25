.libPaths(c("H:/Documents/R/win-library/4.1", "C:/Program Files/R/R-4.1.2/library"))

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

no_classes_map <-5
col5viridis <- viridis(5, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
col8viridis <- viridis(8, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")


size_axis <-20
strip_text <- 20
size_axis_title <- 20
lwd_size_stillbirth <- 5
lwd_size <- 0.8
pch_type <- 19
lwdline <- 1
size_legend <- 15
size_legend_title<- 15
pd <-position_dodge(width=0.8)
plot_title <- 25

# load R sources

source("R/maps.R")
source("R/maps_all.R")
source("R/Hotspots_wave.R")
source("R/Hotspots_year.R")


render(paste0("R/Influenza_Bern.Rmd"), output_file = paste0("../output/",today(),"_Influenza_Bern.html"))