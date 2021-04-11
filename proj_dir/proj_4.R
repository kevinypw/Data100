rm(list=ls(all=TRUE))

# install.packages("raster", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("doParallel", dependencies = TRUE)
# install.packages("snow", dependencies = TRUE)

library(sf)
library(raster)
library(tidyverse)
library(doParallel)
library(snow)

# setwd("D:/programming/Data100/proj_dir/")

# LOAD STACK
setwd("D:/programming/Data100/proj_dir/lulc/")
f_names <- list.files(pattern=regex("esaccilc_dst.*\\.tif$"), recursive = TRUE)
lulc_stack <- stack(lapply(f_names, function(i) raster(i, band=1)))
trunc_names <- sub("_100m_2015.tif", "", sub("gtm_esaccilc_", "", f_names))
names(lulc_stack) <- trunc_names

topography <- raster("gtm_srtm_topo_100m.tif")
slope <- raster("gtm_srtm_slope_100m.tif")
night <- raster("gtm_viirs_100m_2015.tif")
lulc_stack <- addLayer(lulc_stack, topography, slope, night)
names(lulc_stack)[c(1,10:12)] <- c("water","topo","slope", "night")

#import administrative sublevels
setwd("D:/programming/Data100/proj_dir/data/guatemala/")
gtm_pop15 <- raster("GTM_ppp_v2b_2015.tif")
gtm_adm0 <- sf::read_sf("gadm36_GTM_0.shp")
gtm_adm1 <- sf::read_sf("gadm36_GTM_1.shp")
gtm_adm2 <- sf::read_sf("gadm36_GTM_2.shp")
setwd("D:/programming/Data100/proj_dir/")
load("GTM_adm1_pop.RData")
load("GTM_adm2_pop.RData")
load("lulc_vals_adm2.RData")
lulc_ttls_adm2 <- lulc_vals_adm2 %>%
  group_by(ID) %>%
  summarize_all(sum, na.rm = TRUE)
gtm_adm2 <- bind_cols(lbr_adm2, lulc_ttls_adm2)

#Mask RasterStack
lulc_brick <- mask(lulc_stack, gtm_adm0)

#estimate linear model
linear_model <- lm(pop21 ~ water + dst011 + dst040 + dst130 + dst140 + dst150 + dst160 + dst190 + dst200 + topo + slope + night, data=gtm_adm2)
predicted_values <- raster::predict(lulc_brick, linear_model, progress="window")
base = predicted_values - minValue(predicted_values)
cellStats(base, sum)