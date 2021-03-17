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

setwd("D:/programming/Data100/proj_dir/")

#import adm boundaries
load("GTM_adm1_pop.RData")
load("GTM_adm2_pop.RData")

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

plot(lulc_stack[[10]])
plot(st_geometry(lbr_adm1), add = TRUE)

# ncores <- detectCores() - 1
# beginCluster(ncores)
# lulc_vals_adm2 <- raster::extract(lulc_stack, lbr_adm2, df = TRUE)
# endCluster()
# save(lulc_vals_adm2, file = "lulc_vals_adm2.RData")

setwd("D:/programming/Data100/proj_dir/")
load("lulc_vals_adm2.RData")
lulc_ttls_adm2 <- lulc_vals_adm2 %>%
  group_by(ID) %>%
  summarize_all(sum, na.rm = TRUE)

lbr_adm2 <- bind_cols(lbr_adm2, lulc_ttls_adm2)

ggplot(lbr_adm2, aes(log(night))) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#FF6666") + 
  theme_minimal()
