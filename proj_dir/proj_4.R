rm(list=ls(all=TRUE))

# install.packages("raster", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("doParallel", dependencies = TRUE)
# install.packages("snow", dependencies = TRUE)
# install.packages("rgl", dependencies = TRUE)
# install.packages("rasterVis", dependencies = TRUE)
# install.packages("tmap", dependencies = TRUE)

library(sf)
library(raster)
library(tidyverse)
library(doParallel)
library(snow)
library(rgl)
library(rasterVis)
library(exactextractr)

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
gtm_pd_pop15 <- raster("gtm_pd_2015_1km.tif")
gtm_adm0 <- sf::read_sf("gadm36_GTM_0.shp")
gtm_adm1 <- sf::read_sf("gadm36_GTM_1.shp")
gtm_adm2 <- sf::read_sf("gadm36_GTM_2.shp")
setwd("D:/programming/Data100/proj_dir/")
lulc_vals_adm2 <- exactextractr::exact_extract(lulc_stack, gtm_adm2, force_df=TRUE, fun=c('sum'), append_cols=FALSE)
names(lulc_vals_adm2) <- c('water','dst011', 'dst040', 'dst130', 'dst140', 'dst150', 'dst160', 'dst190', 'dst200', 'topo', 'slope', 'night')
pop_vals_adm2 <- exactextractr::exact_extract(gtm_pd_pop15, gtm_adm2, force_df=TRUE, fun=c('sum'))
names(pop_vals_adm2) <- c('pop15')
gtm_adm2 <- bind_cols(gtm_adm2, lulc_vals_adm2)
gtm_adm2 <- bind_cols(gtm_adm2, pop_vals_adm2)

#Mask RasterStack
lulc_brick <- mask(lulc_stack, gtm_adm0)

#estimate linear model
linear_model <- lm(pop15 ~ water + dst011 + dst040 + dst130 + dst140 + dst150 + dst160 + dst190 + dst200 + topo + slope + night, data=gtm_adm2)
predicted_values <- raster::predict(lulc_brick, linear_model, progress="window")
base <- predicted_values - minValue(predicted_values)

#get base adm2
setwd("D:/programming/Data100/proj_dir/data/guatemala/")
# pred_vals_adm2 <- exact_extract(base, gtm_adm2, force_df=TRUE, fun=c('sum'))
# names(pred_vals_adm2) <- c('layer')
# gtm_adm2 <- bind_cols(gtm_adm2, pred_vals_adm2)
#total and bind columns
# pred_ttls_adm2 <- aggregate(. ~ ID, pred_vals_adm2, sum)
# gtm_adm2 <- bind_cols(gtm_adm2, pred_ttls_adm2)
# save(gtm_adm2, file = "proj4_temp_adm2.RData")
load("proj4_temp_adm2.RData")

#predicted totals  and proportion of total
rasterized_pred_ttls_adm2 <- rasterize(gtm_adm2, predicted_values, field = "layer")
proportion_of_total_adm2 <- base / rasterized_pred_ttls_adm2

#predicted totals and population 2015
rasterized_pop15_adm2 <- rasterize(gtm_adm2, predicted_values, field = "pop15")
population_gtm_adm2 <- proportion_of_total_adm2 * rasterized_pop15_adm2

##########################################################################
#Investigate Margins of Error
pop_diff_resampled <- resample(population_gtm_adm2, gtm_pop15)
pop_diff_15 <- pop_diff_resampled - gtm_pop15
cellStats(pop_diff_15, sum)
original_gtm_adm2 <- sf::read_sf("gadm36_GTM_2.shp")

setwd("D:/programming/Data100/proj_dir/data/images/")
gtm_city_adm2 <- gtm_adm2 %>% 
  filter(substr(NAME_2, 1, 4) == "ZONA")
# plot(gtm_city_adm2)
urban_diff <- mask(pop_diff_15, gtm_city_adm2)
urban_pop <- mask(population_gtm_adm2, gtm_city_adm2)
b_box = c(-90.647711, -90.350940, 14.506167, 14.719152)
gtm_city_diff <- crop(urban_diff, b_box)
gtm_city_pop <- crop(urban_pop, b_box)
jpeg(file="proj4_gtmcity_adm2.jpeg")
plot(gtm_city_pop, main="Population in Guatemala City at the ADM2 Level")
dev.off()
# plot(gtm_city_pop)

#3D map of Guatemala City population
# rasterVis::plot3D(gtm_city_pop)

#Map view of Guatemala City differences in population estimates
# mapview::mapview(gtm_city_diff, alpha = .5)

mixco_adm2 <- gtm_adm2 %>% 
  filter(NAME_2 == "Mixco")
mixco_adm1 <- gtm_adm2 %>%
  filter(NAME_1 == "Guatemala")
# plot(gtm_city_adm2)
m_urban_diff <- mask(pop_diff_15, mixco_adm1)
m_urban_pop <- mask(population_gtm_adm2, mixco_adm1)
m_box = c(-90.8, -90.2, 14.2, 15)
mixco_diff <- crop(m_urban_diff, m_box)
mixco_pop <- crop(m_urban_pop, m_box)
jpeg(file="proj4_gtmcity_adm1.jpeg")
plot(mixco_pop, main="Population in Guatemala City at the ADM1 Level")
dev.off()
# plot(gtm_city_pop)

#3D map of Guatemala City population
# rasterVis::plot3D(gtm_city_pop)

#Map view of Guatemala City differences in population estimates
# mapview::mapview(gtm_city_diff, alpha = .5)

################################################################################
#Investigating and Comparing Results
rm(list=ls(all=TRUE))
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

#prepare RasterBrick
lulc_stack <- crop(lulc_stack, gtm_adm0)
lulc_stack <- mask(lulc_stack, gtm_adm0)
writeRaster(lulc_stack, filename="proj4_lulc_stack.tif", overwrite = TRUE)
lulc_stack <- brick("proj4_lulc_stack.tif")
names(lulc_stack) <- c("water", "dst011" , "dst040", "dst130", "dst140", "dst150", 
                 "dst160", "dst190", "dst200", "topography", "slope", "night")

#Extract 2015 Population to ADM2 raster
gtm_adm2 <- sf::read_sf("gadm36_GTM_2.shp")
pop_vals_adm2 <- exact_extract(gtm_pop15, gtm_adm2, force_df = TRUE, fun=c("sum"))
names(pop_vals_adm2) = c("pop15")
gtm_adm2 <- gtm_adm2 %>%
  add_column(pop15 = pop_vals_adm2$pop15)
gtm_adm2 <- gtm_adm2 %>%
  mutate(area = st_area(gtm_adm2) %>%
           units::set_units(km^2)) %>%
  mutate(density = pop15 / area)

lulc_vals_adm2 <- exact_extract(lulc_stack, gtm_adm2, force_df=TRUE, fun=c("mean", "sum"))
names(lulc_vals_adm2) <- c("water", "dst011" , "dst040", "dst130", "dst140", "dst150", 
                       "dst160", "dst190", "dst200", "topography", "slope", "night",
                       "water1", "dst0111" , "dst0401", "dst1301", "dst1401", "dst1501", 
                       "dst1601", "dst1901", "dst2001", "topography1", "slope1", "night1")
gtm_adm2 <- bind_cols(gtm_adm2, lulc_vals_adm2)

model.sums <- lm(pop15 ~ water + dst011 + dst040 + dst130 + dst140 + dst150 + dst160 + dst190 + dst200 + topography + slope + night, data=gtm_adm2)
model.means <- lm(pop15 ~ water1 + dst0111 + dst0401 + dst1301 + dst1401 + dst1501 + dst1601 + dst1901 + dst2001 + topography1 + slope1 + night1, data=gtm_adm2)
gtm_adm2$logpop15 <- log(gtm_adm2$pop15)
model.logpop15 <- lm(logpop15 ~ water1 + dst0111 + dst0401 + dst1301 + dst1401 + dst1501 + dst1601 + dst1901 + dst2001 + topography1 + slope1 + night1, data=gtm_adm2)

summary(model.sums)
summary(model.means)
summary(model.logpop15)

names(lulc_stack) <- c("water", "dst011" , "dst040", "dst130", "dst140", "dst150", 
                        "dst160", "dst190", "dst200", "topography", "slope", "night")
lulc_stack1 <- lulc_stack
names(lulc_stack1) <- c("water1", "dst0111" , "dst0401", "dst1301", "dst1401", "dst1501", 
                        "dst1601", "dst1901", "dst2001", "topography1", "slope1", "night1")

predicted_values_sums <- raster::predict(lulc_stack, model.sums)
predicted_values_means <- raster::predict(lulc_stack1, model.means)
predicted_values_logpop15 <- raster::predict(lulc_stack1, model.logpop15)
save(predicted_values_sums, predicted_values_means, predicted_values_logpop15, file = "proj4_predicted_values.RData")
load("proj4_predicted_values.RData")

gtm_pred_val_sums <- exactextractr::exact_extract(predicted_values_sums, gtm_adm2, force_df=TRUE, fun=c('sum'))
gtm_pred_val_means <- exactextractr::exact_extract(predicted_values_means, gtm_adm2,  force_df=TRUE, fun=c('sum'))
gtm_pred_val_logpop15 <- exactextractr::exact_extract(predicted_values_logpop15, gtm_adm2,  force_df=TRUE, fun=c('sum'))
load("predicted_values_adm3s.RData")
totals <- cbind.data.frame(preds_sums = gtm_pred_val_sums$sum, 
                         preds_means = gtm_pred_val_means$sum, 
                         resp_logpop = gtm_pred_val_logpop15$sum)

gtm_adm2 <- bind_cols(gtm_adm2, totals)

predicted_totals_sums <- rasterize(gtm_adm2, predicted_values_sums, field = "preds_sums")
predicted_totals_means <- rasterize(gtm_adm2, predicted_values_sums, field = "preds_means")
predicted_totals_logpop <- rasterize(gtm_adm2, predicted_values_sums, field = "resp_logpop")

gridcell_proportions_sums  <- predicted_values_sums / predicted_totals_sums
gridcell_proportions_means  <- predicted_values_means / predicted_totals_means
gridcell_proportions_logpop  <- predicted_values_logpop15 / predicted_totals_logpop

# cellStats(gridcell_proportions_sums, sum)
# cellStats(gridcell_proportions_means, sum)
# cellStats(gridcell_proportions_logpop, sum)

population_adm2 <- rasterize(gtm_adm2, predicted_values_sums, field = "pop15")

population_sums <- gridcell_proportions_sums * population_adm2
population_means <- gridcell_proportions_means * population_adm2
population_logpop <- gridcell_proportions_logpop * population_adm2

# cellStats(population_sums, sum)
# cellStats(population_means, sum)
# cellStats(population_logpop, sum)

# sum(gtm_adm2$pop15)

sum_diff_resampled <- resample(gtm_pop15, population_sums)
diff_sums <- population_sums - sum_diff_resampled
mean_diff_resampled <- resample(gtm_pop15, population_means)
diff_means <- population_sums - mean_diff_resampled
log_diff_resampled <- resample(gtm_pop15, population_logpop)
diff_logpop <- population_logpop - mean_diff_resampled

setwd("D:/programming/Data100/proj_dir/images/")
png(file="proj4_predicted_gtm_sums.png")
plot(population_sums, main="Plot of Predicted Population in Guatemala")
dev.off()
png(file="proj4_diff_gtm_sums.png")
plot(diff_sums, main="Plot of Difference between Predicted \non and Actual Population in Guatemala")
dev.off()
rasterVis::plot3D(diff_sums) #Plot of Difference between Predicted and Actual Population in Guatemala
cellStats(abs(diff_sums), sum)

png(file="proj4_predicted_gtm_means.png")
plot(population_means,  main="Plot of Predicted Population in Guatemala")
dev.off()
png(file="proj4_diff_gtm_means.png")
plot(diff_means, main="Plot of Difference between Predicted \non and Actual Population in Guatemala")
dev.off()
rasterVis::plot3D(diff_means)
cellStats(abs(diff_means), sum)

png(file="proj4_predicted_gtm_logpop.png")
plot(population_logpop,  main="Plot of Predicted Population in Guatemala")
dev.off()
png(file="proj4_diff_gtm_logpop.png")
plot(diff_logpop, main="Plot of Difference between Predicted and Actual Population in Guatemala")
dev.off()
rasterVis::plot3D(diff_logpop)
cellStats(abs(diff_logpop), sum)

plot(gtm_pop15)

rgl.snapshot("diff", fmt = "png", top = TRUE )