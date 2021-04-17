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
gtm_adm2 <- bind_cols(gtm_adm2, lulc_ttls_adm2)

#Mask RasterStack
lulc_brick <- mask(lulc_stack, gtm_adm0)

#estimate linear model
linear_model <- lm(pop21 ~ water , dst011 + dst040 + dst130 + dst140 + dst150 + dst160 + dst190 + dst200 + topo + slope + night, data=gtm_adm2)
predicted_values <- raster::predict(lulc_brick, linear_model, progress="window")
base <- predicted_values - minValue(predicted_values)

#get base adm2
setwd("D:/programming/Data100/proj_dir/data/guatemala/")
#parallel
# ncores <- detectCores() - 1
# beginCluster(ncores)
# pred_vals_adm2 <- raster::extract(base, gtm_adm2, df=TRUE)
# endCluster()

#total and bind columns
# pred_ttls_adm2 <- aggregate(. ~ ID, pred_vals_adm2, sum)
# gtm_adm2 <- bind_cols(gtm_adm2, pred_ttls_adm2)
# save(gtm_adm2, file = "proj4_temp_adm2.RData")
load("proj4_temp_adm2.RData")

#predicted totals  and proportion of total
rasterized_pred_ttls_adm2 <- rasterize(gtm_adm2, predicted_values, field = "layer")
proportion_of_total_adm2 <- base / rasterized_pred_ttls_adm2

#predicted totals and population 2015
rasterized_pop15_adm2 <- rasterize(gtm_adm2, predicted_values, field = "pop21")
population_gtm_adm2 <- proportion_of_total_adm2 * rasterized_pop15_adm2

##########################################################################
#Investigate Margins of Error
pop_diff_resampled <- resample(population_gtm_adm2, gtm_pop15)
pop_diff_15 <- pop_diff_resampled - gtm_pop15
cellStats(abs(pop_diff_15), sum)
plot(pop_diff_15)

gtm_city_adm2 <- gtm_adm2 %>% 
  filter(substr(NAME_2, 1, 4) == "ZONA")
# plot(gtm_city_adm2)
urban_diff <- mask(pop_diff_15, gtm_city_adm2)
urban_pop <- mask(population_gtm_adm2, gtm_city_adm2)
b_box = c(-90.647711, -90.350940, 14.506167, 14.719152)
gtm_city_diff <- crop(urban_diff, b_box)
gtm_city_pop <- crop(urban_pop, b_box)
# plot(gtm_city_diff)
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
lulc_stack <- crop(lulc_stack, gtm_adm0)
writeRaster(lulc_stack, filename="proj4_lulc_stack.tif", overwrite = TRUE)
lulc_stack <- brick("proj4_lulc_stack.tif")
names(lulc_stack) <- c("water", "dst011" , "dst040", "dst130", "dst140", "dst150", 
                 "dst160", "dst190", "dst200", "topography", "slope", "night")

#Extract 2015 Population to ADM2 raster
gtm_adm2 <- sf::read_sf("gadm36_GTM_2.shp")
# ncores <- detectCores() - 1
# beginCluster(ncores)
# pop_vals_adm2 <- raster::extract(gtm_pop15, gtm_adm2, df = TRUE)
# endCluster()
# save(pop_vals_adm2, file = "proj4_gtm_pop_vals.RData")
load("proj4_gtm_pop_vals.RData")
# totals_adm2 <- pop_vals_adm2 %>%
#   group_by(ID) %>%
#   summarize(pop15 = sum(GTM_ppp_v2b_2015, na.rm = TRUE))
# gtm_adm2 <- gtm_adm2 %>%
#   add_column(pop15 = totals_adm2$pop15)
# gtm_adm2 <- gtm_adm2 %>%
#   mutate(area = st_area(gtm_adm2) %>%
#            units::set_units(km^2)) %>%
#   mutate(density = pop15 / area)
# save(gtm_adm2, file="proj4_gtm_adm2.RData")
load("proj4_gtm_adm2.RData")
#Get LulC
# ncores <- detectCores() - 1
# beginCluster(ncores)
# lulc_vals_adm2 <- raster::extract(lulc_stack, gtm_adm2, df = TRUE)
# endCluster()
# save(lulc_vals_adm2, file = "proj4_lulc_vals_adms.RData")
load("proj4_lulc_vals_adms.RData")

# lulc_ttls_adm2 <- lulc_vals_adm2 %>%
#   group_by(ID) %>%
#   summarize_all(sum, na.rm = TRUE)
# lulc_means_adm2 <- lulc_vals_adm2 %>%
#   group_by(ID) %>%
#   summarize_all(mean, na.rm = TRUE)
# gtm_adm2 <- bind_cols(gtm_adm2, lulc_ttls_adm2, lulc_means_adm2)
# save(gtm_adm2, file="proj4_adm2.RData")
load("proj4_adm2.RData")

model.sums <- lm(pop15 ~ water...19 + dst011...20 + dst040...21 + dst130...22 + dst140...23 + dst150...24 + dst160...25 + dst190...26 + dst200...27 + topography...28 + slope...29 + night...30, data=gtm_adm2)
model.means <- lm(pop15 ~ water...32 + dst011...33 + dst040...34 + dst130...35 + dst140...36 + dst150...37 + dst160...38 + dst190...39 +dst200...40 + topography...41 + slope...42 + night...43, data=gtm_adm2)
gtm_adm2$logpop15 <- log(gtm_adm2$pop15)
model.logpop15 <- lm(logpop15 ~ water...32 + dst011...33 + dst040...34 + dst130...35 + dst140...36 + dst150...37 + dst160...38 + dst190...39 +dst200...40 + topography...41 + slope...42 + night...43, data=gtm_adm2)

summary(model.sums)
summary(model.means)
summary(model.logpop15)

names(lulc_stack) <- c("water...19" , "dst011...20" , "dst040...21" , "dst130...22" , "dst140...23" , "dst150...24" , "dst160...25" , "dst190...26" , "dst200...27" , "topography...28" , "slope...29" , "night...30")
lulc_stack1 <- lulc_stack
names(lulc_stack1) <- c("water...32" , "dst011...33" , "dst040...34" , "dst130...35" , "dst140...36" , "dst150...37" , "dst160...38" , "dst190...39" , "dst200...40" , "topography...41" , "slope...42" , "night...43")

predicted_values_sums <- raster::predict(lulc_stack, model.sums)
predicted_values_means <- raster::predict(lulc_stack1, model.means)
predicted_values_logpop15 <- raster::predict(lulc_stack1, model.logpop15)
save(predicted_values_sums, predicted_values_means, predicted_values_logpop15, file = "proj4_predicted_values.RData")
load("proj4_predicted_values.RData")

# gtm_pred_val_sums <- exactextractr::exact_extract(predicted_values_sums, gtm_adm2, force_df=TRUE, fun=c('count'))
# gtm_pred_val_means <- exactextractr::exact_extract(predicted_values_means, gtm_adm2,  force_df=TRUE, fun=c('count'))
# gtm_pred_val_logpop15 <- exactextractr::exact_extract(predicted_values_logpop15, gtm_adm2,  force_df=TRUE, fun=c('count'))
# save(gtm_pred_val_sums, gtm_pred_val_means, gtm_pred_val_logpop15, file = "predicted_values_adm3s.RData")
load("predicted_values_adm3s.RData")
totals <- cbind.data.frame(preds_sums = gtm_pred_val_sums$count, 
                         preds_means = gtm_pred_val_means$count, 
                         resp_logpop = gtm_pred_val_logpop15$count)

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

plot(population_sums)
plot(diff_sums)
rasterVis::plot3D(diff_sums)
cellStats(abs(diff_sums), sum)

plot(population_means)
plot(diff_means)
rasterVis::plot3D(diff_means)
cellStats(abs(diff_means), sum)

plot(population_logpop)
plot(diff_logpop)
rasterVis::plot3D(diff_logpop)
cellStats(abs(diff_logpop), sum)

plot(gtm_pop15)

rgl.snapshot("diff", fmt = "png", top = TRUE )