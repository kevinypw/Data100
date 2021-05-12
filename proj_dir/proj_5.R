rm(list=ls(all=TRUE))

# install.packages("raster", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("maptools", dependencies = TRUE)
# install.packages("spatstat", dependencies = TRUE)

library(raster)
library(sf)
library(tidyverse)
library(maptools)
library(spatstat)
library(exactextractr)
library(units)

setwd("D:/programming/Data100/proj_dir/data/guatemala")
gtm_adm2  <- read_sf("gadm36_GTM_2.shp")
gtm_ppp_pop15 <- raster("GTM_ppp_v2b_2015.tif")

# chose Chichicastenango
cc <- gtm_adm2 %>% 
  filter(NAME_2 == "Chichicastenango")
cc_pop15 <- crop(gtm_ppp_pop15, cc)
cc_pop15 <- mask(cc_pop15, cc)
pop <- floor(cellStats(cc_pop15, 'sum'))

#generate initial plot
setwd("D:/programming/Data100/proj_dir/images")
# png("cc_pop15.png", width = 800, height = 800)
# plot(cc_pop15, main = NULL)
# plot(st_geometry(cc), add = TRUE)
# dev.off()

setwd("D:/programming/Data100/proj_dir/")
# st_write(cc, "cc_st_2015.shp", delete_dsn = TRUE)
cc_adm2_mtools <- readShapeSpatial("cc_st_2015.shp")

win <- as(cc_adm2_mtools, "owin")
cc_adm2_ppp <- rpoint(pop, f = as.im(cc_pop15), win = win)

# png("cc_points_2015.png", width = 2000,  height = 2000)
# plot(win, main = NULL)
# plot(cc_adm2_ppp, cex = 0.15, add = TRUE)
# dev.off()

# bw <- bw.ppl(cc_adm2_ppp)
# save(bw, file="bw.RData")
load("bw.RData")

cc_density_image <- density.ppp(cc_adm2_ppp, sigma = bw)
Dsg <- as(cc_density_image, "SpatialGridDataFrame")  # convert to spatial grid class
Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image
Dcl <- contourLines(Dim, levels = 8000000)  # create contour object
SLDF <- ContourLines2SLDF(Dcl, CRS("+proj=longlat +datum=WGS84 +no_defs"))
multiline_cc_sf <- st_as_sf(SLDF, sf)

# plot(cc_density_image, main="Population Density")
# plot(multiline_cc_sf, add=TRUE)

inside_polygons <- st_polygonize(multiline_cc_sf)
outside_lines <- st_difference(multiline_cc_sf, inside_polygons)

buffer_offset1 <- st_buffer(outside_lines, 0.001)
buffer_offset2 <- st_difference(cc, buffer_offset1)
buffer_offset3 <- st_cast(buffer_offset2, "POLYGON")
buffer_offset3$area <- as.numeric(st_area(buffer_offset3))
sub_polygons <- buffer_offset3 %>%
  filter(area < 60000000)
sub_polygons <- sub_polygons[ -c(5), ]

setwd("D:/programming/Data100/proj_dir/")
# save(sub_polygons, file="cc_subpolys.RData")
load("cc_subpolys.RData")

subpolys_extract <- raster::extract(cc_pop15, sub_polygons, df = TRUE)
subpolys_totals <- subpolys_extract %>%
  group_by(ID) %>%
  summarize(pop15 = sum(GTM_ppp_v2b_2015, na.rm = TRUE))
sub_polygons <- sub_polygons %>%
  add_column(pop15 = subpolys_totals$pop15)

# png("subpolys.png", width = 1200, height = 1200)
# plot(cc_density_image, main = NULL)
# plot(st_geometry(sub_polygons), add = TRUE)
# dev.off()

subpolys_filtered <- sub_polygons %>%
  filter(pop15 > 750)
# png("subpolys_filtered.png", width = 1200, height = 1200)
# plot(cc_density_image, main = NULL)
# plot(st_geometry(subpolys_filtered), add = TRUE)
# dev.off()

inside_polygons <- st_collection_extract(inside_polygons, "POLYGON")
ips_extract <- raster::extract(cc_pop15, inside_polygons, df = TRUE)
ips_totals <- ips_extract %>%
  group_by(ID) %>%
  summarize(pop15 = sum(GTM_ppp_v2b_2015, na.rm = TRUE))
inside_polygons <- inside_polygons %>%
  add_column(pop15 = ips_totals$pop15)

inside_polys_filtered <- inside_polygons %>%
  filter(pop15 > 150)
uas <- st_union(inside_polys_filtered, subpolys_filtered)
urban_areas <- st_cast(uas, "POLYGON")
urban_areas[ ,1:19] <- NULL

# plot(cc_pop15, main = NULL)
# plot(st_geometry(urban_areas), add = TRUE)

uas_extract <- raster::extract(cc_pop15, urban_areas, df = TRUE)
uas_totals <- uas_extract %>%
  group_by(ID) %>%
  summarize(pop15 = sum(GTM_ppp_v2b_2015, na.rm = TRUE))
urban_areas <- urban_areas %>%
  add_column(pop15 = uas_totals$pop15)
urban_areas <- urban_areas %>%
  unique()

# ggplot() +
#   geom_sf(data = cc,
#           size = 0.75,
#           color = "gray50",
#           fill = "gold3",
#           alpha = 0.15) +
#   geom_sf(data = urban_areas,
#           fill = "lightblue",
#           size = 0.45,
#           alpha = 0.5)

urban_areas <- urban_areas %>%
  mutate(area = st_area(urban_areas) %>%
           set_units(km^2)) %>%
  mutate(density = as.numeric(pop15 / area))

ua_cntr_pts <-  urban_areas %>% 
  st_centroid() %>% 
  st_cast("MULTIPOINT")

setwd("D:/programming/Data100/proj_dir/images")
# ggplot() +
#   ggtitle("Urbanized Population in Chichicastenango, Guatemala") +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   geom_sf(data = cc,
#           size = 0.75,
#           color = "gray50",
#           fill = "gold3",
#           alpha = 0.15) +
#   geom_sf(data = urban_areas,
#           fill = "lightblue",
#           size = 0.25,
#           alpha = 0.5) +
#   geom_sf(data = ua_cntr_pts,
#           aes(size = pop15,
#               color = density),
#           show.legend = 'point') +
#   scale_color_gradient2(low = "yellow", high = "red")
# ggsave("proj5_final_plot.png",
#           dpi = 800,
#           width = 5,
#           height = 5,
#           scale = 1.75,
#           units="in")

#accessiblity 2
setwd("D:/programming/Data100/proj_dir/data/guatemala")
LMIC_roads <- read_sf("hotosm_gtm_roads_lines.shp")
uas_areas <- st_cast(uas, "POLYGON")
uas_areas[ ,1:19] <- NULL
adm2_roads <- st_crop(LMIC_roads, cc)
primary <- adm2_roads %>%
  filter(highway == "primary")
secondary <- adm2_roads %>%
  filter(highway == "secondary" | highway == "secondary_link")
tertiary <- adm2_roads %>%
  filter(highway == "tertiary" | highway == "footway" | highway == "pedestrian" | highway == "steps" | highway == "track" | highway == "residential" | 
           highway == "trunk" | highway == "trunk_link" | highway == "unclassified" | highway == "service")

setwd("D:/programming/Data100/proj_dir/data/guatemala/shapefiles")
LMIC_hospitals <- read_sf("healthsites.shp")
adm2_hospitals <- st_crop(LMIC_hospitals, cc)
clinic <- adm2_hospitals %>%
  filter(amenity == "clinic")
other <- adm2_hospitals %>%
  filter(amenity == "dentist" | amenity =="pharmacy")
hospital <- adm2_hospitals %>%
  filter(amenity == "hospital")
# ggplot() +
#   geom_sf(data = cc,
#           size = 0.75,
#           color = "gray50",
#           fill = "gold3",
#           alpha = 0.15) +
#   geom_sf(data = urban_areas,
#           size = 0.75,
#           color = "gray50",
#           fill = "gold3",
#           alpha = 0.15) +
#   geom_sf(data = primary,
#           size = 0.75,
#           color = "steelblue4",
#           alpha = 0.75) +
#   geom_sf(data = secondary,
#           size = 0.75,
#           color = "steelblue4",
#           alpha = 0.5) +
#   geom_sf(data = tertiary,
#           size = 0.75,
#           color = "steelblue4",
#           alpha = 0.25) +
#   geom_sf(data = hospital,
#           size = 4,
#           color = "slateblue2",
#           pch=3) +
#   geom_sf(data = clinic,
#           size = 4,
#           color = "slateblue2",
#           pch=3) +
#   geom_sf(data = other,
#           size = 4,
#           color = "chartreuse2",
#           pch=3) +
#   geom_sf(data = cc,
#           size = 0.75,
#           color = "gray50",
#           fill = "gold3",
#           alpha = 0.15) +
#   geom_sf(data = ua_cntr_pts,
#           aes(size = pop15,
#               color = density),
#           show.legend = 'point') +
#   scale_color_gradient2(low = "yellow", high = "red") + 
#   xlab("longitude") + ylab("latitude") +
#   ggtitle("Healthcare Facilities and Roadways throughout Chichicastenango, Guatemala")
# setwd("D:/programming/Data100/proj_dir/images")
# ggsave("proj5_final_plot_a2.png",
#        dpi = 800,
#        width = 5,
#        height = 5,
#        scale = 1.75,
#        units="in")
