rm(list=ls(all=TRUE))
#install.packages("rayshader", dependencies = TRUE)
library(raster)
library(sf)
library(tidyverse)
library(rayshader)
library(rayrender)

setwd("D:/programming/Data100/proj_dir/data/guatemala")
gtm_adm2 <- read_sf("gadm36_GTM_2.shp")
setwd("D:/programming/Data100/proj_dir/lulc/")
topography <- raster("gtm_srtm_topo_100m.tif")
cc <- gtm_adm2 %>% 
  filter(NAME_2 == "Chichicastenango")

combined_topo <- crop(topography, cc)
combined_matrix <- raster_to_matrix(combined_topo)

# combined_matrix %>%
#   sphere_shade() %>%
#   add_water(detect_water(combined_matrix)) %>%
#   plot_map()

ambientshadows <- ambient_shade(combined_matrix)
# combined_matrix %>%
#   sphere_shade(texture = "unicorn") %>%
#   add_water(detect_water(combined_matrix), color = "lightblue") %>%
#   add_shadow(ray_shade(combined_matrix, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
#   add_shadow(lamb_shade(combined_matrix, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
#   add_shadow(ambientshadows, max_darken = 0.1) %>%
#   plot_3d(combined_matrix, zscale = 20,windowsize = c(1000,1000), 
#           phi = 40, theta = 135, zoom = 0.9, 
#           background = "grey30", shadowcolor = "grey5", 
#           soliddepth = -50, shadowdepth = -100)
# render_snapshot(title_text = "Chichicastenango, Guatemala", 
#                 title_size = 50,
#                 title_color = "grey90")

setwd("D:/programming/Data100/proj_dir/")
load("cc_subpolys.RData")
cc_lineplot <- ggplot() +
  geom_sf(data = cc,
          size = 5.0,
          linetype = "11",
          color = "gold",
          alpha = 0) +
  geom_sf(data = sub_polygons,
          size = 0.75,
          color = "gray50",
          fill = "gold3",
          alpha = 0.5) +
  geom_sf(data = primary,
          size = 2.5,
          color = "red2") +
  geom_sf(data = secondary,
          size = 1.5,
          color = "red3") +
  geom_sf(data = tertiary,
          size = 0.8,
          color = "red4") +
  geom_sf(data = hospital,
          size = 4,
          color = "blue2",
          pch=3) +
  geom_sf(data = clinic,
          size = 4,
          color = "blue3",
          pch=3) +
  geom_sf(data = other,
          size = 4,
          color = "blue4",
          pch=3) +
  theme_void() + theme(legend.position="none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y=NULL, title=NULL)

setwd("D:/programming/Data100/proj_dir/images/")
png("combined.png", width = 920, height = 1136, units = "px", bg = "transparent")
cc_lineplot
dev.off()

# setwd("D:/programming/Data100/proj_dir/images/")
# png("proj_6_cc.png", width = 920, height = 1136, units = "px", bg = "transparent")
# cc_lineplot
# dev.off()

overlay_img <- png::readPNG("combined.png")
# combined_matrix %>%
#   sphere_shade() %>%
#   add_water(detect_water(combined_matrix)) %>%
#   add_overlay(overlay_img, alphalayer = 0.95) %>%
#   plot_map()
combined_matrix %>%
  sphere_shade() %>%
  add_water(detect_water(combined_matrix), color = "lightblue") %>%
  add_shadow(ray_shade(combined_matrix, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.6) %>%
  add_shadow(lamb_shade(combined_matrix, sunaltitude = 3, zscale = 33), max_darken = 0.4) %>%
  add_shadow(ambientshadows, max_darken = 0.1) %>%
  add_overlay(overlay_img, alphalayer = 0.85) %>%
  plot_3d(combined_matrix, zscale = 20,windowsize = c(1000,1000), 
          phi = 60, theta = 135, zoom = 1, 
          background = "grey30", shadowcolor = "grey5", 
          soliddepth = -10, shadowdepth = -100) 

render_snapshot(title_text = "Chichicastenango, Guatemala",
                title_size = 50,
                title_color = "grey90")

render_label(combined_matrix, "Chichicastenango", textcolor ="white", linecolor = "white", 
             x = 159, y = 45, z = 1839, textsize = 24, linewidth = 4, zscale = 10)


