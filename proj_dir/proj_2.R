# rm(list=ls(all=TRUE))
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)

library(tidyverse)
library(sf)
library(raster)
library(parallel)
library(evaluate)
library(rayshader)
setwd("D:/programming/Data100/proj_dir")

lbr_pop20 <- raster::raster("D:/programming/Data100/proj_dir/data/guatemala/gtm_pd_2020_1km.tif") 
lbr_adm2 <- sf::read_sf("D:/programming/Data100/proj_dir/data/guatemala/gadm36_GTM_2.shp") 
lbr_adm1 <- sf::read_sf("D:/programming/Data100/proj_dir/data/guatemala/gadm36_GTM_1.shp")

# ncores <- parallel::detectCores() - 1
# beginCluster(ncores)
# pop_values_adm2 <- raster::extract(lbr_pop20, lbr_adm2, df = TRUE)
# pop_values_adm1 <- raster::extract(lbr_pop20, lbr_adm1, df = TRUE)
# endCluster()
# save(pop_values_adm2, file = "pop_vals_adm2.RData")
# save(pop_values_adm1, file = "pop_vals_adm1.RData")


load("D:/programming/Data100/proj_dir/pop_vals_adm2.RData")
load("D:/programming/Data100/proj_dir/pop_vals_adm1.RData")

totals_adm2 <- pop_values_adm2 %>%
  group_by(ID) %>%
  summarize(pop21 = sum(gtm_pd_2020_1km, na.rm = TRUE))
lbr_adm2 <- lbr_adm2 %>%
  add_column(pop21 = totals_adm2$pop21)

totals_adm1 <- pop_values_adm1 %>%
  group_by(ID) %>%
  summarize(population = sum(gtm_pd_2020_1km, na.rm = TRUE))

lbr_adm1 <- lbr_adm1 %>%
  add_column(population = totals_adm1$population)

save(lbr_adm1, file="GTM_adm1_pop.RData")

# gg3d <- ggplot(lbr_adm2) + 
#   geom_sf(aes(fill = log(pop21)),
#           size = .25) +
#   scale_fill_gradient2(low = "white", mid = "yellow", high = "purple", midpoint = 10)

# plot_gg(gg3d, multicore = TRUE, width = 6, height = 2.7, fov = 69)

# render_movie(type = "orbit", 
#              frames = 720, 
#              fps = 30,
#              filename = "gtm_mov",
#              zoom = 0.5)
# ggplot(lbr_adm2) +
#   geom_sf(aes(fill = log(pop21)),
#           size = .25) +
#   geom_sf(data = lbr_adm1,
#           size = 1,
#           fill = NA) + 
#   scale_fill_gradient2(low = "white", mid = "yellow", high = "darkorange2", midpoint = 10) + 
#   geom_sf_text(aes(label = NAME_2),
#                color = "black",
#                size = .5) +
#   geom_sf_text(data = lbr_adm1,
#                aes(label = NAME_1),
#                color = "black",
#                size = 2,
#                alpha = .3) + 
#   ggtitle("Guatemala", subtitle = "Logarithmic Population") +
#   xlab("Longitude") + 
#   ylab("Latitude") + 
#   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
#         panel.background = element_rect(fill = "azure"),
#         panel.border = element_rect(fill = NA))
# ggsave("gtm_adm2_pop.png",
#        dpi = 1000,
#        width = 10,
#        height = 10)