rm(list=ls(all=TRUE))

# install.packages("tidyverse", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)

library(tidyverse)
library(sf)

setwd("D:/programming/Data100/Project 1.1")

lbr_int <- sf::read_sf("D:/programming/Data100/Project 1.1/gadm36_GAB_0.shp")
lbr_adm1 <- sf::read_sf("D:/programming/Data100/Project 1.1/gadm36_GAB_1.shp")
lbr_adm2 <- sf::read_sf("D:/programming/Data100/Project 1.1/gadm36_GAB_2.shp")
ggplot() +
  geom_sf(data = lbr_adm2,
          size = .5,
          color = "#6705f2",
          fill = "#ceabff",
          alpha = 0.5) +
  geom_sf(data = lbr_adm1,
          size = 1,
          color = "#220052",
          fill = "#ceabff",
          alpha = 0.5) +
  geom_sf(data = lbr_int, size = .75, color = "#000000", fill = "#ceabff", alpha = 0.5) +
  geom_sf_text(data = lbr_adm2,
               aes(label = NAME_2),
               size = 3,
               color = "#6a00ff") +
  geom_sf_text(data = lbr_adm1,
               aes(label = NAME_1),
               size = 5,
               color = "#6a00ff")


ggsave("gabon.png")