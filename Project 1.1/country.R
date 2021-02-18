rm(list=ls(all=TRUE))

# install.packages("tidyverse", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)

library(tidyverse)
library(sf)

setwd("D:/programming/Data100/Project 1.1")


lbr_int <- sf::read_sf("D:/programming/Data100/Project 1.1/gadm36_GAB_0.shp")
lbr_adm1 <- sf::read_sf("D:/programming/Data100/Project 1.1/gadm36_GAB_1.shp")
lbr_adm2 <- sf::read_sf("D:/programming/Data100/Project 1.1/gadm36_GAB_2.shp")

filter_name <- 'Haut-Ogooué'
filtered_adm1 <- lbr_adm1 %>% filter(NAME_1 == filter_name)
filtered_adm2 <- lbr_adm2 %>% filter(NAME_1 == filter_name) 
label_B_location <- filtered_adm1 %>% 
  mutate(my_nudge_x=-0.6,
         my_nudge_y=-1.4,
  )
label_A_location <- filtered_adm1 %>% 
  mutate(my_nudge_x=-4.6,
         my_nudge_y=2.5,
  )
big_plot <- ggplot() +
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
  geom_rect(data = lbr_adm1, xmin = 8.75, xmax = 12.1, ymin = -4, ymax = 1.1,
            fill = NA, colour = "green", size = 2) +
  geom_rect(data = lbr_adm1, xmin = 12.75, xmax = 14.5, ymin = -2.5, ymax = 0.1,
            fill = NA, colour = "blue", size = 2) +
  geom_sf_text(data = label_A_location, aes(label = "Detail A"),
               nudge_x=label_A_location$my_nudge_x,
               nudge_y=label_A_location$my_nudge_y,
               size = 5,
               color = "green") +
  geom_sf_text(data = label_B_location, aes(label = "Detail B"),
               nudge_x = label_B_location$my_nudge_x,
               nudge_y = label_B_location$my_nudge_y,
               size = 5,
               color = "blue") +
  geom_sf_text(data = lbr_adm2,
               aes(label = NAME_2),
               size = 1.8,
               color = "#6a00ff",
               alpha = 0.75) +
  geom_sf_text(data = lbr_adm1,
               aes(label = NAME_1),
               size = 4,
               color = "#6a00ff",
               alpha = .75) +
  geom_sf_text(data = lbr_int,
               aes(label = NAME_0),
               size = 8,
               color = "6a00ff",
               alpha = 0.2)

plot2 <- ggplot() + 
  geom_sf(size = 2) +
  geom_sf(data = filtered_adm2,
          size = .15,
          alpha = .4) +
  geom_sf_text(data = filtered_adm2,
               aes(label = NAME_2),
               size = 1.75) +
  geom_sf_text(data = filtered_adm1,
               aes(label = NAME_1),
               size = 4,
               alpha = .4) +
  xlab("longitude") + ylab("latitude") +
  ggtitle("Detail A", subtitle = "An administrative subdivision in Gabon") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "azure"),
        panel.border = element_rect(fill = NA))

cities <- lbr_adm1 %>% filter(NAME_1 == "Estuaire" | NAME_1 == "Moyen-Ogooué" | NAME_1 == "Ogooué-Maritime")
filtered_adm2_2 <- lbr_adm2 %>% filter(NAME_1 == "Estuaire" | NAME_1 == "Moyen-Ogooué" | NAME_1 == "Ogooué-Maritime")
plot3 <- ggplot() +
  geom_sf(size = .15) +
  geom_sf(data = cities,
          size = .5,
          alpha = .5) +
  geom_sf_text(data = filtered_adm2_2,
               aes(label = NAME_2),
               size = 1.75) +
  geom_sf_text(data = cities,
               aes(label = NAME_1),
               size = 4,
               alpha = .5) +
  xlab("longitude") + ylab("latitude") +
  ggtitle("Detail B", subtitle = "The most populous region of Gabon") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "azure"),
        panel.border = element_rect(fill = NA))

ggplot() +
  coord_equal(xlim = c(0, 6.0), ylim = c(0, 4), expand = FALSE) +
  annotation_custom(ggplotGrob(big_plot), xmin = 0.0, xmax = 4.0, ymin = 0,
                    ymax = 4.0) +
  annotation_custom(ggplotGrob(plot3), xmin = 4.0, xmax = 6.0, ymin = 0, 
                    ymax = 2.0) +
  annotation_custom(ggplotGrob(plot2), xmin = 4.0, xmax = 6.0, ymin = 2.0, 
                    ymax = 4.0) +
  theme_void()

ggsave("gabon.png")

