rm(list = ls(all=TRUE))

library(tidyverse)
library(sf)
library(units)
library(scales)
library(ggpubr)

setwd("D:/programming/Data100/proj_dir")

load("D:/programming/Data100/proj_dir/GTM_adm1_pop.RData")

lbr_adm1 <- lbr_adm1 %>%
  mutate(area = sf::st_area(lbr_adm1) %>%
           units::set_units("km^2")) %>%
  mutate(density = population / area)

bar <- lbr_adm1 %>%
  mutate(NAME_1 = fct_reorder(NAME_1, population)) %>%
  ggplot(aes(x=NAME_1, y=population, fill = population)) +
  geom_bar(stat="identity", width = .7) +
  coord_flip() +
  geom_text(aes(label=scales::percent(population/sum(population))),
            position = position_stack(vjust = 0.5),
            size=2.5, color = "black") +
  xlab("county") +
  ylab("population") +
  ggtitle("Population and share of Population (in %)") +
  scale_fill_gradient(low = "yellow", high = "red")
  
country <- ggplot(lbr_adm1) +
  geom_sf(aes(fill = population),
          size = .25) +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = 10) +
  geom_sf_text(aes(label = NAME_1),
               color = "black",
               size = 2) +
  geom_sf_text(aes(label=round(100*population/sum(population), 2)),
               color="black", size=2, nudge_y = -0.07) +
  ggtitle("Population & Density (in persons/km^2)") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "azure"),
        panel.border = element_rect(fill = NA))

gtm <- ggarrange(country, bar, nrow = 1, widths = c(2.25, 2))
gtm <- annotate_figure(gtm, top = text_grob("Guatemala Population Density", color="black", face = "bold", siz = 26))
gtm
ggsave("gtm.png",
  dpi = 200,
  width = 10,
  height = 10)