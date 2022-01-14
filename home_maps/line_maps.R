library(osmdata)
library(ggplot2)
library(ggmap)
library(cowplot)

available_tags('highway')

roads_noho <- getbb('Northampton Massachusetts') %>% 
  opq() %>% 
  add_osm_feature(key = 'highway', 
                  value = c('motorway','primary','motorway_link', 'primary_link')) %>% 
  osmdata_sf()

roads_dc <- getbb('Washington DC') %>% 
  opq() %>% 
  add_osm_feature(key = 'highway', 
                  value = c('motorway', 'primary')) %>% 
  osmdata_sf()

plot_noho <- ggplot() + 
  geom_sf(data = roads_noho$osm_lines, 
          inherit.aes = F, 
          color = 'black') +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid = element_blank()) 

plot_dc <- ggplot() + 
  geom_sf(data = roads_dc$osm_lines, 
          inherit.aes = F, 
          color = 'black') +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid = element_blank()) 

plot_dc

ggsave(
  plot = plot,
  filename = 'map_test.png',
  path = '~/Desktop/',
  dpi = 750
)
