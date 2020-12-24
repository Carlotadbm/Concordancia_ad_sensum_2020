#Mapa datos COSER
#Concordancia ad sensum
# 17 de noviembre de 2020

library(tidyverse)
'%ni%' <- Negate('%in%')
library(sf)
library(ggvoronoi)
library(extrafont)
library(grDevices)
library(ggtext)
library(ggExtra)
library(mapproj) 


coser <- read_delim("conc_ad_sensum_clean.csv", delim = ";")
coords <- read_delim("COSER_coordenadas_con_ad_sensum.csv", delim = "\t")

#solo la_gente
coser_ady <- coser %>% 
  mutate(busqueda = ifelse(busqueda %in% c("la_gente_1", "a_gente", "gente", "la_gente_2"), "la gente", busqueda)) %>% 
  filter(sujeto == "si") %>% 
  filter(V1_adyacencia == "si") %>% 
  filter(busqueda == "la gente") %>% 
  filter(V1_persona %in% c("3sg", "3pl")) %>% 
  mutate(V1_persona = str_replace(V1_persona, "3", "")) %>% 
  select(id_busqueda, id_result, COSERID, V1_persona) %>% 
  rename(persona = V1_persona)

coser_vs <- coser %>% 
  mutate(busqueda = ifelse(busqueda %in% c("la_gente_1", "a_gente", "gente", "la_gente_2"), "la gente", busqueda)) %>% 
  filter(sujeto == "si") %>% 
  filter(Vs_persona %in% c("3sg", "3pl")) %>% 
  filter(busqueda == "la gente") %>% 
  mutate(Vs_persona = str_replace(Vs_persona, "3", "")) %>% 
  select(id_busqueda, id_result, COSERID, Vs_persona) %>% 
  rename(persona = Vs_persona)



coser_map <- coser_ady %>%
  add_row(coser_vs) %>% 
  count(persona, COSERID) %>% 
  spread(persona, n, fill = 0) %>% 
  mutate(total = pl + sg, 
         frequency_pl = pl/total*100) %>% 
  left_join(coords)

coser_map %>% 
  arrange(desc(frequency_pl))


#mapa poligonos (Mapa 2)

mapa <- st_read("ESP_adm1.shp") #cargar un shapefile, obtenido de https://www.diva-gis.org/gdata
mapa <- as_Spatial(mapa$geometry)


ggplot() +
  geom_voronoi(data = coser_map, #polígonos sin cortar
               aes(x = longitude,
                   y = latitude,
                   fill=frequency_pl),
               outline = mapa) + 
  geom_text(data = coser_map, #plotear los valores
            aes(x=longitude,
                y=latitude,
                label = total), #cambiando colores
            size = 2) +
  labs(title = "Frecuencia de la concordancia plural en el verbo en el COSER",
       fill = "% de 3.ª pl") +
  coord_map() +
  theme_bw(base_family = "Times New Roman", base_size = 12) +
  theme(legend.position = c(0.96, 0.02),
        legend.justification = c(1, 0),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_markdown()) +
  scale_fill_gradient(low = "lightgrey", high = "tomato")

ggsave("Conc_ad_sensum_COSER_poligonos_map.png",  scale = 1.5) #saves the last plot

