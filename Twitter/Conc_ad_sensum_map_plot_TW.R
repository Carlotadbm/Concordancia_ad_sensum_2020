#Crear un mapa concordancia ad sensum

library(tidyverse)
library(fs)
library(extrafont)
library(grDevices)
library(ggtext)
library(ggExtra)
library(wesanderson)
library(mapproj) 


##Mapa 1
#obtener las coordenadas
places <- read_delim("places_geo_TW.csv", delim = "\t") 

#obtener archivos
csv_files <- dir_ls(regexp = "_analysed_manual\\.csv$")
full_table <- map_dfr(csv_files, read_delim, delim = "\t", .id = "subject")

#preparar la tabla
#valid_match
full_table_map <- full_table %>% 
  rename(idplaces = place) %>% 
  filter(results != 0) %>%
  filter(!is.na(valid_match)) %>%
  filter(valid_match != "no") %>% 
  select(idplaces, id, valid_match) %>% 
  mutate(valid_match = str_replace(valid_match, "_perif(_prep)?", "_ant")) %>% 
  separate(valid_match, into = c("subject", "person", "position"), sep = "_") %>% 
  left_join(places, by = "idplaces") %>% 
  filter(country_code != "US") 


world <- map_data('world', c("Argentina", "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", 
                             "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela", "Spain", "Canary"))

order_person <- c("3pl", "2pl", "1pl")
full_table_map$person <- factor(full_table_map$person, levels = order_person)

ggplot(world, aes(x = long, y = lat)) + #como tabla va nuestra tabla del mapa y como coordenadas x e y, la longitud y la latitud
  geom_map(map=world, aes(map_id=region), fill="white", color="black") + #y luego usamos la función geom_map para crear el perfil del mapa
  geom_point(data = full_table_map, aes(x = longitude, y = latitude, colour = person), #y geom_point para crear los puntos
             alpha = 0.5, position=position_jitter(h=0.1, w=0.1)) + #alpha da la transparencua de los puntos y la posición permite que se muevan un poco para no solaparse  
  labs(title = "Concordancia plural en el verbo en Twitter") +
  coord_map() +
  theme_bw(base_family = "Times New Roman", base_size = 14) +
  theme(legend.position = c(0.96, 0.02),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_manual(values=wes_palette(n=3, name="Moonrise3")) + 
  theme(plot.title = element_markdown())

ggsave("Conc_ad_sensum_TW_map.png",  scale = 1.5) #saves the last plot

###Figura 1
##Comparación frecuencias tuits vs concordancia
#frecuencias por número de TW
frec_corpus <- read_delim("TW_COUNTporpais.csv", delim = "\t")
frec_corpus <- frec_corpus %>% 
  select(country_code, Total) %>% 
  filter(country_code %in% full_table_map$country_code) %>% 
  rename(tuits = Total)


#frecuencia por país
full_table_map %>% 
  count(country_code) %>% 
  mutate(concordancia = round(n/sum(n)*100)) %>% 
  rename(conc_ad_sens = n) %>% 
  full_join(frec_corpus) %>% 
  mutate(tuits = round(tuits/sum(tuits)*100)) %>% 
  gather(concordancia, tuits, key = tipo_porcentaje, value = porcentaje) %>% 
  ggplot(aes(x = reorder(country_code, porcentaje), y = porcentaje, fill = tipo_porcentaje)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9) +
  coord_flip() +
  labs(title="Proporciones por países", subtitle = "Número de tuits y casos de concordancia *ad sensum*", x="País", y="%") +
  theme_bw(base_family = "Times New Roman", base_size = 12) +
  theme(legend.position = c(0.96, 0.02),
        legend.justification = c(1, 0),
        legend.title = element_blank()) +
  scale_fill_manual(values=wes_palette(n=2, name="Royal1")) + 
  theme(plot.subtitle = element_markdown())

ggsave("Proporciones_TW.png") #saves the last plot
