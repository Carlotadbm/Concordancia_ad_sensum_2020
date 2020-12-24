#Calcular números de datos totales cada vez
library(tidyverse)
library(fs)
setwd("~/switchdrive/Concordancia_la_gente/Twitter/")


## Tabla 1: ocurrencias durante el proceso

#busquedas totales
busquedas <- dir_ls(regexp = "_TW\\.csv$")
busquedas_full <- map_dfr(busquedas, read_delim, delim = "\t", .id = "source")
busquedas_full %>% 
  count(source)

#matches automáticos
analizadas <- dir_ls(regexp = "_analysed_manual\\.csv$")
analizadas_full <- map_dfr(analizadas, read_delim, delim = "\t", .id = "source")
analizadas_full %>% 
  filter(!str_detect(match, "3sg")) %>% 
  filter(match != "FALSE") %>% 
  count(source) %>% 
  mutate(total = sum(n))

#matches válidos

analizadas_full %>% 
  filter(!is.na(valid_match)) %>% 
  filter(valid_match != "no") %>% 
  count(source) %>% 
  mutate(total = sum(n))

## Tabla 2: casos por sujeto, persona y posición
analizadas_full %>% 
  filter(!is.na(valid_match)) %>% 
  filter(valid_match != "no") %>% 
  select(source, id, place, valid_match) %>% 
  mutate(valid_match = str_replace(valid_match, "_perif(_prep)?", "_ant")) %>% 
  separate(valid_match, into = c("subject", "person", "position"), sep = "_") %>% 
  count(subject, person, position) %>% 
  spread(subject, n, fill = 0) %>% 
  mutate(
    alguien_percent = round(alguien/sum(alguien)*100, 1),
    nadie_percent = round(nadie/sum(nadie)*100, 1),
    gente_percent = round(lagente/sum(lagente)*100, 1),
    lafamilia_percent = round(lafamilia/sum(lafamilia)*100, 1),
    elmundo_percent = round(elmundo/sum(elmundo)*100, 1)
  )
  
