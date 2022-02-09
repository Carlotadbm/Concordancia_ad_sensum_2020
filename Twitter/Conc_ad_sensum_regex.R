#Extracción de regex para el artículo sobre la concordancia ad sensum del sujeto 
#Noviembre 2020

#El corpus empleado no es público. 
#Estas son las regex empleadas.

lagente <- tweets %>% 
  select(idtweets, id, text, fecha, place_name, place) %>% 
  mutate(text = str_to_lower(text, locale = "es")) %>% 
  filter(str_detect(text, "\\bla gente\\b")) %>%
  filter(!str_detect(text, "\\b(a|ante|bajo|con|contra|de|desde|en|entre|hacia|hasta|mediante|para|por|seg[úu]n|sin|sobre|tras|versus|vía) (?:toda )?la gente\\b")) 
#(?:) creates a non-capturing group. It groups things together without creating a backreference.

lafamilia <- tweets %>% 
  select(idtweets, id, text, fecha, place_name, place) %>% 
  mutate(text = str_to_lower(text, locale = "es")) %>% 
  filter(str_detect(text, "\\bla familia\\b")) %>%
  filter(!str_detect(text, "\\b(a|ante|bajo|con|contra|de|desde|en|entre|hacia|hasta|mediante|para|por|seg[úu]n|sin|sobre|tras|versus|vía) (?:toda )?la familia\\b")) 

elmundo <- tweets %>% 
  select(idtweets, id, text, fecha, place_name, place) %>% 
  mutate(text = str_to_lower(text, locale = "es")) %>% 
  filter(str_detect(text, "\\btodo el mundo\\b")) %>%
  filter(!str_detect(text, "\\b(a|ante|bajo|con|contra|de|desde|en|entre|hacia|hasta|mediante|para|por|seg[úu]n|sin|sobre|tras|versus|vía) todo el mundo\\b")) 

alguien <- tweets %>% 
  select(idtweets, id, text, fecha, place_name, place) %>% 
  mutate(text = str_to_lower(text, locale = "es")) %>% 
  filter(str_detect(text, "\\balguien\\b")) %>%
  filter(!str_detect(text, "\\b(a|ante|bajo|con|contra|de|desde|en|entre|hacia|hasta|mediante|para|por|seg[úu]n|sin|sobre|tras|versus|vía) alguien\\b")) 
  
nadie <- tweets %>% 
  select(idtweets, id, text, fecha, place_name, place) %>% 
  mutate(text = str_to_lower(text, locale = "es")) %>% 
  filter(str_detect(text, "\\bnadie\\b")) %>%
  filter(!str_detect(text, "\\b(a|ante|bajo|con|contra|de|desde|en|entre|hacia|hasta|mediante|para|por|seg[úu]n|sin|sobre|tras|versus|vía) nadie\\b")) 

