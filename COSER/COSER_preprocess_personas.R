#31 de octubre de 2020
#Preprosecesamiento COSER concordancia ad sensum sujetos

library(tidyverse)
library(fs)


#Leer los .txt para el preprocesado
txt_files <- dir_ls(regexp = "\\.txt$")
#hubo que reparar algunas búsquedas:
#txt_files <- c("familia.txt", "gente.txt") #primera reparación
#txt_files <- dir_ls(regexp = "^(a_|to_|todo_l|naide|nadien).*\\.txt$") #segunda reparación
#txt_files <- c("tol_mundo.txt", "to_l_el_mundo.txt") #tercera reparación


#La función read_lines_COSER() es de creación propia: ver "COSER_preprocess.R"
map(txt_files, read_lines_COSER) 
#for map


#Transformar en .csv los .txt preprocesados
txt_files2 <- dir_ls(regexp = "_preprocessed\\.txt$")
#txt_files2 <- dir_ls(regexp = "^(familia|gente)_preprocessed\\.txt$") #primera reparación
#txt_files2 <- dir_ls(regexp = "^(a_|to_|todo_l|naide|nadien).*_preprocessed\\.txt$") #segunda reparación
#txt_files2 <- dir_ls(regexp = "^(tol_mundo|to_l_el_mundo)_preprocessed\\.txt$") #tercera reparación

#La función read_delim_COSER() es de creación propia: ver "COSER_preprocess.R"
conc_ad_sensum <- map_dfr(txt_files2, read_delim_COSER, .id = "source") #leemos con map_dfr para tener un data_frame como resultado


conc_ad_sensum_clean <- conc_ad_sensum %>% 
  mutate(source = str_replace(source, "_preprocessed\\.txt$", "")) %>% #limpiar el campo source para que se queda la palabra buscada
  rename(busqueda = source) %>%  #renombrarlo como busqueda
  #mutate(id_busqueda = seq(from = 4505, to = (4504 + nrow(conc_ad_sensum)), by = 1), #para los que faltaban, primera vez
  #mutate(id_busqueda = seq(from = 5238, to = (5237 + nrow(conc_ad_sensum)), by = 1), #para los que faltaban, segunda vez
  #mutate(id_busqueda = seq(from = 6470, to = (6469 + nrow(conc_ad_sensum)), by = 1), #para los que faltaban, tercera vez
  mutate(id_busqueda = seq_along(text), #crear columna de id_busqueda (importante, porque vamos a duplicar resultados)
         n_results = str_count(text, "\\*\\*\\*")/2, #crear columna con número de resultados por campo
         id_result = 1) #crear columna de id_result, que cambiará para cada número de resultado

#Al contar el número de resultados vemos que hay muchos repetidos
conc_ad_sensum_clean %>%
  count(n_results) 


#Vamos a extraerlos para eliminar los asteriscos de los primeros resultados y juntar el resultado
#El objetivo es tener un resultado por columna contando con los repetidos y así poder separar en contexto-resultado-contexto
#Primero eliminamos los asteriscos del primer resultado de los que tienen más de uno
conc_ad_sensum_clean_segundo <- conc_ad_sensum_clean %>% 
  filter(n_results > 1) %>%  #los que tienen 2 o más resultados
  mutate(text = str_replace(text, "\\*\\*\\*", ""),
         text = str_replace(text, "\\*\\*\\*", ""), #borramos los dos primeros
         n_results = str_count(text, "\\*\\*\\*")/2,
         id_result = 2)

#Siguen quedando, claro
conc_ad_sensum_clean_segundo %>%
  count(n_results)

#Ahora partimos de esa segunda tabla y repetimos el proceso para localizar el tercer resultado
conc_ad_sensum_clean_tercero <- conc_ad_sensum_clean_segundo %>% 
  filter(n_results > 1) %>%  #los que tienen 2 o más resultados
  mutate(text = str_replace(text, "\\*\\*\\*", ""),
         text = str_replace(text, "\\*\\*\\*", ""), #borramos los dos primeros
         n_results = str_count(text, "\\*\\*\\*")/2,
         id_result = 3)

#Siguen quedando, claro, cada vez menos :)
conc_ad_sensum_clean_tercero %>%
  count(n_results)

#Repetimos
conc_ad_sensum_clean_cuarto <- conc_ad_sensum_clean_tercero %>% 
  filter(n_results > 1) %>%  #los que tienen 2 o más resultados
  mutate(text = str_replace(text, "\\*\\*\\*", ""),
         text = str_replace(text, "\\*\\*\\*", ""), #borramos los dos primeros
         n_results = str_count(text, "\\*\\*\\*")/2,
         id_result = 4)

#Siguen quedando, claro, cada vez menos :)
conc_ad_sensum_clean_cuarto %>%
  count(n_results)

#Repetimos
conc_ad_sensum_clean_quinto <- conc_ad_sensum_clean_cuarto %>% 
  filter(n_results > 1) %>%  #los que tienen 2 o más resultados
  mutate(text = str_replace(text, "\\*\\*\\*", ""),
         text = str_replace(text, "\\*\\*\\*", ""), #borramos los dos primeros
         n_results = str_count(text, "\\*\\*\\*")/2,
         id_result = 5)

#Siguen quedando, claro, cada vez menos :)
conc_ad_sensum_clean_quinto %>%
  count(n_results)

#Repetimos
conc_ad_sensum_clean_sexto <- conc_ad_sensum_clean_quinto %>% 
  filter(n_results > 1) %>%  #los que tienen 2 o más resultados
  mutate(text = str_replace(text, "\\*\\*\\*", ""),
         text = str_replace(text, "\\*\\*\\*", ""), #borramos los dos primeros
         n_results = str_count(text, "\\*\\*\\*")/2,
         id_result = 6)

#Siguen quedando, claro, cada vez menos :)
conc_ad_sensum_clean_sexto %>%
  count(n_results)

#Repetimos
conc_ad_sensum_clean_septimo <- conc_ad_sensum_clean_sexto %>% 
  filter(n_results > 1) %>%  #los que tienen 2 o más resultados
  mutate(text = str_replace(text, "\\*\\*\\*", ""),
         text = str_replace(text, "\\*\\*\\*", ""), #borramos los dos primeros
         n_results = str_count(text, "\\*\\*\\*")/2,
         id_result = 7)

#Siguen quedando, claro, cada vez menos :)
conc_ad_sensum_clean_septimo %>%
  count(n_results)

#Repetimos
conc_ad_sensum_clean_octavo <- conc_ad_sensum_clean_septimo %>% 
  filter(n_results > 1) %>%  #los que tienen 2 o más resultados
  mutate(text = str_replace(text, "\\*\\*\\*", ""),
         text = str_replace(text, "\\*\\*\\*", ""), #borramos los dos primeros
         n_results = str_count(text, "\\*\\*\\*")/2,
         id_result = 8)

#Siguen quedando, claro, cada vez menos :)
conc_ad_sensum_clean_octavo %>%
  count(n_results)

#Repetimos
conc_ad_sensum_clean_noveno <- conc_ad_sensum_clean_octavo %>% 
  filter(n_results > 1) %>%  #los que tienen 2 o más resultados
  mutate(text = str_replace(text, "\\*\\*\\*", ""),
         text = str_replace(text, "\\*\\*\\*", ""), #borramos los dos primeros
         n_results = str_count(text, "\\*\\*\\*")/2,
         id_result = 9)

#Siguen quedando, claro, cada vez menos :)
conc_ad_sensum_clean_noveno %>%
  count(n_results)

#Repetimos
conc_ad_sensum_clean_decimo <- conc_ad_sensum_clean_noveno %>% 
  filter(n_results > 1) %>%  #los que tienen 2 o más resultados
  mutate(text = str_replace(text, "\\*\\*\\*", ""),
         text = str_replace(text, "\\*\\*\\*", ""), #borramos los dos primeros
         n_results = str_count(text, "\\*\\*\\*")/2,
         id_result = 10)

#Ya no quedan más
conc_ad_sensum_clean_decimo %>%
  count(n_results)

#Ahora las juntamos todos

conc_ad_sensum_clean <- conc_ad_sensum_clean %>% 
  add_row(conc_ad_sensum_clean_segundo) %>% 
  add_row(conc_ad_sensum_clean_tercero) %>% 
  add_row(conc_ad_sensum_clean_cuarto) %>% 
  add_row(conc_ad_sensum_clean_quinto) %>% 
  add_row(conc_ad_sensum_clean_sexto) %>% 
  add_row(conc_ad_sensum_clean_septimo) %>% 
  add_row(conc_ad_sensum_clean_octavo) %>% 
  add_row(conc_ad_sensum_clean_noveno) %>% 
  add_row(conc_ad_sensum_clean_decimo) %>% #y las separamos por los asteriscos en tres columnas
  separate(text, into = c("contexto_previo", "match", "contexto_posterior"), 
           sep = "\\*\\*\\*", extra = "merge", remove = F) %>% #con `extra = "merge"` hacemos que no se descarten los resultados posteriores
  mutate(contexto_posterior = str_replace_all(contexto_posterior, "\\*\\*\\*", "")) %>%  #remplazamos los asteriscos que queden del asterisco previo
  select(id_busqueda, id_result, n_results, busqueda, provincia, enclave, COSERID, text, contexto_previo, match, contexto_posterior) #reordenamos

write_delim(conc_ad_sensum_clean, "conc_ad_sensum_clean.csv", delim = "\t") 
#write_delim(conc_ad_sensum_clean, "conc_ad_sensum_clean_late_additions.csv", delim = "\t") #me faltaban gente y familia sin artículo, añadidos ("conc_ad_sensum_clean_late_additions.csv")  
#write_delim(conc_ad_sensum_clean, "conc_ad_sensum_clean_late_additions3.csv", delim = "\t") #tercera reparación ("conc_ad_sensum_clean_late_additions3.csv")  



##Arreglar el problema con los matches parciales (primera adición)
#Con las búsquedas por regex, los matches no dan palabras enteras.
#Me di cuenta demasiado tarde, cuando ya había hecho cambios en el archivo de Excel
#Leo el .csv original, hago los cambios, guardarlos como un archivo nuevo y hago un join con el excel

conc_ad_sensum_clean <- read_delim("conc_ad_sensum_clean_late_additions.csv", delim = "\t")
conc_ad_sensum_clean %>% 
  distinct(match)

conc_ad_sensum_clean %>% 
  separate(match, into = c("match_ant", "match"), sep = " ") %>% 
  #distinct(match_ant) #la letra suelta solo podría ser palabra cuando es "e, o, u, y, Y". Con "Y" es obvio que sí. Con 
  #"e" no podría ser por el contexto vocálico posterior. "u" y "o" alternan como conjunciones en español rural, así que sí.
  #filter(match_ant %in% c("u", "o", "y")) #235 resultados, seamos más precisos
  filter(match_ant == "o") %>% #con o son todo palabras salvo dos NA, veamos (ejecutar con las siguientes dos líneas)
  #filter(match_ant == "u") %>% #con u son todo posesivos (ejecutar con las siguientes dos líneas)
  #mutate(ultima_palabra = str_extract(contexto_previo, "\\b\\w+$")) %>% #ejecutar con el filter anterior
  #count(ultima_palabra, match_ant) #ejecutar con el filter anterior
  filter(is.na(ultima_palabra)) %>% #ejecutar sin el mutate y el count anteriores, comprobar los NA
  mutate(contexto_previo = str_extract(contexto_previo, ".{70}$")) %>% #ejecutar sin el mutate y el count anteriores, comprobar los NA
  select(contexto_previo, match_ant) #ejecutar sin el mutate y el count anteriores, comprobar los NA
#son espacios! No hacen falta más comprobaciones

#Limpiar la tabla
conc_ad_sensum_clean <- conc_ad_sensum_clean %>% 
  separate(match, into = c("match_ant", "match"), sep = " ") %>% 
  mutate(contexto_previo = str_c(contexto_previo, match_ant)) %>% 
  #mutate(ultima_palabra = str_extract(contexto_previo, "\\b\\w+$")) %>% #para comprobar
  #distinct(ultima_palabra) #para comprobar, hay NA…
  #filter(is.na(ultima_palabra)) %>% #comprobar NA
  #mutate(contexto_previo = str_extract(contexto_previo, ".{70}$")) %>% #comprobar NA
  #select(contexto_previo, match_ant) #comprobar NA, son signos de puntuación
  select(-match_ant) %>% 
  mutate(primera_letra = str_extract(contexto_posterior, "^.")) %>% 
  mutate(match = str_c(match, primera_letra)) %>% 
  #distinct(match) #para comprobar
  select(-primera_letra) %>% 
  mutate(contexto_posterior = str_replace(contexto_posterior, "^.", "")) #%>% 
  #select(contexto_posterior) #para comprobar

write_delim(conc_ad_sensum_clean, "conc_ad_sensum_clean_late_additions_fixed.csv", delim = "\t") 


##### Arreglar gente_familia_original.csv
##### con conc_ad_sensum_clean_late_additions_fixed.csv

original <- read_delim("gente_familia_original.csv", delim = ",")
fixed <- read_delim("conc_ad_sensum_clean_late_additions_fixed.csv", delim = "\t")

original <- original %>% 
  select(id_busqueda, id_result, sujeto, V1_persona, V1_posicion, V1_adyacencia, Vs_persona, V2_persona, observaciones)

fixed <- fixed %>% 
  left_join(original) #%>% 
  #count(match)

write_delim(fixed, "gente_familia_fixed.csv", delim = "\t") 




##Arreglar el problema de los matches parciales (segunda adición)
#Con las búsquedas por regex, los matches no dan palabras enteras.
conc_ad_sensum_clean %>% 
  distinct(match) #me interesan solo c("a famili", "a gent")
ultima_pal <- conc_ad_sensum_clean %>% 
  filter(match %in% c("a famili", "a gent")) %>% 
  mutate(ultima_palabra = str_extract(contexto_previo, "\\b\\w+$")) %>% 
  distinct(ultima_palabra) #las reviso manualmente 
#Las que son palabras en sí mismas son: un, es, y, hay, Un, Es, to, Y, recogí. Todas podría tener 'a' detrás, voy a mirarlo
#Es y Un no las reviso enteras, porque no tienen sentido con prep 'a' detrás. Miro las demás y son palabras partidas:
#se puede pegar todo
ultima_pal_contexto <- conc_ad_sensum_clean %>% 
  filter(match %in% c("a famili", "a gent")) %>% 
  filter(str_detect(contexto_previo, "\\b([Yy]|hay|to|recogí)$")) %>% 
  mutate(contexto_previo = str_extract(contexto_previo, ".{70}$")) %>% 
  select(contexto_previo, match)  #las reviso manualmente
ultima_pal_contexto %>% 
  print(n = 162)
#Comprobar regex para extraer primer carácter de contexto_posterior (debería ser o 'a' o 'e')
conc_ad_sensum_clean %>% 
  filter(match == "a famili") %>%
  #filter(match == "a gent") %>%
  mutate(primera_letra = str_extract(contexto_posterior, "^.")) %>% 
  #distinct(primera_letra) #correcto
  select(primera_letra, contexto_posterior) #la letra se queda

#Modificar y unir
#crear una tabla con las que tenían problema en match y arreglarlo
conc_ad_sensum_clean_1 <- conc_ad_sensum_clean %>% 
  filter(match %in% c("a famili", "a gent")) %>% 
  separate(match, into = c("match_ant", "match"), sep = " ") %>% 
  #distinct(match_ant) #para comprobar
  mutate(contexto_previo = str_c(contexto_previo, match_ant)) %>% 
  #mutate(ultima_palabra = str_extract(contexto_previo, "\\b\\w+$")) %>% #para comprobar
  #distinct(ultima_palabra) #para comprobar
  select(-match_ant) %>% 
  mutate(primera_letra = str_extract(contexto_posterior, "^.")) %>% 
  mutate(match = str_c(match, primera_letra)) %>% 
  #distinct(match) #para comprobar
  select(-primera_letra) %>% 
  mutate(contexto_posterior = str_replace(contexto_posterior, "^.", "")) #%>% 
#select(contexto_posterior) #para comprobar

#otra tabla con las que no tenían problemas en match
conc_ad_sensum_clean_2 <- conc_ad_sensum_clean %>% 
  filter(!match %in% c("a famili", "a gent"))
nrow(conc_ad_sensum_clean_1) + nrow(conc_ad_sensum_clean_2) #para comprobar

#unirlas
conc_ad_sensum_clean <- conc_ad_sensum_clean_1 %>% 
  add_row(conc_ad_sensum_clean_2)

#write_delim(conc_ad_sensum_clean, "conc_ad_sensum_clean_late_additions2.csv", delim = "\t") #segunda corrección a la primera búsqueda, añadidos ("conc_ad_sensum_clean_late_additions_2.csv")  
