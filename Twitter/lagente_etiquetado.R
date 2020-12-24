#Crear la tabla etiquetada de la gente

library(udpipe)
library(tidyverse)
library(magrittr)
'%ni%' <- Negate('%in%')
#udpipe_download_model(language = "spanish-ancora")
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.5-191206.udpipe')

lagente <- read_delim("lagente_TW.csv", delim = "\t")


lagente_unnested <- lagente %>% 
  mutate(contexto_reducido = 
           str_extract(text, 
                       "(:?\\b[a-záéíóúüñ]+\\b ){0,3}la gente\\b(:? \\b[a-záéíóúüñ]+\\b){0,4}")) %>% #extraer el contexto reducido 
  mutate(word1 = str_replace(contexto_reducido, #extraer la primera palabra
                             "^(\\b[a-záéíóúüñ]+\\b)?( \\b[a-záéíóúüñ]+\\b){0,2} la gente\\b.*$", 
                             "\\1"),
         word1 = ifelse(word1 == contexto_reducido, NA, word1)) %>% 
  mutate(word2 = str_replace(contexto_reducido, #extraer la segunda palabra
                             "^(\\b[a-záéíóúüñ]+\\b )?(\\b[a-záéíóúüñ]+\\b)?( \\b[a-záéíóúüñ]+\\b)? la gente\\b.*$", 
                             "\\2"),
         word2 = ifelse(word2 == contexto_reducido, NA, word2)) %>%
  mutate(word3 = str_replace(contexto_reducido, #extraer la tercera palabra
                             "^(\\b[a-záéíóúüñ]+\\b ){0,2}(\\b[a-záéíóúüñ]+\\b)? la gente\\b.*$", 
                             "\\2"),
         word3 = ifelse(word3 == contexto_reducido, NA, word3)) %>%
  mutate(word2 = ifelse((word2 == word3) & (word1 != word2), word1, word2), #arreglar lo que pasa en las primeras columnas: si no hay 3 palabras delante se producen diversas repeticiones
         word1 = ifelse((word1 == word2) & (word2 != word3), NA, word1), 
         word1 = ifelse((word1 == word2) & (word1 == word2), NA, word1), 
         word2 = ifelse(is.na(word1) & (word2 == word3), NA, word2)) %>% 
  mutate(word4 = str_replace(contexto_reducido, #extraer el sujeto seguido de 0 a 3 clíticos más la negación 
                             "^(\\b[a-záéíóúüñ]+\\b ){0,3}(la gente ?((no|l[eao]s?|[mst]e|n?os) ){0,3})\\b.*$", 
                             "\\2"),
         word4 = ifelse(word4 == contexto_reducido, NA, word4)) %>%
  mutate(word5 = str_replace(contexto_reducido, #extraer la palabra tras el sujeto
                             "^(\\b[a-záéíóúüñ]+\\b ){0,2}\\bla gente ((no|l[eao]s?|[mst]e|n?os) ){0,3}(\\b[a-záéíóúüñ]+\\b)?.*$", 
                             "\\4"),
         word5 = ifelse(word5 == contexto_reducido, NA, word5)) 

##La función para etiquetar cada una de las columnas
annotate_TW_lagente <- function(column, name) {
  #column <- column %>% 
  #  subset(!is.na(column))
  udpipe_annotate(modelo_ancora, column) %>% 
    as_tibble() %>% 
    select(doc_id, lemma, token_id, upos, xpos, feats) %>% #lemma
    mutate(id_lagente = str_remove(doc_id, "^doc"),
           id_lagente = as.integer(id_lagente)) %>%
    filter(!token_id == "1-2") %>% 
    set_colnames(str_c(colnames(.), c(rep(name, 6), ""), sep = "")) %>%
    select(-starts_with("doc_id"))
  
}

word_1_analisis <- annotate_TW_lagente(lagente_unnested$word1, "_word1")
word_2_analisis <- annotate_TW_lagente(lagente_unnested$word2, "_word2")
word_3_analisis <- annotate_TW_lagente(lagente_unnested$word3, "_word3")
word_5_analisis <- annotate_TW_lagente(lagente_unnested$word5, "_word5")

lagente_annotated <- lagente_unnested %>% 
  full_join(word_1_analisis) %>% 
  full_join(word_2_analisis) %>% 
  full_join(word_3_analisis) %>% 
  full_join(word_5_analisis)

lagente_analysed_preparation <- lagente_annotated %>% 
  mutate(lagente_3sg_ant = three_sg_ant_cont(.),
         lagente_3sg_perif = three_sg_ant_perif(.),
         lagente_3sg_perif_prep = three_sg_ant_perif_prep(.),
         lagente_3sg_pos = three_sg_posp(.),
         
         lagente_3pl_ant = three_pl_ant_cont(.),
         lagente_3pl_perif = three_pl_ant_perif(.),
         lagente_3pl_perif_prep = three_pl_ant_perif_prep(.),
         lagente_3pl_pos = three_pl_posp(.),
         
         lagente_1pl_ant = first_pl_ant_cont(.),
         lagente_1pl_perif = first_pl_ant_perif(.),
         lagente_1pl_perif_prep = first_pl_ant_perif_prep(.),
         lagente_1pl_pos = first_pl_posp(.),
        
         lagente_2pl_ant = second_pl_ant_cont(.),
         lagente_2pl_perif = second_pl_ant_perif(.),
         lagente_2pl_perif_prep = second_pl_ant_perif_prep(.),
         lagente_2pl_pos = second_pl_posp(.),
         ) %>% 
  mutate(results = lagente_3sg_ant + lagente_3sg_perif+ lagente_3sg_perif_prep + lagente_3sg_pos +
           lagente_3pl_ant + lagente_3pl_perif + lagente_3pl_perif_prep + lagente_3pl_pos + 
           lagente_1pl_ant + lagente_1pl_perif + lagente_1pl_perif_prep + lagente_1pl_pos +
           lagente_2pl_ant + lagente_2pl_perif + lagente_2pl_perif_prep + lagente_2pl_pos)
  

#Hay 98 filas con 2 resultados

#lagente_analysed_preparation %>% 
#  count(results, sort = T)

#Para el separate hay que cambiar los valores de TRUE por el nombre de la columna

#One match
lagente_analysed_preparation %>% 
  filter(results != 2) %>% 
  mutate(
    match = ifelse(lagente_3sg_ant, "lagente_3sg_ant",
                   ifelse(lagente_3sg_perif, "lagente_3sg_perif",
                          ifelse(lagente_3sg_perif_prep, "lagente_3sg_perif_prep",
                                 ifelse(lagente_3sg_pos, "lagente_3sg_pos",
                                        ifelse(lagente_3pl_ant, "lagente_3pl_ant",
                                               ifelse(lagente_3pl_perif, "lagente_3pl_perif",
                                                      ifelse(lagente_3pl_perif_prep, "lagente_3pl_perif_prep",
                                                             ifelse(lagente_3pl_pos, "lagente_3pl_pos",
                                                                    ifelse(lagente_1pl_ant, "lagente_1pl_ant",
                                                                           ifelse(lagente_1pl_perif, "lagente_1pl_perif",
                                                                                  ifelse(lagente_1pl_perif_prep, "lagente_1pl_perif_prep",
                                                                                         ifelse(lagente_1pl_pos, "lagente_1pl_pos",
                                                                                                ifelse(lagente_2pl_ant, "lagente_2pl_ant",
                                                                                                       ifelse(lagente_2pl_perif, "lagente_2pl_perif",
                                                                                                              ifelse(lagente_2pl_perif_prep, "lagente_2pl_perif_prep",
                                                                                                                     ifelse(lagente_2pl_pos, "lagente_2pl_pos", "FALSE"
                   ))))))))))))))))) %>% 
  count(match) #¿Cuáles son los más frecuentes (no es posible analizarlos manualmente)?

#seleccionar muestras aleatorias de los más frecuentes
set.seed(739)
sample_ant_3sg <- lagente_analysed_preparation %>% 
  filter(lagente_3sg_ant == TRUE) %>% 
  slice_sample(n = 300) %>% 
  select(id_lagente)

set.seed(973)
sample_pos_3sg <- lagente_analysed_preparation %>% 
  filter(lagente_3sg_pos == TRUE) %>% 
  slice_sample(n = 300) %>% 
  select(id_lagente)
  
  
    
#guardar
lagente_analysed_onematch <- lagente_analysed_preparation %>% 
  filter(results != 2) %>% 
  mutate(
    match = ifelse(lagente_3sg_ant, "lagente_3sg_ant",
                   ifelse(lagente_3sg_perif, "lagente_3sg_perif",
                          ifelse(lagente_3sg_perif_prep, "lagente_3sg_perif_prep",
                                 ifelse(lagente_3sg_pos, "lagente_3sg_pos",
                                        ifelse(lagente_3pl_ant, "lagente_3pl_ant",
                                               ifelse(lagente_3pl_perif, "lagente_3pl_perif",
                                                      ifelse(lagente_3pl_perif_prep, "lagente_3pl_perif_prep",
                                                             ifelse(lagente_3pl_pos, "lagente_3pl_pos",
                                                                    ifelse(lagente_1pl_ant, "lagente_1pl_ant",
                                                                           ifelse(lagente_1pl_perif, "lagente_1pl_perif",
                                                                                  ifelse(lagente_1pl_perif_prep, "lagente_1pl_perif_prep",
                                                                                         ifelse(lagente_1pl_pos, "lagente_1pl_pos",
                                                                                                ifelse(lagente_2pl_ant, "lagente_2pl_ant",
                                                                                                       ifelse(lagente_2pl_perif, "lagente_2pl_perif",
                                                                                                              ifelse(lagente_2pl_perif_prep, "lagente_2pl_perif_prep",
                                                                                                                     ifelse(lagente_2pl_pos, "lagente_2pl_pos", "FALSE"
                                                                                                                     ))))))))))))))))) 


lagente_analysed_twomatches <- lagente_analysed_preparation %>% 
  filter(results == 2) %>% 
  mutate(match = "ambiguous")

#juntarlos
lagente_analysed <- lagente_analysed_onematch %>% 
  add_row(lagente_analysed_twomatches) %>% 
  mutate(
    random_sample = ifelse(id_lagente %in% sample_ant_3sg, TRUE, 
                           ifelse(id_lagente %in% sample_pos_3sg, TRUE, FALSE))
  ) %>% 
  rename(id_busqueda = id_lagente)
  
#exportarla con las columnas relevantes (para la comprobación manual)
lagente_analysed %>% 
  select(idtweets, id, text, fecha, place_name, place, id_busqueda, contexto_reducido, 
         results, match, random_sample) %>% 
  write_delim("lagente_analysed_manual.csv", delim = "\t")
    
    
