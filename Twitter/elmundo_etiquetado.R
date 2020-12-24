#Crear la tabla etiquetada de todo el mundo

library(udpipe)
library(tidyverse)
library(magrittr)
'%ni%' <- Negate('%in%')
#udpipe_download_model(language = "spanish-ancora")
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.5-191206.udpipe')

elmundo <- read_delim("elmundo_TW.csv", delim = "\t")


elmundo_unnested <- elmundo %>% 
  mutate(contexto_reducido = 
           str_extract(text, 
                       "(:?\\b[a-záéíóúüñ]+\\b ){0,3}todo el mundo\\b(:? \\b[a-záéíóúüñ]+\\b){0,4}")) %>% #extraer el contexto reducido 
  mutate(word1 = str_replace(contexto_reducido, #extraer la primera palabra
                             "^(\\b[a-záéíóúüñ]+\\b)?( \\b[a-záéíóúüñ]+\\b){0,2} todo el mundo\\b.*$", 
                             "\\1"),
         word1 = ifelse(word1 == contexto_reducido, NA, word1)) %>% 
  mutate(word2 = str_replace(contexto_reducido, #extraer la segunda palabra
                             "^(\\b[a-záéíóúüñ]+\\b )?(\\b[a-záéíóúüñ]+\\b)?( \\b[a-záéíóúüñ]+\\b)? todo el mundo\\b.*$", 
                             "\\2"),
         word2 = ifelse(word2 == contexto_reducido, NA, word2)) %>%
  mutate(word3 = str_replace(contexto_reducido, #extraer la tercera palabra
                             "^(\\b[a-záéíóúüñ]+\\b ){0,2}(\\b[a-záéíóúüñ]+\\b)? todo el mundo\\b.*$", 
                             "\\2"),
         word3 = ifelse(word3 == contexto_reducido, NA, word3)) %>%
  mutate(word2 = ifelse((word2 == word3) & (word1 != word2), word1, word2), #arreglar lo que pasa en las primeras columnas: si no hay 3 palabras delante se producen diversas repeticiones
         word1 = ifelse((word1 == word2) & (word2 != word3), NA, word1), 
         word1 = ifelse((word1 == word2) & (word1 == word2), NA, word1), 
         word2 = ifelse(is.na(word1) & (word2 == word3), NA, word2)) %>% 
  mutate(word4 = str_replace(contexto_reducido, #extraer el sujeto seguido de 0 a 3 clíticos más la negación 
                             "^(\\b[a-záéíóúüñ]+\\b ){0,3}(todo el mundo ?((no|l[eao]s?|[mst]e|n?os) ){0,3})\\b.*$", 
                             "\\2"),
         word4 = ifelse(word4 == contexto_reducido, NA, word4)) %>%
  mutate(word5 = str_replace(contexto_reducido, #extraer la palabra tras el sujeto
                             "^(\\b[a-záéíóúüñ]+\\b ){0,2}\\btodo el mundo ((no|l[eao]s?|[mst]e|n?os) ){0,3}(\\b[a-záéíóúüñ]+\\b)?.*$", 
                             "\\4"),
         word5 = ifelse(word5 == contexto_reducido, NA, word5)) 

##La función para etiquetar cada una de las columnas
annotate_TW_elmundo <- function(column, name) {
  #column <- column %>% 
  #  subset(!is.na(column))
  udpipe_annotate(modelo_ancora, column) %>% 
    as_tibble() %>% 
    select(doc_id, lemma, token_id, upos, xpos, feats) %>% #lemma
    mutate(id_elmundo = str_remove(doc_id, "^doc"),
           id_elmundo = as.integer(id_elmundo)) %>%
    filter(!token_id == "1-2") %>% 
    set_colnames(str_c(colnames(.), c(rep(name, 6), ""), sep = "")) %>%
    select(-starts_with("doc_id"))
  
}


word_1_analisis <- annotate_TW_elmundo(elmundo_unnested$word1, "_word1")
word_2_analisis <- annotate_TW_elmundo(elmundo_unnested$word2, "_word2")
word_3_analisis <- annotate_TW_elmundo(elmundo_unnested$word3, "_word3")
word_5_analisis <- annotate_TW_elmundo(elmundo_unnested$word5, "_word5")

elmundo_annotated <- elmundo_unnested %>% 
  full_join(word_1_analisis) %>% 
  full_join(word_2_analisis) %>% 
  full_join(word_3_analisis) %>% 
  full_join(word_5_analisis)

elmundo_analysed_preparation <- elmundo_annotated %>% 
  mutate(elmundo_3sg_ant = three_sg_ant_cont(.),
         elmundo_3sg_perif = three_sg_ant_perif(.),
         elmundo_3sg_perif_prep = three_sg_ant_perif_prep(.),
         elmundo_3sg_pos = three_sg_posp(.),
         
         elmundo_3pl_ant = three_pl_ant_cont(.),
         elmundo_3pl_perif = three_pl_ant_perif(.),
         elmundo_3pl_perif_prep = three_pl_ant_perif_prep(.),
         elmundo_3pl_pos = three_pl_posp(.),
         
         elmundo_1pl_ant = first_pl_ant_cont(.),
         elmundo_1pl_perif = first_pl_ant_perif(.),
         elmundo_1pl_perif_prep = first_pl_ant_perif_prep(.),
         elmundo_1pl_pos = first_pl_posp(.),
         
         elmundo_2pl_ant = second_pl_ant_cont(.),
         elmundo_2pl_perif = second_pl_ant_perif(.),
         elmundo_2pl_perif_prep = second_pl_ant_perif_prep(.),
         elmundo_2pl_pos = second_pl_posp(.),
  ) %>% 
  mutate(results = elmundo_3sg_ant + elmundo_3sg_perif+ elmundo_3sg_perif_prep + elmundo_3sg_pos +
           elmundo_3pl_ant + elmundo_3pl_perif + elmundo_3pl_perif_prep + elmundo_3pl_pos + 
           elmundo_1pl_ant + elmundo_1pl_perif + elmundo_1pl_perif_prep + elmundo_1pl_pos +
           elmundo_2pl_ant + elmundo_2pl_perif + elmundo_2pl_perif_prep + elmundo_2pl_pos)


#Hay 30 filas con 2 resultados

elmundo_analysed_preparation %>% 
  count(results, sort = T)

#Para el separate hay que cambiar los valores de TRUE por el nombre de la columna

#One match
elmundo_analysed_preparation %>% 
  filter(results != 2) %>% 
  mutate(
    match = ifelse(elmundo_3sg_ant, "elmundo_3sg_ant",
                   ifelse(elmundo_3sg_perif, "elmundo_3sg_perif",
                          ifelse(elmundo_3sg_perif_prep, "elmundo_3sg_perif_prep",
                                 ifelse(elmundo_3sg_pos, "elmundo_3sg_pos",
                                        ifelse(elmundo_3pl_ant, "elmundo_3pl_ant",
                                               ifelse(elmundo_3pl_perif, "elmundo_3pl_perif",
                                                      ifelse(elmundo_3pl_perif_prep, "elmundo_3pl_perif_prep",
                                                             ifelse(elmundo_3pl_pos, "elmundo_3pl_pos",
                                                                    ifelse(elmundo_1pl_ant, "elmundo_1pl_ant",
                                                                           ifelse(elmundo_1pl_perif, "elmundo_1pl_perif",
                                                                                  ifelse(elmundo_1pl_perif_prep, "elmundo_1pl_perif_prep",
                                                                                         ifelse(elmundo_1pl_pos, "elmundo_1pl_pos",
                                                                                                ifelse(elmundo_2pl_ant, "elmundo_2pl_ant",
                                                                                                       ifelse(elmundo_2pl_perif, "elmundo_2pl_perif",
                                                                                                              ifelse(elmundo_2pl_perif_prep, "elmundo_2pl_perif_prep",
                                                                                                                     ifelse(elmundo_2pl_pos, "elmundo_2pl_pos", "FALSE"
                                                                                                                     ))))))))))))))))) %>% 
  count(match) #¿Cuáles son los más frecuentes (no es posible analizarlos manualmente)?

#seleccionar muestras aleatorias de los más frecuentes
set.seed(739)
sample_ant_3sg <- elmundo_analysed_preparation %>% 
  filter(elmundo_3sg_ant == TRUE) %>% 
  slice_sample(n = 300) %>% 
  select(id_elmundo)

set.seed(973)
sample_pos_3sg <- elmundo_analysed_preparation %>% 
  filter(elmundo_3sg_pos == TRUE) %>% 
  slice_sample(n = 300) %>% 
  select(id_elmundo)



#guardar
elmundo_analysed_onematch <- elmundo_analysed_preparation %>% 
  filter(results != 2) %>% 
  mutate(
    match = ifelse(elmundo_3sg_ant, "elmundo_3sg_ant",
                   ifelse(elmundo_3sg_perif, "elmundo_3sg_perif",
                          ifelse(elmundo_3sg_perif_prep, "elmundo_3sg_perif_prep",
                                 ifelse(elmundo_3sg_pos, "elmundo_3sg_pos",
                                        ifelse(elmundo_3pl_ant, "elmundo_3pl_ant",
                                               ifelse(elmundo_3pl_perif, "elmundo_3pl_perif",
                                                      ifelse(elmundo_3pl_perif_prep, "elmundo_3pl_perif_prep",
                                                             ifelse(elmundo_3pl_pos, "elmundo_3pl_pos",
                                                                    ifelse(elmundo_1pl_ant, "elmundo_1pl_ant",
                                                                           ifelse(elmundo_1pl_perif, "elmundo_1pl_perif",
                                                                                  ifelse(elmundo_1pl_perif_prep, "elmundo_1pl_perif_prep",
                                                                                         ifelse(elmundo_1pl_pos, "elmundo_1pl_pos",
                                                                                                ifelse(elmundo_2pl_ant, "elmundo_2pl_ant",
                                                                                                       ifelse(elmundo_2pl_perif, "elmundo_2pl_perif",
                                                                                                              ifelse(elmundo_2pl_perif_prep, "elmundo_2pl_perif_prep",
                                                                                                                     ifelse(elmundo_2pl_pos, "elmundo_2pl_pos", "FALSE"
                                                                                                                     ))))))))))))))))) 


elmundo_analysed_twomatches <- elmundo_analysed_preparation %>% 
  filter(results == 2) %>% 
  mutate(match = "ambiguous")

#juntarlos
elmundo_analysed <- elmundo_analysed_onematch %>% 
  add_row(elmundo_analysed_twomatches) %>% 
  mutate(
    random_sample = ifelse(id_elmundo %in% sample_ant_3sg, TRUE, 
                           ifelse(id_elmundo %in% sample_pos_3sg, TRUE, FALSE))
  ) %>% 
  rename(id_busqueda = id_elmundo)

#exportarla con las columnas relevantes (para la comprobación manual)
elmundo_analysed %>% 
  select(idtweets, id, text, fecha, place_name, place, id_busqueda, contexto_reducido, 
         results, match, random_sample) %>% 
  write_delim("elmundo_analysed_manual.csv", delim = "\t")


