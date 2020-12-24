#Crear la tabla etiquetada de la familia


library(udpipe)
library(tidyverse)
library(magrittr)
'%ni%' <- Negate('%in%')
#udpipe_download_model(language = "spanish-ancora")
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.5-191206.udpipe')

lafamilia <- read_delim("lafamilia_TW.csv", delim = "\t")


lafamilia_unnested <- lafamilia %>% 
  mutate(contexto_reducido = 
           str_extract(text, 
                       "(:?\\b[a-záéíóúüñ]+\\b ){0,3}la familia\\b(:? \\b[a-záéíóúüñ]+\\b){0,4}")) %>% #extraer el contexto reducido 
  mutate(word1 = str_replace(contexto_reducido, #extraer la primera palabra
                             "^(\\b[a-záéíóúüñ]+\\b)?( \\b[a-záéíóúüñ]+\\b){0,2} la familia\\b.*$", 
                             "\\1"),
         word1 = ifelse(word1 == contexto_reducido, NA, word1)) %>% 
  mutate(word2 = str_replace(contexto_reducido, #extraer la segunda palabra
                             "^(\\b[a-záéíóúüñ]+\\b )?(\\b[a-záéíóúüñ]+\\b)?( \\b[a-záéíóúüñ]+\\b)? la familia\\b.*$", 
                             "\\2"),
         word2 = ifelse(word2 == contexto_reducido, NA, word2)) %>%
  mutate(word3 = str_replace(contexto_reducido, #extraer la tercera palabra
                             "^(\\b[a-záéíóúüñ]+\\b ){0,2}(\\b[a-záéíóúüñ]+\\b)? la familia\\b.*$", 
                             "\\2"),
         word3 = ifelse(word3 == contexto_reducido, NA, word3)) %>%
  mutate(word2 = ifelse((word2 == word3) & (word1 != word2), word1, word2), #arreglar lo que pasa en las primeras columnas: si no hay 3 palabras delante se producen diversas repeticiones
         word1 = ifelse((word1 == word2) & (word2 != word3), NA, word1), 
         word1 = ifelse((word1 == word2) & (word1 == word2), NA, word1), 
         word2 = ifelse(is.na(word1) & (word2 == word3), NA, word2)) %>% 
  mutate(word4 = str_replace(contexto_reducido, #extraer el sujeto seguido de 0 a 3 clíticos más la negación 
                             "^(\\b[a-záéíóúüñ]+\\b ){0,3}(la familia ?((no|l[eao]s?|[mst]e|n?os) ){0,3})\\b.*$", 
                             "\\2"),
         word4 = ifelse(word4 == contexto_reducido, NA, word4)) %>%
  mutate(word5 = str_replace(contexto_reducido, #extraer la palabra tras el sujeto
                             "^(\\b[a-záéíóúüñ]+\\b ){0,2}\\bla familia ((no|l[eao]s?|[mst]e|n?os) ){0,3}(\\b[a-záéíóúüñ]+\\b)?.*$", 
                             "\\4"),
         word5 = ifelse(word5 == contexto_reducido, NA, word5)) 

##La función para etiquetar cada una de las columnas
annotate_TW_lafamilia <- function(column, name) {
  #column <- column %>% 
  #  subset(!is.na(column))
  udpipe_annotate(modelo_ancora, column) %>% 
    as_tibble() %>% 
    select(doc_id, lemma, token_id, upos, xpos, feats) %>% #lemma
    mutate(id_lafamilia = str_remove(doc_id, "^doc"),
           id_lafamilia = as.integer(id_lafamilia)) %>%
    filter(!token_id == "1-2") %>% 
    set_colnames(str_c(colnames(.), c(rep(name, 6), ""), sep = "")) %>%
    select(-starts_with("doc_id"))
  
}


word_1_analisis <- annotate_TW_lafamilia(lafamilia_unnested$word1, "_word1")
word_2_analisis <- annotate_TW_lafamilia(lafamilia_unnested$word2, "_word2")
word_3_analisis <- annotate_TW_lafamilia(lafamilia_unnested$word3, "_word3")
word_5_analisis <- annotate_TW_lafamilia(lafamilia_unnested$word5, "_word5")

lafamilia_annotated <- lafamilia_unnested %>% 
  full_join(word_1_analisis) %>% 
  full_join(word_2_analisis) %>% 
  full_join(word_3_analisis) %>% 
  full_join(word_5_analisis)

lafamilia_analysed_preparation <- lafamilia_annotated %>% 
  mutate(lafamilia_3sg_ant = three_sg_ant_cont(.),
         lafamilia_3sg_perif = three_sg_ant_perif(.),
         lafamilia_3sg_perif_prep = three_sg_ant_perif_prep(.),
         lafamilia_3sg_pos = three_sg_posp(.),
         
         lafamilia_3pl_ant = three_pl_ant_cont(.),
         lafamilia_3pl_perif = three_pl_ant_perif(.),
         lafamilia_3pl_perif_prep = three_pl_ant_perif_prep(.),
         lafamilia_3pl_pos = three_pl_posp(.),
         
         lafamilia_1pl_ant = first_pl_ant_cont(.),
         lafamilia_1pl_perif = first_pl_ant_perif(.),
         lafamilia_1pl_perif_prep = first_pl_ant_perif_prep(.),
         lafamilia_1pl_pos = first_pl_posp(.),
         
         lafamilia_2pl_ant = second_pl_ant_cont(.),
         lafamilia_2pl_perif = second_pl_ant_perif(.),
         lafamilia_2pl_perif_prep = second_pl_ant_perif_prep(.),
         lafamilia_2pl_pos = second_pl_posp(.),
  ) %>% 
  mutate(results = lafamilia_3sg_ant + lafamilia_3sg_perif+ lafamilia_3sg_perif_prep + lafamilia_3sg_pos +
           lafamilia_3pl_ant + lafamilia_3pl_perif + lafamilia_3pl_perif_prep + lafamilia_3pl_pos + 
           lafamilia_1pl_ant + lafamilia_1pl_perif + lafamilia_1pl_perif_prep + lafamilia_1pl_pos +
           lafamilia_2pl_ant + lafamilia_2pl_perif + lafamilia_2pl_perif_prep + lafamilia_2pl_pos)


#Hay 9 filas con 2 resultados

lafamilia_analysed_preparation %>% 
  count(results, sort = T)

#Para el separate hay que cambiar los valores de TRUE por el nombre de la columna

#One match
lafamilia_analysed_preparation %>% 
  filter(results != 2) %>% 
  mutate(
    match = ifelse(lafamilia_3sg_ant, "lafamilia_3sg_ant",
                   ifelse(lafamilia_3sg_perif, "lafamilia_3sg_perif",
                          ifelse(lafamilia_3sg_perif_prep, "lafamilia_3sg_perif_prep",
                                 ifelse(lafamilia_3sg_pos, "lafamilia_3sg_pos",
                                        ifelse(lafamilia_3pl_ant, "lafamilia_3pl_ant",
                                               ifelse(lafamilia_3pl_perif, "lafamilia_3pl_perif",
                                                      ifelse(lafamilia_3pl_perif_prep, "lafamilia_3pl_perif_prep",
                                                             ifelse(lafamilia_3pl_pos, "lafamilia_3pl_pos",
                                                                    ifelse(lafamilia_1pl_ant, "lafamilia_1pl_ant",
                                                                           ifelse(lafamilia_1pl_perif, "lafamilia_1pl_perif",
                                                                                  ifelse(lafamilia_1pl_perif_prep, "lafamilia_1pl_perif_prep",
                                                                                         ifelse(lafamilia_1pl_pos, "lafamilia_1pl_pos",
                                                                                                ifelse(lafamilia_2pl_ant, "lafamilia_2pl_ant",
                                                                                                       ifelse(lafamilia_2pl_perif, "lafamilia_2pl_perif",
                                                                                                              ifelse(lafamilia_2pl_perif_prep, "lafamilia_2pl_perif_prep",
                                                                                                                     ifelse(lafamilia_2pl_pos, "lafamilia_2pl_pos", "FALSE"
                                                                                                                     ))))))))))))))))) %>% 
  count(match) #¿Cuáles son los más frecuentes (no es posible analizarlos manualmente)?

#guardar
lafamilia_analysed_onematch <- lafamilia_analysed_preparation %>% 
  filter(results != 2) %>% 
  mutate(
    match = ifelse(lafamilia_3sg_ant, "lafamilia_3sg_ant",
                   ifelse(lafamilia_3sg_perif, "lafamilia_3sg_perif",
                          ifelse(lafamilia_3sg_perif_prep, "lafamilia_3sg_perif_prep",
                                 ifelse(lafamilia_3sg_pos, "lafamilia_3sg_pos",
                                        ifelse(lafamilia_3pl_ant, "lafamilia_3pl_ant",
                                               ifelse(lafamilia_3pl_perif, "lafamilia_3pl_perif",
                                                      ifelse(lafamilia_3pl_perif_prep, "lafamilia_3pl_perif_prep",
                                                             ifelse(lafamilia_3pl_pos, "lafamilia_3pl_pos",
                                                                    ifelse(lafamilia_1pl_ant, "lafamilia_1pl_ant",
                                                                           ifelse(lafamilia_1pl_perif, "lafamilia_1pl_perif",
                                                                                  ifelse(lafamilia_1pl_perif_prep, "lafamilia_1pl_perif_prep",
                                                                                         ifelse(lafamilia_1pl_pos, "lafamilia_1pl_pos",
                                                                                                ifelse(lafamilia_2pl_ant, "lafamilia_2pl_ant",
                                                                                                       ifelse(lafamilia_2pl_perif, "lafamilia_2pl_perif",
                                                                                                              ifelse(lafamilia_2pl_perif_prep, "lafamilia_2pl_perif_prep",
                                                                                                                     ifelse(lafamilia_2pl_pos, "lafamilia_2pl_pos", "FALSE"
                                                                                                                     ))))))))))))))))) 


lafamilia_analysed_twomatches <- lafamilia_analysed_preparation %>% 
  filter(results == 2) %>% 
  mutate(match = "ambiguous")

#juntarlos
lafamilia_analysed <- lafamilia_analysed_onematch %>% 
  add_row(lafamilia_analysed_twomatches) %>% 
  rename(id_busqueda = id_lafamilia)

#exportarla con las columnas relevantes (para la comprobación manual)
lafamilia_analysed %>% 
  select(idtweets, id, text, fecha, place_name, place, id_busqueda, contexto_reducido, 
         results, match) %>% 
  write_delim("lafamilia_analysed_manual.csv", delim = "\t")


