#Crear la tabla etiquetada de la gente


library(udpipe)
library(tidyverse)
library(magrittr)
'%ni%' <- Negate('%in%')
#udpipe_download_model(language = "spanish-ancora")
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.5-191206.udpipe')

nadie <- read_delim("nadie_TW.csv", delim = "\t")


nadie_unnested <- nadie %>% 
  mutate(contexto_reducido = 
           str_extract(text, 
                       "(:?\\b[a-záéíóúüñ]+\\b ){0,3}nadie\\b(:? \\b[a-záéíóúüñ]+\\b){0,4}")) %>% #extraer el contexto reducido 
  mutate(word1 = str_replace(contexto_reducido, #extraer la primera palabra
                             "^(\\b[a-záéíóúüñ]+\\b)?( \\b[a-záéíóúüñ]+\\b){0,2} nadie\\b.*$", 
                             "\\1"),
         word1 = ifelse(word1 == contexto_reducido, NA, word1)) %>% 
  mutate(word2 = str_replace(contexto_reducido, #extraer la segunda palabra
                             "^(\\b[a-záéíóúüñ]+\\b )?(\\b[a-záéíóúüñ]+\\b)?( \\b[a-záéíóúüñ]+\\b)? nadie\\b.*$", 
                             "\\2"),
         word2 = ifelse(word2 == contexto_reducido, NA, word2)) %>%
  mutate(word3 = str_replace(contexto_reducido, #extraer la tercera palabra
                             "^(\\b[a-záéíóúüñ]+\\b ){0,2}(\\b[a-záéíóúüñ]+\\b)? nadie\\b.*$", 
                             "\\2"),
         word3 = ifelse(word3 == contexto_reducido, NA, word3)) %>%
  mutate(word2 = ifelse((word2 == word3) & (word1 != word2), word1, word2), #arreglar lo que pasa en las primeras columnas: si no hay 3 palabras delante se producen diversas repeticiones
         word1 = ifelse((word1 == word2) & (word2 != word3), NA, word1), 
         word1 = ifelse((word1 == word2) & (word1 == word2), NA, word1), 
         word2 = ifelse(is.na(word1) & (word2 == word3), NA, word2)) %>% 
  mutate(word4 = str_replace(contexto_reducido, #extraer el sujeto seguido de 0 a 3 clíticos más la negación 
                             "^(\\b[a-záéíóúüñ]+\\b ){0,3}(nadie ?((no|l[eao]s?|[mst]e|n?os) ){0,3})\\b.*$", 
                             "\\2"),
         word4 = ifelse(word4 == contexto_reducido, NA, word4)) %>%
  mutate(word5 = str_replace(contexto_reducido, #extraer la palabra tras el sujeto
                             "^(\\b[a-záéíóúüñ]+\\b ){0,2}\\bnadie ((no|l[eao]s?|[mst]e|n?os) ){0,3}(\\b[a-záéíóúüñ]+\\b)?.*$", 
                             "\\4"),
         word5 = ifelse(word5 == contexto_reducido, NA, word5)) 

##La función para etiquetar cada una de las columnas
annotate_TW_nadie <- function(column, name) {
  #column <- column %>% 
  #  subset(!is.na(column))
  udpipe_annotate(modelo_ancora, column) %>% 
    as_tibble() %>% 
    select(doc_id, lemma, token_id, upos, xpos, feats) %>% #lemma
    mutate(id_nadie = str_remove(doc_id, "^doc"),
           id_nadie = as.integer(id_nadie)) %>%
    filter(!token_id == "1-2") %>% 
    set_colnames(str_c(colnames(.), c(rep(name, 6), ""), sep = "")) %>%
    select(-starts_with("doc_id"))
  
}


word_1_analisis <- annotate_TW_nadie(nadie_unnested$word1, "_word1")
word_2_analisis <- annotate_TW_nadie(nadie_unnested$word2, "_word2")
word_3_analisis <- annotate_TW_nadie(nadie_unnested$word3, "_word3")
word_5_analisis <- annotate_TW_nadie(nadie_unnested$word5, "_word5")

nadie_annotated <- nadie_unnested %>% 
  full_join(word_1_analisis) %>% 
  full_join(word_2_analisis) %>% 
  full_join(word_3_analisis) %>% 
  full_join(word_5_analisis)

nadie_analysed_preparation <- nadie_annotated %>% 
  mutate(nadie_3sg_ant = three_sg_ant_cont(.),
         nadie_3sg_perif = three_sg_ant_perif(.),
         nadie_3sg_perif_prep = three_sg_ant_perif_prep(.),
         nadie_3sg_pos = three_sg_posp(.),
         
         nadie_3pl_ant = three_pl_ant_cont(.),
         nadie_3pl_perif = three_pl_ant_perif(.),
         nadie_3pl_perif_prep = three_pl_ant_perif_prep(.),
         nadie_3pl_pos = three_pl_posp(.),
         
         nadie_1pl_ant = first_pl_ant_cont(.),
         nadie_1pl_perif = first_pl_ant_perif(.),
         nadie_1pl_perif_prep = first_pl_ant_perif_prep(.),
         nadie_1pl_pos = first_pl_posp(.),
         
         nadie_2pl_ant = second_pl_ant_cont(.),
         nadie_2pl_perif = second_pl_ant_perif(.),
         nadie_2pl_perif_prep = second_pl_ant_perif_prep(.),
         nadie_2pl_pos = second_pl_posp(.),
  ) %>% 
  mutate(results = nadie_3sg_ant + nadie_3sg_perif+ nadie_3sg_perif_prep + nadie_3sg_pos +
           nadie_3pl_ant + nadie_3pl_perif + nadie_3pl_perif_prep + nadie_3pl_pos + 
           nadie_1pl_ant + nadie_1pl_perif + nadie_1pl_perif_prep + nadie_1pl_pos +
           nadie_2pl_ant + nadie_2pl_perif + nadie_2pl_perif_prep + nadie_2pl_pos)


#Hay 357 filas con 2 resultados, lo que impidiría un separate, podríamos separarlos

nadie_analysed_preparation %>% 
  count(results, sort = T)

#Para el separate hay que cambiar los valores de TRUE por el nombre de la columna

#One match
nadie_analysed_preparation %>% 
  filter(results != 2) %>% 
  mutate(
    match = ifelse(nadie_3sg_ant, "nadie_3sg_ant",
                   ifelse(nadie_3sg_perif, "nadie_3sg_perif",
                          ifelse(nadie_3sg_perif_prep, "nadie_3sg_perif_prep",
                                 ifelse(nadie_3sg_pos, "nadie_3sg_pos",
                                        ifelse(nadie_3pl_ant, "nadie_3pl_ant",
                                               ifelse(nadie_3pl_perif, "nadie_3pl_perif",
                                                      ifelse(nadie_3pl_perif_prep, "nadie_3pl_perif_prep",
                                                             ifelse(nadie_3pl_pos, "nadie_3pl_pos",
                                                                    ifelse(nadie_1pl_ant, "nadie_1pl_ant",
                                                                           ifelse(nadie_1pl_perif, "nadie_1pl_perif",
                                                                                  ifelse(nadie_1pl_perif_prep, "nadie_1pl_perif_prep",
                                                                                         ifelse(nadie_1pl_pos, "nadie_1pl_pos",
                                                                                                ifelse(nadie_2pl_ant, "nadie_2pl_ant",
                                                                                                       ifelse(nadie_2pl_perif, "nadie_2pl_perif",
                                                                                                              ifelse(nadie_2pl_perif_prep, "nadie_2pl_perif_prep",
                                                                                                                     ifelse(nadie_2pl_pos, "nadie_2pl_pos", "FALSE"
                                                                                                                     ))))))))))))))))) %>% 
  count(match) #¿Cuáles son los más frecuentes (no es posible analizarlos manualmente)?

#seleccionar muestras aleatorias de los más frecuentes
set.seed(739)
sample_ant_3sg <- nadie_analysed_preparation %>% 
  filter(nadie_3sg_ant == TRUE) %>% 
  slice_sample(n = 300) %>% 
  select(id_nadie)

set.seed(973)
sample_pos_3sg <- nadie_analysed_preparation %>% 
  filter(nadie_3sg_pos == TRUE) %>% 
  slice_sample(n = 300) %>% 
  select(id_nadie)



#guardar
nadie_analysed_onematch <- nadie_analysed_preparation %>% 
  filter(results != 2) %>% 
  mutate(
    match = ifelse(nadie_3sg_ant, "nadie_3sg_ant",
                   ifelse(nadie_3sg_perif, "nadie_3sg_perif",
                          ifelse(nadie_3sg_perif_prep, "nadie_3sg_perif_prep",
                                 ifelse(nadie_3sg_pos, "nadie_3sg_pos",
                                        ifelse(nadie_3pl_ant, "nadie_3pl_ant",
                                               ifelse(nadie_3pl_perif, "nadie_3pl_perif",
                                                      ifelse(nadie_3pl_perif_prep, "nadie_3pl_perif_prep",
                                                             ifelse(nadie_3pl_pos, "nadie_3pl_pos",
                                                                    ifelse(nadie_1pl_ant, "nadie_1pl_ant",
                                                                           ifelse(nadie_1pl_perif, "nadie_1pl_perif",
                                                                                  ifelse(nadie_1pl_perif_prep, "nadie_1pl_perif_prep",
                                                                                         ifelse(nadie_1pl_pos, "nadie_1pl_pos",
                                                                                                ifelse(nadie_2pl_ant, "nadie_2pl_ant",
                                                                                                       ifelse(nadie_2pl_perif, "nadie_2pl_perif",
                                                                                                              ifelse(nadie_2pl_perif_prep, "nadie_2pl_perif_prep",
                                                                                                                     ifelse(nadie_2pl_pos, "nadie_2pl_pos", "FALSE"
                                                                                                                     ))))))))))))))))) 


nadie_analysed_twomatches <- nadie_analysed_preparation %>% 
  filter(results == 2) %>% 
  mutate(match = "ambiguous")

#juntarlos
nadie_analysed <- nadie_analysed_onematch %>% 
  add_row(nadie_analysed_twomatches) %>% 
  mutate(
    random_sample = ifelse(id_nadie %in% sample_ant_3sg, TRUE, 
                           ifelse(id_nadie %in% sample_pos_3sg, TRUE, FALSE))
  ) %>% 
  rename(id_busqueda = id_nadie)

#exportarla con las columnas relevantes (para la comprobación manual)
nadie_analysed %>% 
  select(idtweets, id, text, fecha, place_name, place, id_busqueda, contexto_reducido, 
         results, match, random_sample) %>% 
  write_delim("nadie_analysed_manual.csv", delim = "\t")


