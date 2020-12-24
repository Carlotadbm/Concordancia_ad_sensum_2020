#29 de octubre de 2020
#Preprosecesamiento COSER: funciones

library(tidyverse)


##Las funciones se aplican sobre archivos .txt extraídos del código fuente del COSER
# Habiendo copiado la línea "Resultado de la búsqueda" entera, sin párrafos antes ni después
# guardado como .txt, codificado como UTF-8


#limpiar los txt
read_lines_COSER <- function(file) { #it takes a file path
  
  #read lines and make replacements
  coser <- read_lines(file) %>% 
    str_replace_all("</p><br><b>", "\n") %>% 
    str_replace_all("<br><hr color='ligth blue' width=80% size=1><b>", "\n") %>% 
    str_replace_all("<p class", "\n<p class") %>% 
    str_replace_all("<h3>Resultado de la búsqueda.+resultados encontrados</p><br>", "") %>% 
    str_replace_all("<br><b>", " ") %>% 
    str_replace_all("</b>", " ") %>% 
    str_replace_all("(.)(?:</mark>)+", "\\*\\*\\*\\1") %>% #con (?:) hago un paréntesis que permite la repetición pero no captura grupo (no cuenta en las backreferences)
    str_replace_all("(?:<mark>)+(.)", "\\1\\*\\*\\*") %>% 
    str_replace_all(" <br><hr color='ligth blue' width=80% size=1><br>", "")
  
  #create file name
  name <- str_replace(file, "\\.txt", "")
  
  #write new .txt file
  write_lines(coser, str_c(name, "_preprocessed", ".txt"))
}

#leer tabulado
read_delim_COSER <- function(file) { #it takes a file path
  
  #read as delim
  coser_df <-read_delim(file, delim = "\n", col_names = "text")
  
  #remove two first lines and create column for paragraph number
  coser_df2 <- coser_df %>% 
    slice(-(1:2)) %>% 
    mutate(paragraph = seq_along(text)) 
  
  #calculate rows per provincia using lead
  provincia_lead <- coser_df2 %>% 
    filter(str_detect(text, "<p class=\\'provincia\\'>.+\\(")) %>% #filter by that first element to get the paragraph number
    mutate(provincia = str_extract_all(text, "<p class=\\'provincia\\'>.+\\("),
           provincia = str_replace(provincia, "<p class=\\'provincia\\'>", ""),
           provincia = str_replace(provincia, " \\(", "")) %>% 
    mutate(diff = lead(paragraph, default = (nrow(coser_df2) + 1)) - paragraph) #total number of rows + 1
  
  #calculate rows per enclave using lead
  enclave_lead <- coser_df2 %>% 
    filter(str_detect(text, "Enclave: <a.+\\(")) %>% #filter by that first element to get the paragraph number
    mutate(enclave = str_extract_all(text, "Enclave: <a.+\\("),
           enclave = str_replace(enclave, "Enclave.+>([A-ZÁÉÍÓÚÑ])", "\\1"),
           enclave = str_replace(enclave, "  \\(COSER.+", "")) %>% 
    mutate(diff = lead(paragraph, default = (nrow(coser_df2) + 1)) - paragraph) #total number of rows + 1
  
  #calculate rows per COSERID using lead
  COSERID_lead <- coser_df2 %>% 
    filter(str_detect(text, "COSER-.+\\)")) %>% #filter by that first element to get the paragraph number
    mutate(COSERID = str_extract_all(text, "COSER-.+\\)"),
           COSERID = str_replace(COSERID, "\\).+", ""),
           COSERID = str_replace(COSERID, "_\\d+", ""),
           COSERID = str_replace(COSERID, "COSER-", "")) %>% 
    mutate(diff = lead(paragraph, default = (nrow(coser_df2) + 1)) - paragraph) #total number of rows + 1
  
  #create 3 relevant columns with repetitions
  provincia <- rep(provincia_lead$provincia, provincia_lead$diff) #create vector with the correct repetitions
  enclave <- rep(enclave_lead$enclave, enclave_lead$diff) #create vector with the correct repetitions
  COSERID <- rep(COSERID_lead$COSERID, COSERID_lead$diff) #create vector with the correct repetitions
  
  #join columns and remove rows with the metadata that now is in the columns
    coser_df3 <- coser_df2 %>% 
      add_column(provincia) %>% 
      slice(-1) %>% 
      add_column(enclave, COSERID) %>% 
      filter(!str_detect(text, "^<")) %>% 
      select(-paragraph)
  
}





