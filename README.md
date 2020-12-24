# Concordancia_ad_sensum_2020
 
En este repositorio se encuentran los datos anotados y los scripts empleados para analizarlos empleados en el artículo: De Benito Moreno, Carlota (2021): "xxx".

## Carpeta Twitter

El script `Conc_ad_sensum_regex.R` contiene las regex empleadas para extraer las concordancias de la base de datos de tuits (no es pública).
Los scripts `alguien_etiquetado.R`, `elmundo_etiquetado.R`, `lafamilia_etiquetado.R`, `lagente_etiquetado.R` y `nadie_etiquetado.R` sirven para etiquetar automáticamente los ejemplos recuperados en los archivos `alguien_TW.csv`, `elmundo_TW.csv`, `lafamilia_TW.csv`, `lagente_TW.csv` y `nadie_TW.csv`. Por razones de privacidad, se ha eliminado la columna `text` de dichos archivos , lo que impide la reproducibilidad total de los resultados, que solo podrían replicarse (es decir, con un corpus no idéntico). Para hacer el código operativo se han añadido los archivos `alguien_analysed_manual.csv`, `elmundo_analysed_manual.csv`, `lafamilia_analysed_manual.csv`, `lagente_analysed_manual.csv` y `nadie_analysed_manual.csv`, que contienen los datos revisados manualmente. Aunque la columna `text` se ha eliminado por los mismos motivos de privacidad, se ha mantenido la columna `contexto_reducido`, que permite observar parcialmente el ejemplo. Esto permite generar tanto las tablas como las figuras del artículo.

El script `Conc_ad_sensum_ocurrencias.R` calcula los datos de las ocurrencias representados en las tablas 1 y 2.
El script `Conc_ad_sensum_map_plot_TW.R` genera el mapa 1 y la figura 1. Emplea las coordenadas guardadas en el archivo `places_geo_TW.csv` y los datos de tamaño del corpus del archivo `TW_COUNTporpais.csv


## Carpeta COSER
Los archivos `.txt` corresponden a las búsquedas realizadas en el COSER (extraídas del código fuente). Se preocesan con el script `COSER_preprocess_personas.R`, que se basa en funciones diseñadas en el script `COSER_preprocess.R`.

El archivo `conc_ad_sensum_clean.csv` contiene los ejemplos anotados manualmente. El análisis de los dichos ejemplos se realiza con el script `COSER_conc_ad_sensum_analisis.R` (incluye el código para las figuras 2 y 3). El mapa 2 se realiza con el `script COSER_conc_ad_sensum_mapa_poligonos.R`. Las coordenadas de cada punto se encuentran en `COSER_coordenadas_con_ad_sensum.csv`.
