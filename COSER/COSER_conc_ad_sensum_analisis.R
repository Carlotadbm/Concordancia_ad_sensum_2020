#Análisis datos COSER
# 17 de noviembre de 2020

'%ni%' <- Negate('%in%')
library(tidyverse)
library(extrafont)
library(grDevices)
library(ggtext)
library(ggExtra)
library(wesanderson)
library(ggrepel)
library(lme4)
library(broom.mixed)

coser <- read_delim("conc_ad_sensum_clean.csv", delim = ";")

#Casos totales analizados
#sujetos
coser %>%
  filter(busqueda %ni% c("nadie", "ninguno", "ninguna", "nadien", "naide")) %>% 
  count(sujeto) %>% 
  mutate(total = sum(n))

coser_validos <- coser %>%
  filter(busqueda %ni% c("nadie", "ninguno", "ninguna", "nadien", "naide")) %>% 
  mutate(busqueda = ifelse(busqueda %in% c("la_gente_1", "a_gente", "gente", "la_gente_2"), "gente",
                         ifelse(busqueda %in% c("todo_el_mundo", "to_el_mundo", "tol_mundo", "to_l_el_mundo", "to_l_mundo", "todo_l_mundo"),  "todo el mundo", 
                                ifelse(busqueda %in% c("a_familia", "la_familia"), "familia", 
                                       ifelse(busqueda %in% c("alguna"), "alguno", busqueda))))) %>% 
  filter(sujeto == "si") 
coser_validos %>% 
  count(busqueda) %>% 
  mutate(percentage=round(n/sum(n)*100, 1))
  

#verbos
n_v1 <- coser_validos %>%  
  filter(V1_persona %in% c("3sg", "3pl", "1pl", "2pl", "2sg")) %>% 
  nrow()  
n_v2 <- coser_validos %>%
    filter(V2_persona %in% c("3sg", "3pl", "1pl")) %>% 
    nrow() 
n_vs <- coser_validos %>%
  filter(Vs_persona %in% c("3sg", "3pl", "1pl")) %>% 
  nrow()
tibble(n_v1, n_v2, n_vs) %>% 
  gather(n_v1, n_v2, n_vs, key = "type", value = "n") %>% 
  mutate(percentage=round(n/sum(n)*100, 1), 
         total = sum(n))
  
#Proporción de concordancia en todos los contextos
v1 <- coser_validos %>%  
  filter(V1_persona %in% c("3sg", "3pl", "1pl", "2pl", "2sg")) %>% 
  select(id_busqueda, id_result, busqueda, V1_persona) %>% 
  rename(persona = V1_persona)
v2 <- coser_validos %>%
  filter(V2_persona %in% c("3sg", "3pl", "1pl"))%>% 
  select(id_busqueda, id_result, busqueda, V2_persona) %>% 
  rename(persona = V2_persona)
vs <- coser_validos %>%
  filter(Vs_persona %in% c("3sg", "3pl", "1pl")) %>% 
  select(id_busqueda, id_result, busqueda, Vs_persona) %>% 
  rename(persona = Vs_persona)

v1 %>% 
  add_row(v2) %>% 
  add_row(vs) %>% 
  count(persona) %>% 
  mutate(percentage = round(n/sum(n)*100, 1))

v1 %>% 
  add_row(v2) %>% 
  add_row(vs) %>%
  filter(persona == "2sg")
  
#Plots
# Figura 2
## todos los sujetos, en 3 contextos
#adyacencia
coser_adyacencia <- coser_validos %>% 
  filter(V1_persona %ni% c("ambiguo", "2sg")) %>% 
  filter(!is.na(V1_persona)) %>% 
  filter(V1_posicion != "ambiguo") %>% 
  filter(V1_adyacencia == "si") %>% 
  mutate(tipo = ifelse(V1_posicion == "antes", "V1 antepuesto", "V1 pospuesto")) %>% 
  rename(persona = V1_persona) %>% 
  group_by(busqueda, tipo) %>% 
  count(persona) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  ungroup()


## Vs

coser_vs <- coser_validos %>% 
  filter(!is.na(Vs_persona)) %>% 
  group_by(busqueda) %>% 
  count(Vs_persona) %>% 
  mutate(prop = n/sum(n)*100, 
         tipo = "V dependiente") %>% 
  rename(persona = Vs_persona) %>% 
  ungroup()

coser_todos <- coser_adyacencia %>% 
  add_row(coser_vs)

order_person <- c("3sg", "3pl", "2pl", "1pl")
coser_todos$persona <- factor(coser_todos$persona, levels = order_person)

coser_todos %>% 
  ggplot(aes(x = tipo, y = prop, group = persona))  + 
  geom_col(aes(fill=persona), position = "fill") +
  labs(title="Concordancia verbal por lexema del sujeto", x="Verbo", 
       y="Frecuencia de la concordancia", fill="Concordancia") +
  geom_text_repel(aes(label = n), position = position_fill(vjust = .5), segment.alpha = .2, direction = "y") + 
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  scale_fill_manual(values=wes_palette(n=4, name="Moonrise3")) + 
  coord_flip() +
  facet_wrap(~ busqueda, scales = "free_x")

ggsave("coser_todos_sujetos.png", width = 12, height = 8)

# Figura 3
#gente en todos los contextos
la_gente <- coser_validos %>% 
  filter(busqueda == "gente")

la_gente_V1 <- la_gente %>% 
  filter(V1_persona %ni% c("2sg", "2pl", "ambiguo")) %>% 
  mutate(tipo = ifelse(V1_posicion == "antes" & V1_adyacencia == "si", "V1_antepuesto_adyacente", 
                       ifelse(V1_posicion == "antes" & V1_adyacencia == "no", "V1_antepuesto_distante",
                              ifelse(V1_posicion == "despues" & V1_adyacencia == "si", "V1_pospuesto_adyacente", 
                                     ifelse(V1_posicion == "despues" & V1_adyacencia == "no", "V1_pospuesto_distante", NA))))) %>% 
  filter(!is.na(tipo)) %>% 
  rename(persona = V1_persona) %>% 
  group_by(tipo) %>% 
  count(persona) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  ungroup()

la_gente_V2 <- la_gente %>% 
  filter(!is.na(V2_persona)) %>% 
  mutate(tipo = "extraclausal") %>% 
  rename(persona = V2_persona) %>% 
  group_by(tipo) %>% 
  count(persona) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  ungroup()

la_gente_Vs <- la_gente %>% 
  filter(!is.na(Vs_persona))  %>% 
  mutate(tipo = "dependiente") %>% 
  rename(persona = Vs_persona) %>% 
  group_by(tipo) %>% 
  count(persona) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  ungroup()


la_gente_plot <- la_gente_V1 %>% 
  add_row(la_gente_V2) %>% 
  add_row(la_gente_Vs)
  

order_person2 <- c("3sg", "3pl", "1pl")
la_gente_plot$persona <- factor(la_gente_plot$persona, levels = order_person2)

la_gente_plot %>% 
  ggplot(aes(x = tipo, y = prop, group = persona))  + 
  geom_col(aes(fill=persona), position = "fill") +
  labs(title="Concordancia de *gente* según el contexto", x="Sujeto", 
       y="Frecuencia de la concordancia", fill="Concordancia") +
  geom_text_repel(aes(label = n), position = position_fill(vjust = .5), segment.alpha = .2, direction = "y") + 
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  scale_fill_manual(values=wes_palette(n=3, name="Moonrise3")) + 
  coord_flip() +
  theme(plot.title = element_markdown())
  
ggsave("coser_lagente_contextos.png", width = 12, height = 8)

#Modelo estadístico
la_gente_modelo <-  coser_validos %>% 
  filter(busqueda == "gente") %>% 
  filter(sujeto == "si") %>% 
  select(id_busqueda, id_result, COSERID, V1_persona, V1_posicion, V1_adyacencia, Vs_persona, V2_persona) %>% 
  rename(V1 = V1_persona, V2 = V2_persona, Vd = Vs_persona, posicion = V1_posicion, adyacencia = V1_adyacencia) %>% 
  gather(V1, V2, Vd, key = "oracion", value = "persona") %>% 
    filter(!is.na(persona)) %>% 
    filter(!persona %in% c("2sg", "ambiguo")) %>% 
    mutate(persona = ifelse(persona %in% c("3pl", "1pl", "2pl"), "pl", "sg"))

#convertimos en factor
#el primer nivel es el de referencia
la_gente_modelo$persona <- factor(la_gente_modelo$persona, levels = c("sg", "pl"))
la_gente_modelo$posicion <- factor(la_gente_modelo$posicion, levels = c("despues","antes"))
la_gente_modelo$adyacencia <- factor(la_gente_modelo$adyacencia, levels = c("si", "no"))
la_gente_modelo$oracion <- factor(la_gente_modelo$oracion, levels = c("V1", "Vd", "V2"))
la_gente_modelo$COSERID <- as.factor(la_gente_modelo$COSERID)

la_gente_glm <- glmer(persona ~ adyacencia + oracion + posicion*adyacencia + (1|COSERID), family = "binomial", data = la_gente_modelo)
summary(la_gente_glm)
range(resid(la_gente_glm)) #parece que estos residuals están otra escala que cuando usas summary()
hist(resid(la_gente_glm))#¿distribución normal? Difícil saber si hay dos distribuciones

la_gente_glm_tidy <- tidy(la_gente_glm, exponentiate = T, conf.int = T) #statistic es el z-value
la_gente_glm_tidy
write_delim(la_gente_glm_tidy, "la_gente_glm_tidy.csv", delim = "\t")


