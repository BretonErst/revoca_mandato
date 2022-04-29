###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerias
library(tidyverse)
library(mxmaps)
library(ggtext)
library(ggrepel)
library(janitor)
library(knitr)


## Source
suppressWarnings(source("fuente/data_ready.R")) 



## Mapa de participación por estado
# cálculo la participación por estado
participa_por_estado <- df_03 %>% 
  group_by(id_entidad, estado) %>% 
  summarize(pct_participa = sum(total_voto) / sum(lista_nominal)) %>% 
  arrange(desc(pct_participa)) %>% 
  ungroup()

# base de datos para el mapa
mapa_pct_participa <- participa_por_estado %>% 
  select(region = id_entidad,
         value = pct_participa)

# Tabla de MAYOR participación por estado
participa_por_estado %>% 
  slice_max(order_by = pct_participa, n = 8, with_ties = FALSE) %>% 
  kable(digits = 3, 
               format.args = list(big.mark = ","))

# Tabla de MENOR participación por estado
participa_por_estado %>% 
  slice_min(order_by = pct_participa, n = 8, with_ties = FALSE) %>% 
  kable(digits = 3, 
               format.args = list(big.mark = ","))

# Mapa de participación por estado
mxstate_choropleth(mapa_pct_participa,
                   num_colors = 1) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title = element_text(size = 16),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "darkgrey",
                                        hjust = 0),
        legend.position = "right") +
  labs(title = "Proporción de Participación por Estado",
       subtitle = "Los números y el gradiente de color indican la proporción\nde personas en la lista nominal que acudieron a votar",
       caption = "Fuente: Datos del INE 'computosrm2022' <br>
       Visualización: Juan L. Bretón, PMP | @BretonPmp") #-> rm_01

# Guardar el mapa
ggsave(filename = "imagenes/rev_01", plot = rm_01, device = "tiff")



## Mapa de Que Siga por estado
# cálculo de porcentaje que votó Que Siga
pct_por_estado <- df_03 %>% 
  group_by(id_entidad, estado) %>% 
  summarize(total_revoca = sum(voto_revoque),
            total_siga = sum(voto_siga),
            pct_revoca = total_revoca / sum(total_voto),
            pct_siga = total_siga / sum(total_voto)) %>% 
  ungroup()

# Tabla de MAYOR porcentaje por estado
pct_por_estado %>% 
  slice_max(order_by = pct_siga, n = 8, with_ties = FALSE) %>% 
  kable(digits = 3, 
        format.args = list(big.mark = ","))

# Tabla de MENOR porcentaje por estado
pct_por_estado %>% 
  slice_min(order_by = pct_siga, n = 8, with_ties = FALSE) %>% 
  kable(digits = 3, 
        format.args = list(big.mark = ","))

# base de datos
mapa_pct_quesiga <- pct_por_estado %>% 
  select(region = id_entidad,
         value = pct_siga)

# mapa de que siga por estado
mxstate_choropleth(mapa_pct_quesiga,
                   num_colors = 1) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title = element_text(size = 18),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "darkgrey",
                                        hjust = 0),
        legend.position = "right") +
  labs(title = "Voto por la Continuidad del Mandato",
       subtitle = "Los números y el gradiente de color muestran\nla proporción de electores que eligieron 'Que siga[..]'",
       caption = "Fuente: Datos del INE 'computosrm2022' <br>
       Visualización: Juan L. Bretón, PMP | @BretonPmp") #-> rm_02

# Guarda mapa
ggsave(filename = "imagenes/rev_02", plot = rm_02, device = "tiff")
