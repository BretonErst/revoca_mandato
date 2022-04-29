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


## Exploratorio
# Total lista nominal
df_03 %>% 
  summarize(total_lista_nominal = sum(lista_nominal)) %>% 
  kable(format.args = list(big.mark = ","))

# Porcentaje de participación
df_03 %>% 
  summarize(pct_participa = sum(total_voto) / sum(lista_nominal)) %>% 
  kable(digits = 4)

# Resultados Totales
df_03 %>% 
  summarize(total_revoca = sum(voto_revoque),
            total_siga = sum(voto_siga),
            pct_revoca = total_revoca / sum(total_voto),
            pct_siga = total_siga / sum(total_voto)) %>% 
  kable(digits = 2,
        format.args = list(big.mark = ","))






