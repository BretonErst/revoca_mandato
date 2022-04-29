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


## Relación con el IRS
# Importación de datos de IRS
irs <- read_csv("raw_data/irs.csv")

# limpieza de datos IRS
irs <- irs %>% 
  select(Clave, irs)

# Integración de datos
rela <- df_03 %>% 
  left_join(irs, by = c("id_entidad" = "Clave")) %>% 
  group_by(id_entidad, estado) %>% 
  summarize(total_revoca = sum(voto_revoque),
            total_siga = sum(voto_siga),
            pct_revoca = total_revoca / sum(total_voto),
            pct_siga = total_siga / sum(total_voto),
            pct_participa = sum(total_voto) / sum(lista_nominal),
            irs = mean(irs)) %>% 
  ungroup() 

# Plot de Relación entre IRS y resultado de la consulta
rela %>% 
  ggplot(aes(x = irs, y = pct_siga, size = pct_participa)) +
  geom_point(color = "steelblue",
             alpha = 0.45) +
  geom_text_repel(aes(label = estado), 
                  segment.alpha = 0.3,
                  hjust = 0.85,
                  vjust = 0.95,
                  size = 2.5) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        panel.background = element_rect(fill = "#F7f7f7"),
        plot.title = element_text(size = 18),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "darkgrey",
                                        hjust = 0),
        legend.position = "none") +
  labs(title = "Relación entre el Rezago Social y el Resultado de la Consulta",
       subtitle = "El tamaño del punto indica la participación ciudadana en el ejercicio",
       x = "Índice de Rezago Social",
       y = "Proporción por la Continuación del Mandato",
       caption = "Fuente: Datos del INE 'computosrm2022' y CONEVAL (2020) <br>
       Visualización: Juan L. Bretón, PMP | @BretonPmp") +
  xlim(-1.5, 3) #-> rm_03

# Guardar el plot
ggsave(filename = "imagenes/rev_03", plot = rm_03, device = "tiff")


