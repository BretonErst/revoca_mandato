###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


# Librerias
library(tidyverse)
library(mxmaps)
library(ggtext)
library(ggrepel)


## Adquisición de datos
df_00 <- read_csv("20220411_1845_COMPUTOS_RM2022.csv", 
                  col_names = FALSE,
                  skip = 5)

glimpse(df_00)

## Limpieza de datos
nombres_columnas <- df_00 %>% filter(row_number() == 1)

df_01 <- df_00 %>% 
  filter(!row_number() == 1) %>% 
  filter(!X4 == "VOTO EN EL EXTRANJERO") %>% 
  mutate(across(X13:X17, as.numeric)) %>% 
  mutate(across(X1:X11, as.factor)) %>% 
  select("clave_casilla" = X1,
         "id_entidad" = X3,
         "entidad" = X4,
         "id_distrito" = X5,
         "distrito" = X6,
         "seccion" = X7,
         "id_casilla" = X8,
         "tipo_casilla" = X9,
         "ubica_casilla" = X11,
         "voto_revoque" = X13,
         "voto_siga" = X14,
         "voto_nulos" = X15,
         "total_voto" = X16,
         "lista_nominal" = X17) 
  
glimpse(df_01)


df_01 <- df_01 %>% 
  mutate(id_entidad = recode(id_entidad,
                             "1" = "01",
                             "2" = "02",
                             "3" = "03",
                             "4" = "04",
                             "5" = "05",
                             "6" = "06",
                             "7" = "07",
                             "8" = "08",
                             "9" = "09",
                             "10" = "10",
                             "11" = "11",
                             "12" = "12",
                             "13" = "13",
                             "14" = "14",
                             "15" = "15",
                             "16" = "16",
                             "17" = "17",
                             "18" = "18",
                             "19" = "19",
                             "20" = "20",
                             "21" = "21",
                             "22" = "22",
                             "23" = "23",
                             "24" = "24",
                             "25" = "25",
                             "26" = "26",
                             "27" = "27",
                             "28" = "28",
                             "29" = "29",
                             "30" = "30",
                             "31" = "31",
                             "32" = "32",))

lapply(df_01[, c(2, 4)], levels)


# Adquisición de base de datos de nomenclatura de estados
estados <- read_csv("code_ENTIDAD.csv")

# Preparación de data frame para integración
estados <- estados %>% 
  mutate(ENTIDAD_RES = as.factor(CLAVE_ENTIDAD),
         ENTIDAD_FEDERATIVA = as.factor(ENTIDAD_FEDERATIVA)) %>% 
  select(-CLAVE_ENTIDAD,
         -ABREVIATURA,
         ENTIDAD_RES,
         estado = ENTIDAD_FEDERATIVA)

# Integración de dataframes
df_02 <- df_01 %>% 
  left_join(estados, by = c("id_entidad" = "ENTIDAD_RES")) %>%
  relocate(estado, .after = entidad)

# Reemplazo de NAs por 0
df_03 <- df_02 %>% 
  mutate(voto_revoque = replace_na(voto_revoque, 0),
         voto_siga = replace_na(voto_siga, 0),
         voto_nulos = replace_na(voto_nulos, 0),
         total_voto = replace_na(total_voto, 0),
         lista_nominal = replace_na(lista_nominal, 0))

df_03 %>% 
  map(~sum(is.na(.)))

# Total lista nominal
df_03 %>% 
  summarize(total_lista_nominal = sum(lista_nominal))

df_03 %>% 
  summarize(pct_participa = sum(total_voto) / sum(lista_nominal))

df_03 %>% 
  summarize(total_revoca = sum(voto_revoque),
            total_siga = sum(voto_siga),
            pct_revoca = total_revoca / sum(total_voto),
            pct_siga = total_siga / sum(total_voto)) %>% 
  knitr::kable(digits = 2)



## Mapa de participación por estado
# cálculo
participa_por_estado <- df_03 %>% 
  group_by(id_entidad, estado) %>% 
  summarize(pct_participa = sum(total_voto) / sum(lista_nominal)) %>% 
  arrange(desc(pct_participa)) %>% 
  ungroup()

# base de datos
mapa_pct_participa <- participa_por_estado %>% 
  select(region = id_entidad,
         value = pct_participa)

participa_por_estado %>% 
  slice_max(order_by = pct_participa, n = 8, with_ties = FALSE) %>% 
  knitr::kable(digits = 3, 
               format.args = list(big.mark = ","))

participa_por_estado %>% 
  slice_min(order_by = pct_participa, n = 8, with_ties = FALSE) %>% 
  knitr::kable(digits = 3, 
               format.args = list(big.mark = ","))

# mapa
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

ggsave(filename = "rev_01", plot = rm_01, device = "tiff")


## Mapa de Que Siga por estado
# cálculo
pct_por_estado <- df_03 %>% 
  group_by(id_entidad, estado) %>% 
  summarize(total_revoca = sum(voto_revoque),
            total_siga = sum(voto_siga),
            pct_revoca = total_revoca / sum(total_voto),
            pct_siga = total_siga / sum(total_voto)) %>% 
  ungroup()

pct_por_estado %>% 
  slice_max(order_by = pct_siga, n = 8, with_ties = FALSE) %>% 
  knitr::kable(digits = 3, 
               format.args = list(big.mark = ","))

pct_por_estado %>% 
  slice_min(order_by = pct_siga, n = 8, with_ties = FALSE) %>% 
  knitr::kable(digits = 3, 
               format.args = list(big.mark = ","))

# base de datos
mapa_pct_quesiga <- pct_por_estado %>% 
  select(region = id_entidad,
         value = pct_siga)

# mapa
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

ggsave(filename = "rev_02", plot = rm_02, device = "tiff")
  

code_gto <- df_mxmunicipio_2020 %>% 
  filter(state_name == "Guanajuato") %>% 
  select(region, municipio_code, municipio_name) %>% 
  mutate(municipio = str_to_upper(stringi::stri_trans_general(municipio_name,
                                                              "Latin-ASCII")))


# Mapa de municipios
df_03 %>% 
  filter(estado == "GUANAJUATO") %>% 
  left_join(code_gto, by = c("distrito" = "municipio")) %>% 
  group_by(region, distrito) %>% 
  summarize(total_revoca = sum(voto_revoque),
            total_siga = sum(voto_siga),
            pct_revoca = total_revoca / sum(total_voto),
            pct_siga = total_siga / sum(total_voto)) 
  
  
# IRS
irs <- read_csv("irs.csv")

irs <- irs %>% 
  select(Clave, irs)

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
 
 ggsave(filename = "rev_03", plot = rm_03, device = "tiff")













