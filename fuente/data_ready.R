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



## Adquisición de datos
# Importación de archivo INE
df_00 <- read_csv("raw_data/20220411_1845_COMPUTOS_RM2022.csv", 
                  col_names = FALSE,
                  skip = 5)


# Visualización inicial
#glimpse(df_00)


## Limpieza de datos
# Vector de nombres de columnas
#nombres_columnas <- df_00 %>% filter(row_number() == 1)

# Cambio de tipo de variable
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

# Visualización
#glimpse(df_01)


# Recodificación de Entidades Federativas
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

# Verificación 
#lapply(df_01[, c(2, 4)], levels)


## Adquisición de base de datos de nomenclatura de estados
estados <- read_csv("raw_data/code_ENTIDAD.csv")

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

# Verificación de NAs
#df_03 %>% 
#  map(~sum(is.na(.)))







