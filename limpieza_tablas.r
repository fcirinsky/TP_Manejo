install.packages("countrycode")
library(tidyverse)
library(countrycode)

# LIMPIAMOS LA BASE DE DATOS DE LOS VOTOS DE AGNUS
# Cargamos la base
raw_voeten <- read_csv("Bases Comercio y voto ONU/Idealpointestimates1946-2025.csv")

# Nos quedamos con las variables que necesitamos
voto_clean <- raw_voeten %>%
  select(ccode, year, IdealPointFP)


voto_clean <- voto_clean %>%
  mutate(iso3c = countrycode(ccode, 
                             origin = "cown", 
                             destination = "iso3c"))

# creamos la lista con los países que elegimos
mis_paises <- c("AUS", "CAN", "HUN", "POL", "VNM", "CUB", "EGY", "DZA")

voto_final <- voto_clean %>%
  filter(iso3c %in% mis_paises) %>%
  filter(year >= 1970) # Filtramos desde el caso más antiguo (Egipto)

# Chequeamos que ninguno de los paises tenga NA´s
voto_final %>%
  group_by(iso3c) %>%
  summarize(datos_faltantes = sum(is.na(IdealPointFP)))


#LIMPIAMOS LAS BASES DE DATOS DEL COMERCIO

library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(haven)
library(tidyr)

voto_onu <- "/MVD/TP Manejo/Bases comercio y voto ONU/Idealpointestimates1946-2025.csv"

expo <- read_csv("Bases comercio y voto ONU/IMF_DOT_TXG_FOB_USD.csv")
impo <- read_csv("Bases comercio y voto ONU/IMF_DOT_TMG_CIF_USD.csv")


# Limpieza tabla
paises <- c("AUS", "CAN", "HUN", "POL", "VNM", "CUB", "EGY", "DZA")

expo_limpio <- expo %>%
  filter(REF_AREA %in% paises)

impo_limpio <- impo %>%
  filter(REF_AREA %in% paises)

# Seleccion de columnas necesarias
expo_limpio <- expo_limpio %>%
  select(pais = REF_AREA_LABEL,
         codigo = REF_AREA,
         socio = COMP_BREAKDOWN_1_LABEL,
         anio = TIME_PERIOD,
         value = OBS_VALUE,
         multiplicador = UNIT_MULT)

impo_limpio <- impo_limpio %>%
  select(pais = REF_AREA_LABEL,
         codigo = REF_AREA,
         socio = COMP_BREAKDOWN_1_LABEL,
         anio = TIME_PERIOD,
         value = OBS_VALUE,
         multiplicador = UNIT_MULT)

# Limpieza de socios
socios <- c("Counterpart: Russian Federation",
            "Counterpart: China, P.R.: Mainland",
            "Counterpart: United States",
            "Counterpart: World")

expo_limpio <- expo_limpio %>%
  filter(socio %in% socios) 

impo_limpio <- impo_limpio %>%
  filter(socio %in% socios)

expo_limpio <- expo_limpio %>% 
  mutate(socio = case_when(
    socio == "Counterpart: China, P.R.: Mainland" ~ "China",
    socio == "Counterpart: United States" ~ "Estados Unidos",
    socio == "Counterpart: Russian Federation" ~ "Rusia",
    socio == "Counterpart: World" ~ "World",
    TRUE ~ socio))

impo_limpio <- impo_limpio %>%
  mutate(socio = case_when(
    socio == "Counterpart: China, P.R.: Mainland" ~ "China",
    socio == "Counterpart: United States" ~ "Estados Unidos",
    socio == "Counterpart: Russian Federation" ~ "Rusia",
    socio == "Counterpart: World" ~ "World",
    TRUE ~ socio))

# Calculo del comercio bilateral
comercio <- expo_limpio %>%
  full_join(impo_limpio, by = c("codigo", "pais", "socio", "anio")) %>%
  mutate(comercio_bilateral = value.x + value.y)

comercio_total <- comercio %>% # world denominador
  filter(socio == "World") %>%
  select(codigo, anio, total = comercio_bilateral)

socios_comercio <- comercio %>%
  filter(socio != "World")

# Calculo dependencia
dependencia_df <- socios_comercio %>%
  left_join(comercio_total, by = c("codigo", "anio")) %>%
  mutate(dependencia = comercio_bilateral/total) %>%
  select(pais, codigo, socio, anio, comercio_bilateral, total, dependencia)