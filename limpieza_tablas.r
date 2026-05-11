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


#----------------------------------------------
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

#----------------------------------------------
#AHORA UNIMOS LAS TABLAS 
# Unir comercio con votos
union_tabla <- dependencia_df %>%
  left_join(voto_final, by = c("codigo" = "iso3c", "anio" = "year"))

# Primero agregamos las columnas de grupo y hito al panel
panel <- panel %>%
  mutate(
    grupo = case_when(
      codigo %in% c("AUS", "CAN") ~ "Par 1: Australia-Canadá",
      codigo %in% c("HUN", "POL") ~ "Par 2: Hungría-Polonia",
      codigo %in% c("VNM", "CUB") ~ "Par 3: Vietnam-Cuba",
      codigo %in% c("EGY", "DZA") ~ "Par 4: Egipto-Argelia"
    ),
    tratamiento = case_when(
      codigo %in% c("AUS", "HUN", "VNM", "EGY") ~ "Tratamiento",
      TRUE ~ "Control"
    ),
    hito = case_when(
      codigo == "AUS" ~ 2001,
      codigo == "HUN" ~ 2011,
      codigo == "VNM" ~ 2001,
      codigo == "EGY" ~ 1974
    )
  )

# Un gráfico por país: dependencia (color por socio) e ideal point (negro punteado)
# Normalizamos para poder ver las dos variables en el mismo eje
panel_norm <- panel %>%
  group_by(codigo, socio) %>%
  mutate(
    dep_norm = (dependencia - min(dependencia, na.rm = TRUE)) /
      (max(dependencia, na.rm = TRUE) - min(dependencia, na.rm = TRUE)),
    ip_norm  = (IdealPointFP - min(IdealPointFP, na.rm = TRUE)) /
      (max(IdealPointFP, na.rm = TRUE) - min(IdealPointFP, na.rm = TRUE))
  ) %>%
  ungroup()

ggplot(panel_norm, aes(x = anio)) +
  geom_line(aes(y = dep_norm, color = socio), linewidth = 0.9) +
  geom_line(aes(y = ip_norm), color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = hito), color = "red", linetype = "dotted", linewidth = 0.8) +
  facet_wrap(~ pais, ncol = 2, scales = "free_x") +
  labs(
    title    = "Dependencia comercial e Ideal Point por país",
    subtitle = "Línea punteada negra = Ideal Point | Línea de color = Dependencia por socio | Línea roja = Hito",
    x        = "Año",
    y        = "Valor normalizado (0–1)",
    color    = "Socio"
  ) +
  theme_minimal()

#Ademas hacemos un grafico de tratameinto vs control
panel %>%
  filter(!is.na(IdealPointFP)) %>%
  ggplot(aes(x = anio, y = IdealPointFP, color = pais, linetype = tratamiento)) +
  geom_line(linewidth = 1) +
  geom_vline(aes(xintercept = hito), color = "red", linetype = "dotted") +
  facet_wrap(~ grupo, ncol = 2, scales = "free") +
  labs(
    title    = "Ideal Point: Tratamiento vs Control por par",
    subtitle = "Línea roja punteada = hito comercial",
    x        = "Año",
    y        = "Ideal Point (Voeten)",
    color    = "País",
    linetype = "Grupo"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
