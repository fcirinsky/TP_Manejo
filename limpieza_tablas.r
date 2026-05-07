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
