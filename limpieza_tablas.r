library(tidyverse)
library(countrycode)

# 1. Cargar datos (asumiendo que los bajaste)
raw_comtrade <- read_csv("data/comtrade_data.csv")
raw_voeten <- read_csv("data/Idealpoints.csv")

# 2. Limpiar Comtrade
comtrade_clean <- raw_comtrade %>%
  # Pasamos códigos numéricos de ONU a ISO3 (ej: 36 -> AUS)
  mutate(iso3c = countrycode(reporter_code, origin = "iso3n", destination = "iso3c")) %>%
  filter(iso3c %in% c("AUS", "CAN", "HUN", "POL", "VNM", "CUB", "EGY", "DZA")) %>%
  select(year, iso3c, partner_iso3c, trade_value_usd)

# 3. Crear la variable de "Dependencia"
# Queremos saber cuánto representa el socio (ej China) sobre el total del país
comercio_final <- comtrade_clean %>%
  group_by(year, iso3c) %>%
  summarize(
    total_trade = sum(trade_value_usd),
    partner_trade = sum(trade_value_usd[partner_iso3c %in% c("CHN", "USA", "RUS")]),
    prop_trade = partner_trade / total_trade
  )

# 4. Limpiar Voeten y unir
voeten_clean <- raw_voeten %>%
  mutate(iso3c = countrycode(ccode, origin = "cown", destination = "iso3c")) %>%
  select(year, iso3c, IdealPointAll)

# 5. EL MERGE FINAL
dataset_tp <- left_join(comercio_final, voeten_clean, by = c("year", "iso3c"))