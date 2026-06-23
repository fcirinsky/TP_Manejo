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
                             destination = "iso3c"),
         iso3c = if_else(ccode == 345, "SRB", iso3c))  # Serbia (COW 345)

# creamos la lista con los países que elegimos
mis_paises <- c("AUS", "CAN", "SRB", "ROU", "VNM", "CUB", "EGY", "SYR")

voto_final <- voto_clean %>%
  filter(iso3c %in% mis_paises) %>%
  filter(year >= 1963) # Filtramos desde el caso más antiguo (Egipto)

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

expo <- read_csv("Bases comercio y voto ONU/IMF_DOT_TXG_FOB_USD.csv")
impo <- read_csv("Bases comercio y voto ONU/IMF_DOT_TMG_CIF_USD.csv")


# Limpieza tabla
paises <- c("AUS", "CAN", "SRB", "ROU", "VNM", "CUB", "EGY", "SYR")

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

#AHORA UNIMOS LAS TABLAS 
# Unir comercio con votos
union_tabla <- dependencia_df %>%
  left_join(voto_final, by = c("codigo" = "iso3c", "anio" = "year"))

# Primero agregamos las columnas de grupo y hito al panel
union_tabla <- union_tabla %>%
  mutate(
    grupo = case_when(
      codigo %in% c("AUS", "CAN") ~ "Par 1: Australia-Canadá",
      codigo %in% c("SRB", "ROU") ~ "Par 2: Serbia-Rumanía",
      codigo %in% c("VNM", "CUB") ~ "Par 3: Vietnam-Cuba",
      codigo %in% c("EGY", "SYR") ~ "Par 4: Egipto-Siria"
    ),
    tratamiento = case_when(
      codigo %in% c("AUS", "SRB", "VNM", "EGY") ~ "Tratamiento",
      TRUE ~ "Control"
    ),
    hito = case_when(
      codigo == "AUS" ~ 2001,
      codigo == "SRB" ~ 2013,
      codigo == "VNM" ~ 2001,
      codigo == "EGY" ~ 1974))


# Corrección de gráficos
union_wide <- union_tabla %>%
  select(codigo, pais, anio, socio, dependencia,
         IdealPointFP, grupo, tratamiento) %>%
  pivot_wider(names_from = socio, values_from = dependencia) %>%
  rename(dep_china = China,
         dep_eeuu = "Estados Unidos",
         dep_rusia = Rusia)

#Hito
hito_df <- tibble(
  codigo = c("AUS", "CAN", "SRB", "ROU", "VNM", "CUB", "EGY", "SYR"),
  hito = c(2001, 2001, 2013, 2013, 2001, 2001, 1974, 1974))

union_wide <- union_wide %>%
  left_join(hito_df, by = "codigo") %>%
  mutate(a_hitos = anio - hito,
         periodo = if_else(a_hitos < 0, "antes", "despues"))

# Ventanas alrededor del hito
hito20 <- filter(union_wide, abs(a_hitos) <= 20)
hito10 <- filter(union_wide, abs(a_hitos) <= 10)

# Dependencia general con Estados Unidos
ggplot(union_wide, aes(x = dep_eeuu, y = IdealPointFP)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Ideal Point en funcion de la dependencia con EE.UU.",
       x = "Dependencia comercial con EE.UU.",
       y = "Ideal Point (Voeten)") +
  theme_minimal()

mod_usa <- lm(IdealPointFP ~ dep_eeuu, data = union_wide)

# Dependencia general con China
ggplot(union_wide, aes(x = dep_china, y = IdealPointFP)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Ideal Point en funcion de la dependencia con China",
       x = "Dependencia comercial con China",
       y = "Ideal Point (Voeten)") +
  theme_minimal()

mod_china <- lm(IdealPointFP ~ dep_china, data = union_wide)

# Dependencia general con Rusia
ggplot(union_wide, aes(x = dep_rusia, y = IdealPointFP)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Ideal Point en funcion de la dependencia con Rusia",
       x = "Dependencia comercial con Rusia",
       y = "Ideal Point (Voeten)") +
  theme_minimal()

mod_rusia <- lm(IdealPointFP ~ dep_rusia, data = union_wide)

# Comparación de modelos
stargazer::stargazer(mod_usa, mod_china, mod_rusia, type = "text")

# Data Frame tratamiento y control con Ideal Point
resultados_df <- union_wide %>%
  filter(!is.na(IdealPointFP), abs(a_hitos) <= 10, a_hitos != 0) %>%
  mutate(momento = if_else(a_hitos < 0, "Antes", "Después")) %>%
  group_by(grupo, tratamiento, pais, momento) %>%
  summarise(ip = mean(IdealPointFP), n_anios = n(), .groups = "drop") %>%
  mutate(
    momento     = factor(momento, levels = c("Antes", "Después")),
    tratamiento = factor(tratamiento, levels = c("Tratamiento", "Control")))

cobertura <- resultados_df %>%
  select(grupo, pais, tratamiento, momento, n_anios) %>%
  pivot_wider(names_from = momento, values_from = n_anios)

print(cobertura)

resultados_df %>%
  distinct(pais, tratamiento)

# Gráfico de casos según Ideal Point
grafico_caso <- ggplot(resultados_df, aes(momento, ip, group = pais,
                              color = tratamiento, linetype = tratamiento)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  geom_text(
    data = filter(resultados_df, momento == "Después"),
    aes(label = pais), hjust = -0.12, size = 3, show.legend = FALSE) +
  facet_wrap(~ grupo, ncol = 2) +
  scale_color_manual(values = c("Tratamiento" = "#4F94CD", "Control" = "#EE5C42"), 
                     name = NULL) +
  scale_linetype_manual(values = c(Tratamiento = "solid", Control = "dashed"),
                        name = NULL) +
  scale_x_discrete(expand = expansion(mult = c(0.12, 0.45))) +  
  labs(
    title    = "Ideal Point: promedio 10 años antes vs 10 años después del hito",
    subtitle = "Línea sólida = país tratado | punteada = país control",
    x = NULL,
    y = "Ideal Point (Voeten)   (↑ bloque occidental   ↓ bloque no-occidental)") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold"))

print(grafico_caso)

# Formato de tabla de dirección esperada y obtenida
direccion <- tribble(
  ~grupo,                      ~polo_shock,         ~signo_esperado,
  "Par 1: Australia-Canadá",   "China (no-Occ.)",   -1,   # esperamos que BAJE
  "Par 2: Serbia-Rumanía",    "Este (no-Occ.)",    -1,   # esperamos que BAJE
  "Par 3: Vietnam-Cuba",       "EE.UU. (Occ.)",     +1,   # esperamos que SUBA
  "Par 4: Egipto-Siria",     "EE.UU. (Occ.)",     +1    # esperamos que SUBA
)

# Creación de tabla
dir_tabla <- resultados_df %>%
  select(grupo, tratamiento, momento, ip) %>%
  pivot_wider(names_from = momento, values_from = ip) %>%
  rename(antes = Antes, despues = `Después`) %>%
  mutate(cambio = despues - antes) %>%                       # cambio de cada país
  select(grupo, tratamiento, cambio) %>%
  pivot_wider(names_from = tratamiento, values_from = cambio) %>%
  mutate(Efecto = Tratamiento - Control) %>%                    # el efecto es el tratamiento − control
  left_join(direccion, by = "grupo") %>%
  mutate(consistente_con_H1 = if_else(sign(Efecto) == signo_esperado, "Sí", "No")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  select(grupo, polo_shock, Control, Tratamiento, Efecto,
         signo_esperado, consistente_con_H1)

print(dir_tabla)

# Tabla por socio
socio_shock <- tribble(
  ~grupo,                      ~socio_rel,
  "Par 1: Australia-Canadá",   "China",
  "Par 2: Serbia-Rumanía",    "China",            
  "Par 3: Vietnam-Cuba",       "Estados Unidos",
  "Par 4: Egipto-Siria",     "Estados Unidos")

# Dependencia con el socio relevante 
dep_long <- union_wide %>%
  left_join(socio_shock, by = "grupo") %>%
  mutate(dep_relevante = case_when(
    socio_rel == "China"          ~ dep_china,
    socio_rel == "Estados Unidos" ~ dep_eeuu,
    socio_rel == "Rusia"          ~ dep_rusia))

# Data frame tratamiento y control con dependencia
res_dependencia <- dep_long %>%
  filter(!is.na(dep_relevante), abs(a_hitos) <= 10, a_hitos != 0) %>%
  mutate(momento = if_else(a_hitos < 0, "Antes", "Después")) %>%
  group_by(grupo, socio_rel, tratamiento, pais, momento) %>%
  summarise(dep = mean(dep_relevante), n_anios = n(), .groups = "drop") %>%
  mutate(
    momento     = factor(momento, levels = c("Antes", "Después")),
    tratamiento = factor(tratamiento, levels = c("Tratamiento", "Control")),
    panel       = paste0(grupo, "\n(dependencia con ", socio_rel, ")"))

res_dependencia %>%
  select(grupo, pais, tratamiento, momento, n_anios) %>%
  pivot_wider(names_from = momento, values_from = n_anios)

# Grafico dependencia
grafico_dep <- ggplot(res_dependencia, aes(momento, dep, group = pais,
                                   color = tratamiento, linetype = tratamiento)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  geom_text(
    data = filter(res_dependencia, momento == "Después"),
    aes(label = pais), hjust = -0.12, size = 3, show.legend = FALSE
  ) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_color_manual(values = c("Tratamiento" = "#4F94CD", "Control" = "#EE5C42"),
                     name = NULL) +
  scale_linetype_manual(values = c(Tratamiento = "solid", Control = "dashed"),
                        name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(expand = expansion(mult = c(0.12, 0.45))) +
  labs(
    title    = "Dependencia con el socio del shock: 10 años antes vs después del hito",
    #subtitle = "Esperado: el tratado (sólida) SUBE; el control (punteada) queda plano",
    x = NULL,
    y = "Dependencia comercial (% del comercio total)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold"))

print(grafico_dep)

# Tabla comparativa
tabla_dep <- res_dependencia %>%
  select(grupo, socio_rel, tratamiento, momento, dep) %>%
  pivot_wider(names_from = momento, values_from = dep) %>%
  rename(antes = Antes, despues = Después) %>%
  mutate(cambio = despues - antes) %>%
  select(grupo, socio_rel, tratamiento, cambio) %>%
  pivot_wider(names_from = tratamiento, values_from = cambio) %>%
  mutate(dif_trat_control = Tratamiento - Control) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(tabla_dep)

# Gráfico de shocks
shock_df <- union_wide %>%
  filter(!is.na(IdealPointFP), abs(a_hitos) <= 20) %>%
  mutate(tratamiento = factor(tratamiento,
                              levels = c("Tratamiento", "Control")))

graf_shock <- ggplot(shock_df, aes(a_hitos, IdealPointFP,
                                    color = tratamiento, linetype = tratamiento)) +
  geom_vline(xintercept = 0, color = "#C0392B",
             linetype = "dashed", linewidth = 0.6) +             
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2, alpha = 0.7) +
  facet_wrap(~ grupo, ncol = 2, scales = "free_y") +             
  scale_color_manual(values = c("Tratamiento" = "#4F94CD",
                                "Control"     = "#EE5C42"),
                     name = NULL) +
  scale_linetype_manual(values = c(Tratamiento = "solid",
                                   Control     = "dashed"),
                        name = NULL) +
  labs(
    title    = "Ideal Point: tratamiento vs control, alineado al hito",
    subtitle = "Eje X = años respecto del hito (0 = año del shock) | línea roja = hito",
    x = "Años respecto del hito",
    y = "Ideal Point (Voeten)   (↑ bloque occidental   ↓ bloque no-occidental)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold"))

print(graf_shock)



