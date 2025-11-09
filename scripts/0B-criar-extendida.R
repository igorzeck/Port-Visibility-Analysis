# Script para a criação do dataset definitivo extendido ----
## 0. Setup ----
source('exploracao/libs.R')
library(measurements)

# Metar
df_metar <- read_csv("datasets/metar-SBGL-2011-01-01-2025-11-01.csv")
glimpse(df_metar)

# ERA5 - lmlt
df_lmlt <- read_csv("datasets/lmlt-galeao.csv")
df_lmlt

# Informes - Umidade
df_ur <- read_csv("datasets/info-SBGL-2011-01-01-2025-08-11.csv") %>% 
  janitor::clean_names()
df_ur

### 0.1 Lmlt e datetime ----
# Junta datetime e convert para Celsius o valor (de Kelvin)
df_lmlt <- df_lmlt %>% 
  mutate(datetime = as_datetime(paste0(data, hora), format="%Y%m%d%H%M")) %>% 
  mutate(lmlt = conv_unit(valor, "K", "C")) %>% 
  select(datetime, lmlt)

df_lmlt %>% 
  filter(is.na(lmlt))

## 1. Junção dos valores dos Metar, Informes e do LMLT
df <- df_metar %>% 
  inner_join(df_lmlt, by="datetime") %>% 
  inner_join(df_ur, by="datetime") 

## 2. Renomeia colunas
glimpse(df)

df <- df %>% 
  rename(
    vel_vento = wind_speed,
    dir_vento = wind_direction,
    temp_ar = temperature,
    temp_orvalho = dew_point,
    pressao = pressure,
    vis = visibility,
    clima = weather_information
  )

glimpse(df)

## 3. Exporta resultado
write_csv(df, "datasets/dataset-extendido.csv")
