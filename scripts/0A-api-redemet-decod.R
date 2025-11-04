# Exploração dos CSVs dos METARs do ERA5 (em busca de mais variávies) ---
## 0. Setup ---
# install.packages("pmetar")
library(pmetar)
source("exploracao/libs.R")
df <- read_delim(delim="<>","datasets/raw-metar-SBGL-2011-01-01-2025-11-01.txt") %>% 
  janitor::clean_names()

## 0.1 Decodificação de dados no df ----
decoded_informes <- metar_decode(df$metar)
# Demora alguns minutos...

## 1. Remove elementos que não foram decodificados ----
df_transf <- df_transf %>% 
  filter(!grepl("Incorrect",remark))

# Remove a coluna como um todo
df_transf <- df_transf %>% 
  select(-remark)

## 2. Airpot ICAO ----
df_transf <- df_transf %>% 
  select(-airport_icao)

## 3. DateTime ----
df_transf <- df_transf %>% 
  select(-metar_date)

df_transf <- df_transf %>% 
  select(-day_of_month)

df_transf <- df_transf %>% 
  select(-hour)

df_transf <- df_transf %>% 
  select(-time_zone)

## 3. Unidades ----
df_transf <- df_transf %>% 
  select(!contains("_unit"))

## 4. Gust ----
# Grande maioria é NA, então corta a coluna
df_transf <- df_transf %>% 
  select(-gust)

## 5. Shear ----
# Grande maioria é NA, então corta a coluna
df_transf <- df_transf %>% 
  select(-wind_shear)

## 5. Wind direction ----
# Ele aparenta conter texto em alguns casos
# Por agora só pega o valor inicial
df_transf <- df_transf %>% 
  mutate(wind_direction = as.numeric(str_split_fixed(wind_direction,";", n = 2)[,1]))

## 5. Visbility ----
# Ele aparenta conter texto em alguns casos
# Por agora mantém os valores

## 5. Cloud Coverage ----
# Ele aparenta conter texto em alguns casos
# Por agora mantém os valores
df_transf %>% 
  distinct(cloud_coverage) %>% 
  view()

## 6. Weather Information ----
# Ele aparenta conter texto em alguns casos
# Por agora mantém os valores

## 6. Runway visibility ----
# Ajudaria a orientação direcional
# Por agora mantém os valores

## 7. Airport ----
df_transf <- df_transf %>% 
  select(-contains("airport"))

## 8. Demais ----
df_transf <- df_transf %>% 
  select(-latitude, -longitude, -elevation)

df_transf <- df_transf %>% 
  select(-decode_date, -original_metar)

## 9. Final ----
write_csv(df_transf, "datasets/metar-SBGL-2011-01-01-2025-11-01.csv")
