# Exploração dos CSVs dos METARs do ERA5 (em busca de mais variávies) ---
## 0. Setup ---
if (!require("pmetar")) install.packages("pmetar", dependencies = TRUE)
if (!require("measurements")) install.packages("measurements", dependencies = TRUE)

library(pmetar)
library(measurements)
source("exploracao/libs.R")
df_raw <- read_delim(delim="<>","datasets/raw-metar-SBGL-2011-01-01-2025-11-01.txt")

## 0.1 Decodificação de dados no df ----
df <- metar_decode(df_raw$metar) %>% 
  janitor::clean_names()
# Demora alguns minutos...

## 0.2 Adição do datetime
df <- df %>% 
  mutate(datetime = df_raw$datetime) %>% 
  relocate(datetime)

## 1. Remove elementos que não foram decodificados ----
df <- df %>% 
  filter(!grepl("Incorrect",remark))

# Remove a coluna como um todo
df <- df %>% 
  select(-remark)

## 2. Airpot ICAO ----
df <- df %>% 
  select(-airport_icao)

## 3. DateTime ----
df <- df %>% 
  select(-metar_date)

df <- df %>% 
  select(-day_of_month)

df <- df %>% 
  select(-hour)

df <- df %>% 
  select(-time_zone)

## 3. Unidades ----
df <- df %>% 
  select(!contains("_unit"))

## 4. Gust ----
# Grande maioria é NA, então corta a coluna
df <- df %>% 
  select(-gust)

## 5. Shear ----
# Grande maioria é NA, então corta a coluna
df <- df %>% 
  select(-wind_shear)

## 5. Wind direction ----
# Ele aparenta conter texto em alguns casos
# Por agora só pega o valor inicial
df <- df %>% 
  mutate(wind_direction = as.numeric(str_split_fixed(wind_direction,";", n = 2)[,1]))

## 5. Visbility ----
# Ele aparenta conter texto em alguns casos
# Por agora mantém os valores
df %>% 
  group_by(visibility) %>% 
  summarise(n = n()) %>% 
  view()
# Para CAVOK e 9999 se mantém como 10.000 a visibilidade
# De acordo com o DECEA (https://ajuda.decea.mil.br/base-de-conhecimento/como-decodificar-o-metar-e-o-speci/)
# A sigla CAVOK (“Ceiling na Visibility OK”, teto e visibilidade OK) pode substituir visibilidade, RVR, Tempo Presente e nebulosidade desde que a visibilidade seja maior que 10 km, sem nuvens abaixo de 5000 pés (ou a maior altitude mínima do setor e nenhuma cumulonimbus ou cumulus em qualquer nível) e ausência de outros fenômenos significativos. 
# O type é do DECEA mesmo
df <- df %>% 
  mutate(visibility = case_when(
    visibility == "9999" ~ "10000",
    visibility == "Ceiling And Visibility OK" ~ "10000",
    TRUE ~ visibility
  ))
# Nos demais casos, por serem muito dispersos
# Decide-se cortar a ideia de direção e se pega o
# menor valor numérico na string
df <- df %>%
  mutate(
    visibility = str_extract_all(visibility, "\\d+(?:\\.\\d+)?") |> 
      lapply(as.numeric) |>                                             
      sapply(min, na.rm = TRUE)
  )

## 5. Cloud Coverage ----
# Ele aparenta conter texto em alguns casos
# Por agora pega so a primeira categoria
df %>% 
  distinct(cloud_coverage) %>% 
  view()

# lógica:
# 1. Separa para pegar primeira aprte antes do ';'
# 2. Divide pelos espaços e pega:
#  2.1 Categoria (Few, Broken, etc.) - 1ª
#  2.2 Altura da base da nuvem em ft (após "at") - apenas primeira palavra
df <- df %>%
  mutate(cloud_coverage = str_split_fixed(cloud_coverage, ";", 2)[, 1]) %>% 
  mutate(categ_nuvem = str_split_fixed(cloud_coverage, " ", 2)[, 1],
         altura_nuvem = conv_unit(as.numeric(str_extract(cloud_coverage, "(?<=\\bat\\s)\\S+")), "feet", "meter"))
# Por agora ignora os tipos de nuvem - complexo demais pegar eles

# Verifica situação dos NAs
df %>% 
  select(altura_nuvem) %>% 
  mutate(vazio = is.na(altura_nuvem)) %>% 
  group_by(vazio) %>% 
  summarise(n = n())
# Bastante valores vazios!
df %>% 
  filter(is.na(altura_nuvem)) %>% 
  group_by(cloud_coverage) %>% 
  group_by(visibility, cloud_coverage) %>% 
  summarise(n = n()) %>% 
  view()
# Em geral, aparenta ser o caso de que
# quando não há nuvens ele pode estar com o campo
# vazio ou "No (nil) significant cloud"
df %>% 
  filter(is.na(altura_nuvem)) %>% 
  group_by(cloud_coverage) %>% 
  group_by(weather_information, cloud_coverage) %>% 
  summarise(n = n()) %>% 
  view()
# Por agora, quando o campo estiver vazio ou com
# No (nil) signifcant cloud, ele receberá um NA
df %>% 
  filter(grepl("nil", cloud_coverage)) %>% 
  group_by(categ_nuvem, altura_nuvem) %>% 
  summarise(n = n())
# A categoria acaba sendo o "No" e fica sem altura
# Acho aceitável
df %>% 
  filter(cloud_coverage == "") %>% 
  group_by(categ_nuvem, altura_nuvem) %>% 
  summarise(n = n())
# Aqui seria uma boa mudar para ficar "No" também
df <- df %>% 
  mutate(categ_nuvem = if_else(
    categ_nuvem == "", "No", categ_nuvem)
  ) 

df %>% 
  filter(is.na(altura_nuvem)) %>% 
  group_by(categ_nuvem, altura_nuvem) %>% 
  summarise(n = n())
# Todos os dados vazios foram levados em conta!
df <- df %>% 
  select(-cloud_coverage)

## 6. Weather Information ----
# Ele aparenta conter texto em alguns casos
# Por agora mantém os valores

## 6. Runway visibility ----
# Ajudaria a orientação direcional
df %>% 
  group_by(runway_visibility) %>% 
  summarise(n = n()) %>% 
  view()
# Ignora, já que a gigantesca maioria é NAs

df <- df %>% 
  select(-runway_visibility)

## 7. Airport ----
df <- df %>% 
  select(-contains("airport"))

## 8. Demais ----
df <- df %>% 
  select(-latitude, -longitude, -elevation)

df <- df %>% 
  select(-decode_date, -original_metar)

## Final ----
write_csv(df, "datasets/dataset-extendido.csv")
