# Engnharia de atributos ----
## 0. Setup ----
# Seed (para haver reprodutibilidade)
# Vale a pena olhar não ser NA na real!
source("exploracao/libs.R")
set.seed(42)

print("Carregamento do datset")
df <- read_csv('datasets/dataset-extendido.csv')

df %>% 
  group_by(clima) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

## 1. Transformações ao dataset ----
print("Transformações...")

### 1.1. Converte a coluna de data e garante a ordem cronológica ----
df_processed <- df %>%
  mutate(datetime = as.POSIXct(datetime)) %>%
  arrange(datetime)

### 1.2. Criação de features temporais, de interação e polinomiais ----
df_processed <- df_processed %>%
  mutate(
    hora_do_dia = as.numeric(format(datetime, "%H")),
    mes = as.numeric(format(datetime, "%m")),
    # Features cíclicas
    hora_sin = sin(2 * pi * hora_do_dia / 24),
    hora_cos = cos(2 * pi * hora_do_dia / 24),
    mes_sin = sin(2 * pi * mes / 12),
    mes_cos = cos(2 * pi * mes / 12),
    dir_vento_sin = sin(dir_vento * pi / 180),
    dir_vento_cos = cos(dir_vento * pi / 180),
    # Features de interação e polinomiais
    temp_x_umidade = temp_ar * umidade_relativa,
    temp_ar_quadrado = temp_ar^2,
  )

### 1.3. Criação de "Lag Features" (atributos da hora anterior) ----
df_processed <- df_processed %>%
  mutate(
    vis_lag_1 = lag(vis, 1),
    temp_ar_lag_1 = lag(temp_ar, 1),
    umidade_relativa_lag_1 = lag(umidade_relativa, 1)
  )

### 1.4. Desloca a visibilidade 1h para frente ----
df_processed <- df_processed %>% 
  mutate(vis = lead(vis, 1))

### 1.5. Remove linhas com NAs por causa do "lag" e "lead" ----
df_processed <- na.omit(df_processed)
# Isso tá removendo linhas boas também!
# Melhor remover apenas linhas finais e inciais

print("Exportando os datsets...")
write_csv(df_processed, "exploracao/T0-dataset-extendido-transf.csv")

rm(df_processed)
rm(df)
print("Finalizado!")
