# Engnharia de atributos ----
## 0. Setup ----
# Seed (para haver reprodutibilidade)
set.seed(42)
# Test passa a ser o df_test_final (contém 10% dos dados)
print("Carregamento do datset")
df <- read_csv('dataset-definitivo.csv')

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
    dir_vento_sin = sin(`dir-vento(graus)` * pi / 180),
    dir_vento_cos = cos(`dir-vento(graus)` * pi / 180),
    # Features de interação e polinomiais
    temp_x_umidade = `temp-ar(c)` * `umidade-relativa`,
    temp_ar_quadrado = `temp-ar(c)`^2,
  )

### 1.3. Criação de "Lag Features" (atributos da hora anterior) ----
df_processed <- df_processed %>%
  mutate(
    `vis(m)_lag_1` = lag(`vis(m)`, 1),
    `temp-ar(c)_lag_1` = lag(`temp-ar(c)`, 1),
    `umidade-relativa_lag_1` = lag(`umidade-relativa`, 1)
  )

### 1.4. Desloca a visibilidade 1h para frente ----
df_processed <- df_processed %>% 
  mutate(`vis(m)` = lead(`vis(m)`, 1))

### 1.5. Remove linhas com NAs por causa do "lag" e "lead" ----
df_processed <- na.omit(df_processed)

print("Exportando os datsets...")
write_csv(df_processed, "dataset-j0-definitivo-transf.csv")

rm(df_processed)
rm(df)
print("Finalizado!")
