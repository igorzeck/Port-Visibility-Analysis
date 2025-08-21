# --- 1. INSTALAÇÃO E CARREGAMENTO DOS PACOTES ---
# Instala os pacotes necessários caso eles ainda não estejam instalados
if (!require("readr")) install.packages("readr", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("ranger")) install.packages("ranger", dependencies = TRUE)
if (!require("doParallel")) install.packages("doParallel", dependencies = TRUE)
if (!require("tictoc")) install.packages("tictoc", dependencies = TRUE)

# Carrega todos os pacotes na sessão
library(readr)
library(dplyr)
library(caret)
library(ranger)
library(doParallel)
library(tictoc)


# --- 2. ENGENHARIA DE ATRIBUTOS AVANÇADA ---
# Carrega o dataset original
file_path <- "dataset-features-avancadas.csv"
if (!file.exists(file_path)) {
  stop("Erro: O arquivo 'dataset-features-avancadas.csv' não foi encontrado.")
}
df_raw <- read_csv(file_path)

# Converte a coluna de data e garante a ordem cronológica
df_processed <- df_raw %>%
  mutate(datetime = as.POSIXct(datetime)) %>%
  arrange(datetime)

# Criação de features temporais, de interação e polinomiais
df_processed <- df_processed %>%
  mutate(
    hora_do_dia = as.numeric(format(datetime, "%H")),
    mes = as.numeric(format(datetime, "%m")),
    # Features cíclicas
    hora_sin = sin(2 * pi * hora_do_dia / 24),
    hora_cos = cos(2 * pi * hora_do_dia / 24),
    mes_sin = sin(2 * pi * mes / 12),
    mes_cos = cos(2 * pi * mes / 12),
    # Features de interação e polinomiais
    temp_x_umidade = `temp-ar(c)` * `umidade-relativa`,
    temp_ar_quadrado = `temp-ar(c)`^2
  )

# Criação de "Lag Features" (atributos da hora anterior)
df_processed <- df_processed %>%
  mutate(
    `vis(m)_lag_1` = lag(`vis(m)`, 1),
    `temp-ar(c)_lag_1` = lag(`temp-ar(c)`, 1),
    `umidade-relativa_lag_1` = lag(`umidade-relativa`, 1)
  )

# Criação da variável alvo transformada (logarítmica)
df_processed <- df_processed %>%
  mutate(log_vis_m = log1p(`vis(m)`))

# Remove a primeira linha que conterá NAs por causa do "lag"
df_processed <- na.omit(df_processed)


# --- 3. PREPARAÇÃO FINAL PARA O TREINAMENTO ---
# **CORREÇÃO CRÍTICA DO DATA LEAKAGE**
# Remove a variável alvo original `vis(m)` e a coluna `datetime` (redundante)
df_para_treino <- df_processed %>%
  select(-`vis(m)`, -datetime)

cat("Dataset final preparado para o treinamento.\n")
cat(sprintf("Número de amostras: %d\n", nrow(df_para_treino)))
cat(sprintf("Número de preditores: %d\n", ncol(df_para_treino) - 1))


# --- 4. CONFIGURAÇÃO E TREINAMENTO DO MODELO ---
# Configura o cluster para processamento paralelo
num_cores <- detectCores()
cl <- makeCluster(num_cores - 1)
registerDoParallel(cl)
cat(sprintf("\nProcessamento paralelo registrado para usar %d núcleos.\n", num_cores - 1))

# Define o controle da validação cruzada
# Usaremos 5 folds para um bom equilíbrio entre velocidade e robustez
set.seed(42) # Para reprodutibilidade
control <- trainControl(method = "cv",
                        number = 5,
                        verboseIter = TRUE,
                        allowParallel = TRUE)

# Inicia o cronômetro
tic("Tempo total de treinamento do modelo")

# Treina o modelo Random Forest com 'ranger'
# O caret irá otimizar o 'mtry' automaticamente
# A variável alvo é a 'log_vis_m'
model_final <- train(log_vis_m ~ .,
                     data = df_para_treino,
                     method = "ranger",
                     trControl = control,
                     metric = "RMSE",
                     importance = "permutation",
                     # Define um número robusto de árvores
                     num.trees = 500)

# Para o cronômetro e exibe o tempo total
toc()

# Para o cluster de processamento paralelo
stopCluster(cl)
registerDoSEQ()
cat("Cluster paralelo finalizado.\n\n")


# --- 5. EXIBIÇÃO DOS RESULTADOS ---
cat("--- Resultados Finais do Modelo ---\n")
print(model_final)

# Mostra as variáveis mais importantes
cat("\n--- Importância das Variáveis ---\n")
imp <- varImp(model_final, scale = FALSE)
print(imp)