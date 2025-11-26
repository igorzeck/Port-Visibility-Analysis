# Relatório V - modelo RF com XGDBoost (CLassificação) ----
# Treino de um único modelo de teste
# ---- 1. SETUP ----
source("exploracao/libs.R")
library(dplyr)

if (!require("xgboost")) install.packages("xgboost", dependencies = TRUE)
library(xgboost)

# ---- 2. DIVISÃO DE CLASSES ----
df <- read_csv("exploracao/T0-dataset-extendido-transf.csv") %>% 
  mutate(tipo_vis = cut(vis, 
                        breaks = c(-1, 1000, 6000, 9999, 10000),
                        labels = c("nevoa",
                                   "visBaixa",
                                   "visMedia",
                                   "visCompleta"))
  ) %>% 
  select(-vis) %>% 
  mutate(clima = as.factor(clima))

# ---- 3. XGBOOST ----
tic(paste("Começando a treinar o modelo..."))
set.seed(42)
# Divisão do dataset
idx <- createDataPartition(df$tipo_vis, p = 0.8, list = FALSE)
df_treino <- df[idx, ]
df_teste  <- df[-idx, ]

df_treino$tipo_vis <- factor(df_treino$tipo_vis)
df_teste$tipo_vis  <- factor(df_teste$tipo_vis,
                            levels = levels(df_treino$tipo_vis))

dv <- dummyVars(tipo_vis ~ ., data = df_treino, fullRank = TRUE)

# Para os dummies
treino_x <- predict(dv, df_treino) %>% as.data.frame()
teste_x  <- predict(dv, df_teste)  %>% as.data.frame()

# Garante colunas idênticas
aus_em_treino <- setdiff(colnames(teste_x), colnames(treino_x))
aus_em_teste  <- setdiff(colnames(treino_x), colnames(teste_x))

for (col in aus_em_treino) treino_x[, col] <- 0
for (col in aus_em_teste)  teste_x[, col]  <- 0

treino_x <- treino_x[, sort(colnames(treino_x))]
teste_x  <- teste_x [, sort(colnames(teste_x))]

# Transforma em matriz numérica pro XGBoost
label_treino <- as.numeric(df_treino$tipo_vis) - 1
label_teste  <- as.numeric(df_teste$tipo_vis)  - 1

dtreino <- xgb.DMatrix(data = as.matrix(treino_x), label = label_treino)
dteste  <- xgb.DMatrix(data = as.matrix(teste_x),  label = label_teste)

# Setup dos parâmetros
params <- list(
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(levels(df_treino$tipo_vis)),
  eta = 0.08,
  max_depth = 5,  # Aparenta ter impacto negativo!
  subsample = 0.8,
  colsample_bytree = 0.8,
  nthread = 10  # Aqui ele entende que são 5 threads virtuais
)

# Treino em si
set.seed(42)
modelo <- xgb.train(
  params = params,
  data = dtreino,
  nrounds = 1000, # Basicamente ntrees, mas com árvores que não são independetes
  watchlist = list(treino = dtreino, teste = dteste),
  verbose = 1
)

# Para timer e pega tempo
toc(log = TRUE, quiet = TRUE)
log_list <- tic.log(format = FALSE)
entry <- log_list[[length(log_list)]]
runtime <- entry$toc - entry$tic
tic.clearlog()

# Tempo de treino do modelo
cat(paste0(round(runtime,3),"s"))

# modelo <- readRDS("exploracao/modelo-v-3-c.RDS")

## Análise do modelo ----
modelo

### Log de treino ----
# Mlogloss do treino e teste
modelo$evaluation_log %>% 
  ggplot() +
  geom_line(aes(iter, train_mlogloss), colour = "blue") +
  geom_line(aes(iter, test_mlogloss), color = "red")

#### Predições ----
# Treino
# Lista de probabilidades de cada classe
pred_prob <- predict(modelo, dtreino)
length(pred_prob)
# Separa em 4 colunas (uma matriz) com 'num_class' de colunas
pred_prob <- matrix(pred_prob,
                    ncol = params$num_class,
                    byrow = TRUE)
dim(pred_prob)
# Pega a posição (coluna) com maior valor, linha a linha
max.col(pred_prob)
# Retira de cada valor (já que as classes começam com índice 0)
pred_class <- max.col(pred_prob) - 1
# Factoriza as classes de previsão utilizando os levels da alvo
pred_factor <- factor(pred_class,
                      labels = levels(df_treino$tipo_vis))

# A acurácia de Névoa foi perfeita! Suspeito... Mas, faz sentido
confusionMatrix(pred_factor, df_treino$tipo_vis)

# Teste
pred_prob <- predict(modelo, dteste)
pred_prob <- matrix(pred_prob,
                    ncol = params$num_class,
                    byrow = TRUE)

pred_class <- max.col(pred_prob) - 1
pred_factor <- factor(pred_class,
                      labels = levels(df_treino$tipo_vis))

# Resultados mais sensíveis!
confusionMatrix(pred_factor, df_teste$tipo_vis)

#### Importância ----
imp <- xgb.importance(model = modelo)
print(imp)
# Relativo ao primeiro para deixar em escala de 0 a 100
xgb.plot.importance(imp, rel_to_first = TRUE)

## Salva o modelo treinado ----
saveRDS(modelo, "exploracao/modelo-v-3-c.RDS")