# Relatório V - modelo RF com XGBoost (Regressão) ----
# Treino de um único modelo de teste
# ---- 1. SETUP ----
source("exploracao/libs.R")
library(dplyr)

if (!require("xgboost")) install.packages("xgboost", dependencies = TRUE)
library(xgboost)

# ---- 2. DIVISÃO DE CLASSES ----
df <- read_csv("exploracao/T0-dataset-extendido-transf.csv") %>%
  mutate(clima = as.factor(clima))

# ---- 3. XGBOOST ----
tic(paste("Começando a treinar o modelo..."))
set.seed(42)
# Divisão do dataset
tam <- floor(0.8 * nrow(df))
idx <- sample(seq_len(nrow(df)), tam)

df_treino <- df[idx, ]
df_teste  <- df[-idx, ]

y_treino <- df_treino$vis
y_teste  <- df_teste$vis

df_treino$vis <- NULL
df_teste$vis  <- NULL

# Para os dummies (Por one-hot encoding)
dummies <- caret::dummyVars(~ ., data = df_treino, fullRank = TRUE)
treino_x <- predict(dummies, df_treino)
teste_x  <- predict(dummies, df_teste)

# Garante colunas idênticas
# aus_em_treino <- setdiff(colnames(teste_x), colnames(treino_x))
# aus_em_teste  <- setdiff(colnames(treino_x), colnames(teste_x))
# 
# for (col in aus_em_treino) treino_x[, col] <- 0
# for (col in aus_em_teste)  teste_x[, col]  <- 0
# 
# treino_x <- treino_x[, sort(colnames(treino_x))]
# teste_x  <- teste_x [, sort(colnames(teste_x))]

# Transforma em matriz numérica pro XGBoost
# label_treino <- as.numeric(df_treino$tipo_vis) - 1
# label_teste  <- as.numeric(df_teste$tipo_vis)  - 1

dtreino <- xgb.DMatrix(data = as.matrix(treino_x), label = y_treino)
dteste  <- xgb.DMatrix(data = as.matrix(teste_x),  label = y_teste)

# Setup dos parâmetros
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.01,
  max_depth = 8,
  subsample = 1,
  colsample_bytree = 1,
  nthread = 10
)

# Treino
modelo <- xgb.train(
  params = params,
  data = dtreino,
  nrounds = 5000,
  watchlist = list(train = dtreino, test = dteste),
  print_every_n = 50
)

# Para timer e pega tempo
toc(log = TRUE, quiet = TRUE)
log_list <- tic.log(format = FALSE)
entry <- log_list[[length(log_list)]]
runtime <- entry$toc - entry$tic
tic.clearlog()

# Tempo de treino
cat(paste0(round(runtime,3),"s"))

# modelo <- readRDS("exploracao/modelo-v-4-r.RDS")

## Predições ----
modelo

### Log de treino ----
modelo$evaluation_log
modelo$evaluation_log %>% 
  ggplot() +
  geom_line(aes(iter, train_rmse), colour = "blue") +
  geom_line(aes(iter, test_rmse), color = "red")

#### Predições ----
# Treino
pred <- predict(modelo, dtreino)
y_true_treino <- y_treino
rmse <- sqrt(mean((pred - y_treino)^2))
mae  <- mean(abs(pred - y_treino))
r2 <- 1 - sum((pred_treino - y_true_treino)^2) /
  sum((mean(y_true_treino) - y_true_treino)^2)

# Resultados
rmse; mae; r2

# Teste
pred <- predict(modelo, dteste)
y_true <- y_teste
rmse <- sqrt(mean((pred - y_teste)^2))
mae  <- mean(abs(pred - y_teste))
r2 <- 1 - sum((pred - y_true)^2) /
  sum((mean(y_true) - y_true)^2)

# Resultados
rmse; mae; r2

#### Importância ----
imp <- xgb.importance(model = modelo)
print(imp)
# Relativo ao primeiro elemento para deixar em escala de 0 a 1
xgb.plot.importance(imp, rel_to_first = TRUE)

# Salva o modelo treinado
saveRDS(modelo, "exploracao/modelo-v-4-r.RDS")