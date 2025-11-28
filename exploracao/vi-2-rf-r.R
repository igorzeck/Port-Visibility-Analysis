# Modelo para RF-R XGBoost do relatório VI ----
# Especifiações:
#   - hrs: 1:60:1 - 60 iterações
# Total de modelos: 60
# ---- 1. SETUP ----
source("exploracao/libs.R")
library(dplyr)

if (!require("xgboost")) install.packages("xgboost", dependencies = TRUE)
library(xgboost)

# ---- 2. DIVISÃO DE CLASSES ----
df_original <- read_csv("exploracao/T0-dataset-extendido-transf.csv") %>%
  mutate(clima = as.factor(clima))

# ---- 3. ARQUIVO ----
# Arquivo de saída
outfile <- "exploracao/arq-vi-2.csv"

if (!file.exists(outfile)) {
  write.csv(
    tibble(
      hrs = numeric(),
      RMSE_treino = numeric(),
      Rsquared_treino = numeric(),
      MAE_treino = numeric(),
      RMSE_teste = numeric(),
      Rsquared_teste = numeric(),
      MAE_teste = numeric(),
      runtime_sec = numeric()
    ),
    outfile,
    row.names = TRUE
  )
}

# ---- 4. XGBOOST ----
min_hrs <- 1
max_hrs <- 60
step_hrs <- 1

runtime_total <- 0

# For loop
for (hrs in seq(min_hrs, max_hrs, step_hrs)) {
  tic()
  df <- df_original
  # -- Lead das horas --
  if (hrs != 1) {
    df <- df %>% 
      mutate(vis = lead(vis, hrs)) %>% 
      na.omit()
  }

  tam <- floor(0.8 * nrow(df))
  idx <- sample(seq_len(nrow(df)), tam)
  
  df_treino <- df[idx, ]
  df_teste  <- df[-idx, ]
  
  y_treino <- df_treino$vis
  y_teste  <- df_teste$vis
  
  df_treino$vis <- NULL
  df_teste$vis  <- NULL
  
  dummies <- caret::dummyVars(~ ., data = df_treino, fullRank = TRUE)
  treino_x <- predict(dummies, df_treino)
  teste_x  <- predict(dummies, df_teste)
  
  for (col in aus_em_treino) treino_x[, col] <- 0
  for (col in aus_em_teste)  teste_x[, col]  <- 0
  
  treino_x <- treino_x[, sort(colnames(treino_x))]
  teste_x  <- teste_x [, sort(colnames(teste_x))]
  
  dtreino <- xgb.DMatrix(data = as.matrix(treino_x), label = y_treino)
  dteste  <- xgb.DMatrix(data = as.matrix(teste_x),  label = y_teste)
  # -- Setup dos parâmetros --
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.01,
    max_depth = 8,
    subsample = 1,
    colsample_bytree = 1,
    nthread = 10
  )
  
  # -- Treino em si --
  modelo <- xgb.train(
    params = params,
    data = dtreino,
    nrounds = 1000,
    watchlist = list(train = dtreino, test = dteste),
    verbose = 0
  )
  
  # Para timer e pega o tempo
  toc(log = TRUE, quiet = TRUE)
  log_list <- tic.log(format = FALSE)
  entry <- log_list[[length(log_list)]]
  runtime <- entry$toc - entry$tic
  tic.clearlog()
  
  # -- Análise do modelo --
  # - Treino -
  pred <- predict(modelo, dtreino)
  y_true_treino <- y_treino
  rmse_treino <- sqrt(mean((pred - y_treino)^2))
  mae_treino  <- mean(abs(pred - y_treino))
  rsq_treino <- 1 - sum((pred - y_true_treino)^2) /
    sum((mean(y_true_treino) - y_true_treino)^2)
  
  # - Teste -
  pred <- predict(modelo, dteste)
  y_true_teste <- y_teste
  rmse_teste <- sqrt(mean((pred - y_teste)^2))
  mae_teste  <- mean(abs(pred - y_true_teste))
  rsq_teste <- 1 - sum((pred - y_true_teste)^2) /
    sum((mean(y_true_teste) - y_true_teste)^2)
  
  # Append dos resultados
  write.table(
    tibble(
      hrs = hrs,
      RMSE_treino = rmse_treino,
      Rsquared_treino = rsq_treino,
      MAE_treino = mae_treino,
      RMSE_teste = rmse_teste,
      Rsquared_teste = rsq_teste,
      MAE_teste = mae_teste,
      runtime_sec = runtime
    ),
    file = outfile,
    sep = ",",
    col.names = FALSE,
    append = TRUE,
    row.names = FALSE
  )
  
  message(
    " hrs = ", hrs,
    " → RMSE treino = ", round(rmse_treino, 3),
    " RMSE teste = ", round(rmse_teste, 3),
    " R2 treino = ", round(rsq_treino, 3),
    " R2 teste = ", round(rsq_teste, 3),
    " MAE teste = ", round(mae_teste, 3),
    " (", round(runtime, 2), " s)"
  )
  
  runtime_total <- runtime_total + runtime
}

# ---- 5. FINALIZAÇÃO ----
cat("Tempo total:", runtime_total)
