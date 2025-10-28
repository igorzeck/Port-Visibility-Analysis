# Teste para futuro E ntrees variável ----
# Teste com modelo de regressão
# A largura do dataset foi fixa para 0.5 por 0.5
# ---- 1. Setup ----
source("libs.R")
library(lubridate)

# ---- 2. CARREGAMENTO E PRÉ-PROCESSAMENTO ----
df <- read_csv("dataset-j0-definitivo-transf.csv")

# ---- 3. CONFIGURAÇÃO DE PARALELISMO ----
num_cores <- 5
cl <- makeCluster(num_cores)
registerDoParallel(cl)
cat(sprintf("\nProcessamento paralelo registrado para usar %d núcleos.\n", num_cores))

# ---- 4. CONTROLE DE TREINAMENTO ----
# arquivo dos resultados
outfile <- "arq-j4-tam-ntrees.csv"

if (!file.exists(outfile)) {
  write.csv(
    tibble(
      ntrees = numeric(),
      RMSE_treino = numeric(),
      Rsquared_treino = numeric(),
      MAE_treino = numeric(),
      RMSE_teste = numeric(),
      Rsquared_teste = numeric(),
      MAE_teste = numeric(),
      runtime_sec = numeric()
    ),
    outfile,
    row.names = FALSE
  )
}

# -- Divisão do dataset --
train_indices <- seq(1, nrow(df) - 1, 2)

df_para_treino <- df[train_indices, ]
df_para_teste <- df[-train_indices, ]

total_trees <- 201
step_trees <- 10

iter_atual <- 1
iter_total <- floor(total_trees / step_trees)

control <- trainControl(
  method = "cv",        # Validação cruzada clássica
  number = 5,           # 5 folds (ou ajuste se quiser mais/menos)
  allowParallel = TRUE, # usa todos os núcleos do cluster
  verboseIter = FALSE
)

for (nt in seq(1, total_trees, step_trees)) {
  set.seed(42)
  tic(paste("Treinando para", nt, " árvores!"))
  message("Iteração: ", iter_atual, "/", iter_total)
  
  model <- train(`vis(m)` ~ .,
                       data = df_para_treino,
                       method = "ranger",
                       trControl = control,
                       metric = "RMSE",
                       importance = "permutation",
                       # Define um número robusto de árvores
                       num.trees = nt)
  
  toc(log = TRUE, quiet = TRUE)
  log_list <- tic.log(format = FALSE)
  entry <- log_list[[length(log_list)]]
  runtime <- entry$toc - entry$tic
  tic.clearlog()
  
  best_idx <- which.min(model$results$RMSE)
  rmse_treino    <- model$results$RMSE[best_idx]
  rsq_treino     <- model$results$Rsquared[best_idx]
  mae_treino     <- model$results$MAE[best_idx]
  
  pred <- predict(model, newdata = df_para_teste)
  obs  <- df_para_teste$`vis(m)`
  
  perf_teste <- postResample(pred = pred, obs = obs)
  
  rmse_teste    <- perf_teste["RMSE"]
  rsq_teste     <- perf_teste["Rsquared"]
  mae_teste     <- perf_teste["MAE"]
  
  write.table(
    tibble(
      ntrees = nt,
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
    "nt = ", nt,
    " → RMSE treino = ", round(rmse_treino, 3),
    " RMSE teste = ", round(rmse_teste, 3),
    " R2 teste = ", round(rsq_teste, 3),
    " MAE teste = ", round(mae_teste, 3),
    " (", round(runtime, 2), " s)"
  )
  
  iter_atual <- iter_atual + 1
}
