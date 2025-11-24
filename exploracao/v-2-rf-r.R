# Relatório V - Modelo RF com METARs extendidos ----
# Essa é a versão com as correçãos (aplicáveis) do arquivo v-1
# OBS: Modelo de regressão
# . Treinado com:
# - tamanho = (80/20)/20;
# - ntrees = variável (1:201:20) -> 11 iterações
# - horas = variável (1:18:2) -> 9 iterações
# - cv = 5
# . Informações armazenadas:
# - Acurácia balanceada;
# ---- 1. Setup ----
source("exploracao/libs.R")
library(lubridate)

# ---- 2. CARREGAMENTO E PRÉ-PROCESSAMENTO ----
df_original <- read_csv("exploracao/T0-dataset-extendido-transf.csv") %>% 
  mutate(clima = as.factor(clima))

# ---- 3. CONFIGURAÇÃO DE PARALELISMO ----
num_cores <- 5
cl <- makeCluster(num_cores)
registerDoParallel(cl)
cat(sprintf("\nProcessamento paralelo registrado para usar %d núcleos.\n", num_cores))

# ---- 4. CONTROLE DE TREINAMENTO ----
# arquivo dos resultados
outfile <- "exploracao/arq-v-2-corr.csv"

if (!file.exists(outfile)) {
  write.csv(
    tibble(
      ntrees = numeric(),
      horas = numeric(),
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
# -- kfolds --
min_kfolds <- 5
max_kfolds <- 5
step_kfolds <- 2

# -- horas --
min_hrs <- 1
max_hrs <- 18
step_hrs <- 2

# -- ntrees --
min_ntrees <- 1
max_ntrees <- 201
step_ntrees <- 20

iter_atual <- 1
iter_total <- (floor((max_ntrees - min_ntrees) / step_ntrees)) *
  (floor((max_hrs - min_hrs) / step_hrs))

control <- trainControl(
  method = "cv",        # Validação cruzada clássica
  number = 5,           # 5 folds
  allowParallel = TRUE, # usa todos os núcleos do cluster
  verboseIter = FALSE
)

## Treino ----
runtime_total <- 0

for (hrs in seq(min_hrs, max_hrs, step_hrs)) {
  df <- df_original
  if (hrs != 1) {
    # -- Lead das horas --
    df <- df %>% 
      mutate(vis = lead(vis, hrs)) %>% 
      na.omit()
  }
  
  # -- Divisão do dataset --
  set.seed(42)
  # -- Split externo 80/20 --
  tam_amostra <- floor(0.8 * nrow(df))
  indices_treino <- sample(seq_len(nrow(df)), size = tam_amostra)
  
  df_treino <- df[indices_treino, ]
  df_teste <- df[-indices_treino, ]
  for (nt in seq(min_ntrees, max_ntrees, step_ntrees)) {
    set.seed(42)
    tic(paste("Treinando para", nt, " árvores!"))
    message("Iteração: ", iter_atual, "/", iter_total)
    
    model <- train(vis ~ .,
                   data = df_treino,
                   method = "ranger",
                   trControl = control,
                   metric = "RMSE",
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
    
    pred <- predict(model, newdata = df_teste)
    obs  <- df_teste$vis
    
    perf_teste <- postResample(pred = pred, obs = obs)
    
    rmse_teste    <- perf_teste["RMSE"]
    rsq_teste     <- perf_teste["Rsquared"]
    mae_teste     <- perf_teste["MAE"]
    
    write.table(
      tibble(
        ntrees = nt,
        horas = hrs,
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
    
    runtime_total <- runtime_total + runtime
    
    message(
      "nt = ", nt,
      " hrs = ", hrs,
      " → RMSE treino = ", round(rmse_treino, 3),
      " RMSE teste = ", round(rmse_teste, 3),
      " R2 teste = ", round(rsq_teste, 3),
      " MAE teste = ", round(mae_teste, 3),
      " (", round(runtime, 2), " s)"
    )
    
    iter_atual <- iter_atual + 1
  }
}

## Finalização ----
cat("Tempo:", runtime_total)

# Observação:
# 22h
