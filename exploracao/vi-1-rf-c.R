# Modelo para RF-C XGBoost do relatório VI ----
# Especifiações:
#   - hrs: 1:24:1 - 24 iterações
# Total de modelos: 12
# ---- 1. SETUP ----
source("exploracao/libs.R")
library(dplyr)

if (!require("xgboost")) install.packages("xgboost", dependencies = TRUE)
library(xgboost)

# ---- 2. DIVISÃO DE CLASSES ----
df_original <- read_csv("exploracao/T0-dataset-extendido-transf.csv") %>% 
  mutate(tipo_vis = cut(vis, 
                        breaks = c(-1, 1000, 6000, 9999, 10000),
                        labels = c("nevoa",
                                   "visBaixa",
                                   "visMedia",
                                   "visCompleta"))
  ) %>% 
  select(-vis) %>% 
  mutate(clima = as.factor(clima))

# ---- 3. ARQUIVO ----
# Arquivo de saída
outfile <- "exploracao/arq-vi-1.csv"

tbl_out <- tibble(
  hrs = numeric(),
  acuracia_total_treino = numeric(),
  acuracia_total_teste = numeric(),
  bal_acc_nevoa = numeric(),
  bal_acc_visBaixa = numeric(),
  bal_acc_visMedia = numeric(),
  bal_acc_visCompleta = numeric(),
  runtime_sec = numeric()
)

# -- Cria arquivo caso não exista --
if (!file.exists(outfile)) {
  # - Criação do csv -
  write.csv(
    tbl_out,
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
      mutate(tipo_vis = lead(tipo_vis, hrs)) %>% 
      na.omit()
  }
  
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
  
  for (col in aus_em_treino) treino_x[, col] <- 0
  for (col in aus_em_teste)  teste_x[, col]  <- 0
  
  treino_x <- treino_x[, sort(colnames(treino_x))]
  teste_x  <- teste_x [, sort(colnames(teste_x))]
  
  # Transforma em matriz numérica pro XGBoost
  label_treino <- as.numeric(df_treino$tipo_vis) - 1
  label_teste  <- as.numeric(df_teste$tipo_vis)  - 1
  
  dtreino <- xgb.DMatrix(data = as.matrix(treino_x), label = label_treino)
  dteste  <- xgb.DMatrix(data = as.matrix(teste_x),  label = label_teste)
  

  # -- Setup dos parâmetros --
  params <- list(
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = length(levels(df_treino$tipo_vis)),
    eta = 0.08,
    max_depth = 5,
    subsample = 0.8,
    colsample_bytree = 0.8,
    nthread = 10
  )
  
  # -- Treino em si --
  modelo <- xgb.train(
    params = params,
    data = dtreino,
    nrounds = 1000,
    watchlist = list(treino = dtreino, teste = dteste),
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
  preds_prob <- predict(modelo, dtreino)
  preds_prob <- matrix(preds_prob,
                       ncol = params$num_class,
                       byrow = TRUE)
  pred_class_id <- max.col(preds_prob) - 1
  pred_factor <- factor(pred_class_id,
                        labels = levels(df_treino$tipo_vis))
  acc_treino <- sum(pred_factor == df_treino$tipo_vis) / length(df_treino$tipo_vis)
  
  # - Teste -
  preds_prob <- predict(modelo, dteste)
  preds_prob <- matrix(preds_prob,
                       ncol = params$num_class,
                       byrow = TRUE)
  pred_class_id <- max.col(preds_prob) - 1
  pred_factor <- factor(pred_class_id,
                        labels = levels(df_teste$tipo_vis))
  dim(preds_prob)
  acc_teste <- sum(pred_factor == df_teste$tipo_vis) / length(df_teste$tipo_vis)
  
  # Acurácia por classe
  conf_m <- confusionMatrix(pred_factor, df_teste$tipo_vis)
  
  ba <- conf_m$byClass[ , "Balanced Accuracy"]
  
  bal_acc_nevoa       <- ba[grep("nevoa", names(ba))]
  bal_acc_visBaixa    <- ba[grep("visBaixa", names(ba))]
  bal_acc_visMedia    <- ba[grep("visMedia", names(ba))]
  bal_acc_visCompleta <- ba[grep("visCompleta", names(ba))]
  
  # Append dos resultados
  write.table(
    tibble(hrs = hrs,
           acuracia_total_treino = acc_treino,
           acuracia_total_teste = acc_teste,
           bal_acc_nevoa = bal_acc_nevoa,
           bal_acc_visBaixa = bal_acc_visBaixa,
           bal_acc_visMedia = bal_acc_visMedia,
           bal_acc_visCompleta = bal_acc_visCompleta,
           runtime_sec = runtime),
    file = outfile,
    sep = ",",
    col.names = FALSE,
    append = TRUE,
    row.names = FALSE
  )
  
  # -- Mensagem final --
  message(" horas = ", hrs,
          " Acurácia de Treino = ", round(acc_treino, 3),
          " Acurácia de Teste = ", round(acc_teste, 3),
          " Acurácia de Névoa = ", round(bal_acc_nevoa, 3),
          " (", round(runtime, 2), "s)")
  
  runtime_total = runtime_total + runtime
}

# ---- 5. FINALIZAÇÃO ----
cat("Tempo total:", runtime_total)

