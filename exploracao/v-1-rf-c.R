# Relatório V - Modelo RF com METARs extendidos ----
# Foram também corrigidos os seguintes erros:
# 1. O 'lead' era aplicado de forma cumulativa
# 2. A extração da acurácia balanceada é mais robusta
# 3. Retirou-se a 'importance' já que não é analisada
# OBS: Mais variáveis dos METARS
# . Treinado com:
# - tamanho = (80/20)/20;
# - ntrees = variável (1:200:20) -> 10 iterações
# - horas = variável (1:24:2) -> 12 iterações
# - kflolds = variável (8) -> 1 iterações
# - classes = 4 classes -> 1 iteração
#   - nevoa: [0 - 1e3m]; -> névoa restrita
#   - visBaixa: ]1e3m - 6e3m];
#   - visMedia: ]6e3m - 10e4m[;
#   - VisCompleta: [10e4m];
# Total: 10 * 12 * 1 = 120 iterações
# . Informações armazenadas:
# - Acurácia balanceada;
# ---- 1. SETUP ----
source("exploracao/libs.R")
library(tidyverse)

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
# ---- 3. CONFIGURAÇÃO DE PARALELISMO ----
num_cores <- 5
cl <- makeCluster(num_cores)  # Levando em conta existência de núcleos lógicos
registerDoParallel(cl)

cat(sprintf("\nProcessamento paralelo registrado para usar %d núcleos (físicos).\n", num_cores))

# Arquivo dos resultados (corrigido)
outfile <- "exploracao/arq-v-1-corr.csv"

# -- Tibble de exportação --
tbl_out <- tibble(
  kfolds = numeric(),
  hrs = numeric(),
  ntrees = numeric(),
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

# ---- 5. SETUP ----
# -- kfolds --
min_kfolds <- 5
max_kfolds <- 5
step_kfolds <- 2

# -- horas --
min_hrs <- 1
max_hrs <- 24
step_hrs <- 4

# -- ntrees --
min_ntrees <- 1
max_ntrees <- 201
step_ntrees <- 20

iter_atual <- 1

iter_total <- (floor((max_ntrees - min_ntrees) / step_ntrees) + 1) *
  (floor((max_hrs - min_hrs) / step_hrs) + 1) *
  (floor((max_kfolds - min_kfolds) / step_kfolds) + 1)

runtime_total <- 0

# ---- 6. TREINO ----
for (num_kfold in seq(min_kfolds, max_kfolds, step_kfolds)) {
  # -- Controle de treinamento --
  control <- trainControl(
    method = "cv",        # Validação cruzada clássica
    number = num_kfold,
    allowParallel = TRUE,
    verboseIter = FALSE
  )
  for (hrs in seq(min_hrs, max_hrs, step_hrs)) {
    df <- df_original
    if (hrs != 1) {
      # -- Lead das horas --
      df <- df %>% 
        mutate(tipo_vis = lead(tipo_vis, hrs)) %>% 
        na.omit()
    }
    
    # -- Divisão do dataset --
    set.seed(42)
    # -- Split externo 80/20 --
    tam_amostra <- floor(0.8 * nrow(df))
    indices_treino <- sample(seq_len(nrow(df)), size = tam_amostra)
    
    df_treino <- df[indices_treino, ]
    df_teste <- df[-indices_treino, ]
    
    # -- Número de árvores --
    for (nt in seq(min_ntrees, max_ntrees, step_ntrees)) {
      set.seed(42)
      tic(paste("Treinando para", nt, " árvores!"))
      message("Iteração: ", iter_atual, "/", iter_total)
      
      # -- Treino --
      model <- train(tipo_vis ~ .,
                     data = df_treino,
                     method = "ranger",
                     trControl = control,
                     metric = "Accuracy",
                     num.trees = nt)
      
      # -- Log --
      toc(log = TRUE, quiet = TRUE)
      log_list <- tic.log(format = FALSE)
      entry <- log_list[[length(log_list)]]
      runtime <- entry$toc - entry$tic
      tic.clearlog()
      
      # -- Acurácia --
      acc <- max(model$results$Accuracy)
      
      # Acurácia de teste
      pred <- predict(model, df_teste)
      
      acc_teste <- sum(pred == df_teste$tipo_vis) / length(df_teste$tipo_vis)
      
      # Acurácia por classe em teste
      conf_m <- confusionMatrix(data=pred, reference = df_teste$tipo_vis)
      
      # bal_acc_nevoa <- conf_m$byClass[, "Balanced Accuracy"]["Class: nevoa"]
      # bal_acc_visBaixa <- conf_m$byClass[, "Balanced Accuracy"]["Class: visBaixa"]
      # bal_acc_visMedia <- conf_m$byClass[, "Balanced Accuracy"]["Class: visMedia"]
      # bal_acc_visCompleta <- conf_m$byClass[, "Balanced Accuracy"]["Class: visCompleta"]
      
      ba <- conf_m$byClass[ , "Balanced Accuracy"]
      
      bal_acc_nevoa       <- ba[grep("nevoa", names(ba))]
      bal_acc_visBaixa    <- ba[grep("visBaixa", names(ba))]
      bal_acc_visMedia    <- ba[grep("visMedia", names(ba))]
      bal_acc_visCompleta <- ba[grep("visCompleta", names(ba))]
      
      # Append dos resultados
      write.table(
        tibble(kfolds = num_kfold,
               hrs = hrs,
               ntrees = nt,
               acuracia_total_treino = acc,
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
      
      
      message("kfolds = ", num_kfold,
              " horas = ", hrs,
              " ntrees = ", nt,
              " finalizada → Acurácia = ", round(acc, 3),
              " Acurácia de Teste = ", round(acc_teste, 3),
              " Acurácia de Névoa = ", round(bal_acc_nevoa, 3),
              " (", round(runtime, 2), "s)")
      
      runtime_total = runtime_total + runtime
      iter_atual = iter_atual + 1;
    }
  }
}
# ---- 7. FINALIZAÇÃO ----
stopCluster(cl)

registerDoSEQ()
cat("Cluster paralelo finalizado.\n\n")

cat("Tempo:", runtime_total)
