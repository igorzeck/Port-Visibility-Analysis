  # Relatório IV - Modelo RF de Classificação ----
# . Treinado com:
# - tamanho = (80/20)/20;
# - ntrees = variável (1:200:20) -> 10 iterações
# - horas = variável (1:24:2) -> 12 iterações
# - amostragem = com reposição -> 1 iteração
# - kflolds = variável (2:6:2) -> 3 iterações
# - classes = 4 classes -> 1 iteração
#   - nevoa: [0 - 2e3m];
#   - visBaixa: ]2e3m - 6e3m];
#   - visMedia: ]6e3m - 1e4m[;
#   - VisCompleta: [10e4m];
# Total: 10 * 12 * 1 * 3 = 360 iterações
# . Informações armazenadas:
# - Acurácia balanceada;
# ---- 1. SETUP ----
source("exploracao/libs.R")
library(tidyverse)

# ---- 2. DIVISÃO DE CLASSES ----
df <- read_csv("exploracao/dataset-j0-definitivo-transf.csv") %>% 
  mutate(tipo_vis = cut(`vis(m)`, 
                        breaks = c(-1, 2000, 6000, 9999, 10000),
                        labels = c("nevoa",
                                   "visBaixa",
                                   "visMedia",
                                   "visCompleta"))
         ) %>% 
  janitor::clean_names() %>% 
  select(-vis_m)

# ---- 3. CONFIGURAÇÃO DE PARALELISMO ----
num_cores <- 5
cl <- makeCluster(num_cores)  # Levando em conta existência de núcleos lógicos
registerDoParallel(cl)

cat(sprintf("\nProcessamento paralelo registrado para usar %d núcleos (físicos).\n", num_cores))

# Arquivo dos resultados
outfile <- "arq-iv-1-ntrees-hrs.csv"

# -- Tibble de exportação --
tbl_out <- tibble(
  kfolds = numeric(),
  hrs = numeric(),
  ntrees = numeric(),
  acuracia_total_treino = numeric(),
  acuracia_total_teste = numeric(),
  imp_vis = numeric(),
  bal_acc_nevoa = numeric(),
  bal_acc_visBaixa = numeric(),
  bal_acc_visMedia = numeric(),
  bal_acc_visCompleta = numeric(),
  runtime_sec = numeric()
)
# - Adição de colunas de importância -
# TODO: Implementar isso?
# tbl_imp_cols <- df %>% 
#   select(-datetime) %>% 
#   slice(0) %>%
#   rename_with(~ paste0("imp_", .x))
# 
# tbl_out <- tbl_out %>%
#   bind_cols(tbl_imp_cols) %>% 
#   mutate(imp_tipo_vis = as.numeric(imp_tipo_vis))
# 
# imp_tbl <- imp$importance %>%
#   tibble::rownames_to_column(var = "variable") %>%
#   as_tibble() 

# -- Cria arquivo caso não exista --
if (!file.exists(outfile)) {
  # - Criação do csv -
  write.csv(
    tbl_out,
    outfile,
    row.names = FALSE
  )
}

# ---- 5. SETUP ----
# -- kfolds --
min_kfolds <- 2
# max_kfolds <- 8
max_kfolds <- 6
step_kfolds <- 2

# -- horas --
min_hrs <- 1
max_hrs <- 24
step_hrs <- 2

# -- ntrees --
min_ntrees <- 1
max_ntrees <- 200
step_ntrees <- 20

iter_atual <- 1
# Havia parado no 167 sem o paralelismo, a fim de consertar isso 
# Usou-se o código abaixo
# iter_atual <- 167
# Útil para ver o impacto relativo do paralelismo

iter_total <- (floor((max_ntrees - min_ntrees) / step_ntrees) + 1) *
  (floor((max_hrs - min_hrs) / step_hrs) + 1) *
  (floor((max_kfolds - min_kfolds) / step_kfolds) + 1)

runtime_total <- 0
df %>% 
  select(tipo_vis, vis_m_lag_1) %>% 
  filter(is.na(tipo_vis))
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
    if (hrs != 1) {
      # -- Lag das horas --
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
                     importance = "permutation",
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
      
      bal_acc_nevoa <- conf_m$byClass[, "Balanced Accuracy"]["Class: nevoa"]
      bal_acc_visBaixa <- conf_m$byClass[, "Balanced Accuracy"]["Class: visBaixa"]
      bal_acc_visMedia <- conf_m$byClass[, "Balanced Accuracy"]["Class: visMedia"]
      bal_acc_visCompleta <- conf_m$byClass[, "Balanced Accuracy"]["Class: visCompleta"]
      
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
