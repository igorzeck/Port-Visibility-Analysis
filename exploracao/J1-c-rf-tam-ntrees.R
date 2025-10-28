# Teste para tamanho E ntrees variável ----
# Teste com modelo binário
# ---- 1. Setup ----
source("libs.R")
library(lubridate)

# ---- 2. CARREGAMENTO E PRÉ-PROCESSAMENTO ----
df <- read_csv("dataset-j0-definitivo-transf.csv") %>% 
  mutate(tipo_vis = cut(`vis(m)`, 
                        breaks = c(-Inf, 2000, Inf),
                        labels = c("Nevoa", "Visivel"))) %>% 
  select(-`vis(m)`)

# ---- 3. CONFIGURAÇÃO DE PARALELISMO ----
num_cores <- 5
cl <- makeCluster(num_cores)
registerDoParallel(cl)
cat(sprintf("\nProcessamento paralelo registrado para usar %d núcleos.\n", num_cores))

# ---- 4. CONTROLE DE TREINAMENTO ----
# arquivo dos resultados
outfile <- "arq-j1-tam-ntrees.csv"
# Cria arquivo caso não exista
if (!file.exists(outfile)) {
  write.csv(
    tibble(porcentagem = numeric(),
           horas = numeric(),
           ntrees = numeric(),
           acuracia_total_treino = numeric(),
           acuracia_total_teste = numeric(),
           acuracia_nevoa = numeric(),
           acuracia_visivel = numeric(),
           sens_nevoa = numeric(),
           sens_visivel = numeric(),
           spec_nevoa = numeric(),
           spec_visivel = numeric(),
           ppv_nevoa = numeric(),
           ppv_visivel = numeric(),
           npv_nevoa = numeric(),
           npv_visivel = numeric(),
           bal_acc_nevoa = numeric(),
           bal_acc_visivel = numeric(),
           runtime_sec = numeric()),
    outfile,
    row.names = FALSE
  )
}

control <- trainControl(
  method = "cv",        # Validação cruzada clássica
  number = 5,           # 5 folds (ou ajuste se quiser mais/menos)
  allowParallel = TRUE, # usa todos os núcleos do cluster
  verboseIter = FALSE
)

# ---- 5. PORCENTAGEM MÍNIMA ----
# Ao menos 1 ano
ano_periodo <- year(df$datetime[nrow(df) - 1]) - year(df$datetime[1])
min_percent <- ceiling(nrow(df) / (nrow(df) / ano_periodo)) / 100
# Um min_percent melhor seria 1 / ano_periodo!
max_percent <- 1 - min_percent
step_percent <- 0.1

total_trees <- 201
step_trees <- 20

total_horas <- 24
step_horas <- 3

iter_atual <- 1
iter_total <- floor((max_percent - min_percent) / step_percent * total_trees / step_trees * total_horas / step_horas)
length(seq(1, 24, 3))
floor(25 / 3)
# ---- 6. TREINO ----
for (percent in c(seq(min_percent,max_percent, step_percent))) {
  tic(paste("Treinando com", percent, "% do dataset para treino!"))
  
  # -- Horas no futuro --
  for (horas in seq(1, total_horas, step_horas)) {
    if (horas != 1) {
      # -- Divisão do dataset --
      df <- df %>% 
          mutate(tipo_vis = lead(tipo_vis, horas)) %>% 
          na.omit()
    }
    
    # -- Divisão do dataset --
    cutoff <- floor(percent * nrow(df))
    df_para_treino <- df[1:cutoff, ]
    df_para_teste  <- df[(cutoff + 1):nrow(df), ]
  
  
    # -- Número de árvores --
    for (nt in seq(1,201,20)) {
      set.seed(42)
      tic(paste("Treinando para", nt, " árvores!"))
      message("Iteração: ", iter_atual, "/", iter_total)
      
      model <- train(tipo_vis ~ .,
                     data = df_para_treino,
                     method = "ranger",
                     trControl = control,
                     metric = "Accuracy",
                     importance = "permutation",
                     num.trees = nt)
      
      toc(log = TRUE, quiet = TRUE)
      
      log_list <- tic.log(format = FALSE)
      
      entry <- log_list[[length(log_list)]]
      runtime <- entry$toc - entry$tic
      
      tic.clearlog()
      
      acc <- max(model$results$Accuracy)
      
      # Acurácia de teste
      pred <- predict(model, df_para_teste)
      
      acc_teste <- sum(pred == df_para_teste$tipo_vis) / length(df_para_teste$tipo_vis)
      
      # Acurácia por classe em teste
      # Usa acurácia balanceada para levar em conta o tamanho relativo das amostras
      conf_m <- confusionMatrix(data=pred,
                                reference = df_para_teste$tipo_vis,
                                positive = "Nevoa")
      
      # Métricas para a classe positiva ("Nevoa")
      sens_nevoa <- conf_m$byClass["Sensitivity"]
      spec_nevoa <- conf_m$byClass["Specificity"]
      ppv_nevoa <- conf_m$byClass["Pos Pred Value"]
      npv_nevoa <- conf_m$byClass["Neg Pred Value"]
      bal_acc_nevoa <- conf_m$byClass["Balanced Accuracy"]
      
      # Métricas para a classe negativa ("Visivel")
      sens_visivel <- conf_m$byClass["Specificity"]  # Sensibilidade da classe negativa
      spec_visivel <- conf_m$byClass["Sensitivity"]  # Especificidade da classe negativa
      ppv_visivel <- conf_m$byClass["Neg Pred Value"]  # Valor preditivo positivo da classe negativa
      npv_visivel <- conf_m$byClass["Pos Pred Value"]  # Valor preditivo negativo da classe negativa
      bal_acc_visivel <- bal_acc_nevoa  # Acurácia balanceada é a mesma para ambas as classes
      
      # Append dos resultados
      write.table(
        tibble(porcentagem = percent,
               horas = horas,
               ntrees = nt,
               acuracia_total_treino = acc,
               acuracia_total_teste = acc_teste,
               acuracia_nevoa = sens_nevoa,
               acuracia_visivel = spec_visivel,
               sens_nevoa = sens_nevoa,
               sens_visivel = sens_visivel,
               spec_nevoa = spec_nevoa,
               spec_visivel = spec_visivel,
               ppv_nevoa = ppv_nevoa,
               ppv_visivel = ppv_visivel,
               npv_nevoa = npv_nevoa,
               npv_visivel = npv_visivel,
               bal_acc_nevoa = bal_acc_nevoa,
               bal_acc_visivel = bal_acc_visivel,
               runtime_sec = runtime),
        file = outfile,
        sep = ",",
        col.names = FALSE,
        append = TRUE,
        row.names = FALSE
      )
      
      
      message("Porcentagem para treino = ", percent, 
              " horas = ", horas,
              " ntrees = ", nt,
              " finalizada → Acurácia = ", round(acc, 3),
              " Acurácia de Teste = ", round(acc_teste, 3), 
              " Acurácia para Nevoa = ", round(sens_nevoa, 3), 
              " Acurácia para Visivel = ", round(spec_visivel, 3), 
              " (", round(runtime, 2), "s)")
      
      iter_atual = iter_atual + 1;
    }
  }
}

varImp(model)
# ---- 7. FINALIZAÇÃO ----
stopCluster(cl)
registerDoSEQ()
cat("Cluster paralelo finalizado.\n\n")