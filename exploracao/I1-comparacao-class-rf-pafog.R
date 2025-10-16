# Arquivo de comparação do modelo Random Forest com o Pafog ----
# Este arquivo contém as comparações
# do modelo Pafog com um modelo de classificação Random Forest
# Setup ----
modelo <- readRDS("c-G2-modelo.rds")

acc_train <- max(model$results$Accuracy)

pred <- predict(model, df_test)
acc_test <- mean(pred == df_test$tipo_vis)

# Info ----

conf_m <- confusionMatrix(data = pred, reference = df_test$tipo_vis)
acc_nevoa   <- conf_m$byClass["Class: Nevoa", "Balanced Accuracy"]
acc_neblina <- conf_m$byClass["Class: Neblina", "Balanced Accuracy"]
acc_visivel <- conf_m$byClass["Class: Visivel", "Balanced Accuracy"]


message(" → Acurácia treino = ", round(acc_train, 3),
        " | teste = ", round(acc_test, 3),
        " | Nevoa = ", round(acc_nevoa, 3),
        " | Neblina = ", round(acc_neblina, 3),
        " | Visivel = ", round(acc_visivel, 3),
        " (", round(runtime, 2), "s)")

print(model)
print(conf_m)
print(model$results)

# Cálculo do Skill Score do modelo ----
skill_score <- function(f, d, t) {
  (f - d) / (t - d)
}

d_acaso <- function(c1, l1, c2, l2, t) {
  ((c1 * l1) + (c2 * l2)) / t
}

## Teste ----
d_teste <- 57.91
sk_score_teste <- skill_score(70, d_teste, 73)
print(sk_score_teste)  # OK

## Do nosso modelo ----
# Tabela
conf_m$table

# Coeficientes
c1 <- 37   +    9    +   1
c2 <- 159  + 7723   + 4662 + 58  +  7047  + 65696
l1 <- 37 + 159 + 58
l2 <- 9 + 7723 + 7047 + 1 + 4662 + 65696
t <- l1 + l2
(c1 + c2) == t

f <- 37 + 7723 + 65696

d <- d_acaso(c1, l1, c2, l2, t)
sk_score <- skill_score(f, d, t)
print(d)
print(sk_score)

# Hm...