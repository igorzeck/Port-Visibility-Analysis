# Análise do arquivo vi-1 ----
# Modelo de classificação XGBoost
## 0. Setup ----
library(tidyverse)
library(knitr)
library(kableExtra)

path_res <- '.relatorios/relatorio-VI-2025-11-27/res'

df <- read_csv("exploracao/arq-vi-1.csv")

view(df)

tempo_total <- df |> 
  select(runtime_sec) |> 
  summarise(tempo_total = sum(runtime_sec))

seconds_to_period(tempo_total)
# Tempo total ≃ 1h 52min de treino

print(nrow(df))
# 60 modelos treinados

## 1. Tempo de treino ----
### 1.1. Por hrs ----
tempo <- df |> 
  group_by(hrs) |> 
  select(hrs, runtime_sec)

view(tempo)

# Tabela
tempo |> 
  kbl(
    format = "latex",
    caption = "Tabela de tempo por horas no futuro",
    label = "tempo_hrs",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(paste0(path_res,"/tempo_hrs.tex"))

# Gráfico
tempo |> 
  ggplot(aes(x = hrs,
             y = runtime_sec)) +
  geom_line() +
  geom_point()
# Análise:
# O modelo da hrs = 43 demorou muito mais (6 vezes mais)
# retirando ele:
tempo |> 
  filter(hrs != 43) |> 
  ggplot(aes(x = hrs,
             y = runtime_sec)) +
  geom_line() +
  geom_point()
# Análise:
# Padrão relativamente linear

## 2. Acurácia ----
### 2.1 Melhor modelo ----
tab_melhor <- df |> 
  slice_max(acuracia_total_teste) |> 
  pivot_longer(cols = everything(),
               names_to = "métrica",
               values_to = "valor")

view(tab_melhor)
# Análise:
# O melhor modelo tem uma acurácia de névoa maior que a acurácia
# de treino do modelo de Regressão do CNIT! Muito bom!
tab_melhor |> 
  kbl(
    format = "latex",
    caption = "Estatísticas do melhor modelo",
    label = "melhor_modelo",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(paste0(path_res,"/melhor_modelo.tex"))

### 2.2. Por hrs ----
acc <- df |> 
  select(hrs, acuracia_total_teste)

view(acc)

# Tabela
acc |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por horas no futuro",
    label = "acc_hrs",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(paste0(path_res,"/acc_hrs.tex"))

# Gráfico
acc |> 
  ggplot(aes(x = hrs,
             y = acuracia_total_teste)) +
  geom_line() +
  geom_point()

## 3. Acurácia por classe ----
# Maior acurácia da classe névoa
df |> 
  slice_max(bal_acc_nevoa) |> 
  view()

### 3.1. Por hrs ----
bal_acc_classes <- df |> 
  select(hrs, bal_acc_nevoa:bal_acc_visCompleta)

view(bal_acc_classes)

# Tabela
bal_acc_classes |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por horas no futuro",
    label = "bal_acc_classes_hrs",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(paste0(path_res,"/bal_acc_classes_hrs.tex"))

# Gráfico
bal_acc_classes |> 
  pivot_longer(cols = bal_acc_nevoa:bal_acc_visCompleta,
               names_to = "tipo_bal_acc",
               values_to = "valor_bal_acc") |> 
  ggplot(aes(x = hrs,
             y = valor_bal_acc,
             linetype = tipo_bal_acc,
             color = tipo_bal_acc)) +
  geom_line() +
  geom_point()
# Análise:
# Bem ruidoso para névoa, muito propensa a flutuações aleatórias.

## 4. Treino X Teste ----
### 4.1. Por hrs ----
df |> 
  group_by(hrs) |> 
  pivot_longer(cols = c("acuracia_total_treino","acuracia_total_teste"),
               names_to = "tipo_acuracia_total",
               values_to = "valor_acuracia_total") |> 
  ggplot(aes(x = hrs,
             y = valor_acuracia_total,
             linetype = tipo_acuracia_total,
             color = tipo_acuracia_total)) +
  geom_line()
# Análise: Ambas retas ficam equidistantes, sem sinal de aumento
# de sobreajuste!
