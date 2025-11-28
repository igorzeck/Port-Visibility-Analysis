# Análise do arquivo vi-1 ----
# Modelo de classificação XGBoost
## 0. Setup ----
library(tidyverse)
library(knitr)
library(kableExtra)

path_res <- '.relatorios/relatorio-VI-2025-11-27/res'

df <- read_csv("exploracao/arq-vi-2.csv")

view(df)

tempo_total <- df |> 
  select(runtime_sec) |> 
  summarise(tempo_total = sum(runtime_sec))

seconds_to_period(tempo_total)
# Tempo total ≃ 48min de treino

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
# Uma ligeira tendência a ser decrescente, mas ruidoso demais
# para concluir se é ou não significativo

## 2. R² ----
### 2.1 Melhor modelo ----
tab_melhor <- df |> 
  slice_max(Rsquared_teste) |> 
  pivot_longer(cols = everything(),
               names_to = "métrica",
               values_to = "valor")

view(tab_melhor)
# Análise:
# O melhor modelo um R² de treino muito bom (70%), mas deixa
# a desejar na acurácia de teste.
tab_melhor |> 
  kbl(
    format = "latex",
    caption = "Estatísticas do melhor modelo",
    label = "melhor-modelo-vi2",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(paste0(path_res,"/melhor-modelo-vi2.tex"))

### 2.2. Por hrs ----
acc <- df |> 
  select(hrs, Rsquared_teste)

view(acc)

# Tabela
acc |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por horas no futuro",
    label = "acc_hrs_vi2",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(paste0(path_res,"/acc_hrs_vi2.tex"))

# Gráfico
acc |> 
  ggplot(aes(x = hrs,
             y = Rsquared_teste)) +
  geom_line() +
  geom_point()

## 3. Treino X Teste ----
### 3.1. Por hrs ----
df |> 
  group_by(hrs) |> 
  pivot_longer(cols = c("Rsquared_treino","Rsquared_teste"),
               names_to = "tipo_Rsquared",
               values_to = "valor_Rsquared") |> 
  ggplot(aes(x = hrs,
             y = valor_Rsquared,
             linetype = tipo_Rsquared,
             color = tipo_Rsquared)) +
  geom_line()
# Análise: Ambas retas ficam equidistantes, sem sinal de aumento
# de sobreajuste!
