# Análise do arquivo v-2-corr ----
# Modelo de regressão 80/20 com reposição
## 0. Setup ----
library(tidyverse)
library(knitr)
library(kableExtra)
library(plotly)

df <- read_csv("exploracao/arq-v-2-corr.csv")

view(df)

tempo_total <- df |> 
  select(runtime_sec) |> 
  summarise(tempo_total = sum(runtime_sec))

print(tempo_total / 3600)
# Tempo total de aproximadamente 13h

print(nrow(df))
# 120 modelos treinados

# Tempo total de treino aproximadamente 56h (sem repetições)

head(df)

## 1. Tempo de treino ----
### 1.1. Por horas ----
tempo <- df |> 
  group_by(horas) |> 
  summarise(tempo_medio = mean(runtime_sec),
            tempo_max = max(runtime_sec),
            tempo_min = min(runtime_sec),
            tempo_total = sum(runtime_sec))

view(tempo)

# Tabela
tempo |> 
  kbl(
    format = "latex",
    caption = "Tabela de tempo por horas no futuro",
    label = "tab_iv_tempo_horas",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_tempo_horas.tex")

# Gráfico
tempo |> 
  pivot_longer(cols = tempo_medio:tempo_min,
               names_to = "tipo_tempo",
               values_to = "valor_tempo") |> 
  ggplot(aes(x = horas,
             y = valor_tempo,
             linetype = tipo_tempo,
             color = tipo_tempo)) +
  geom_line() +
  geom_point()
# Análise:
# Parece haver "steps" e de resto se mantém costantne,
# Mas, com uma tendência de decréscimo de tempo

### 1.3. Por ntrees ----
tempo <- df |> 
  group_by(ntrees) |> 
  summarise(tempo_medio = mean(runtime_sec),
            tempo_max = max(runtime_sec),
            tempo_min = min(runtime_sec),
            tempo_total = sum(runtime_sec))

view(tempo)

# Tabela
tempo |> 
  kbl(
    format = "latex",
    caption = "Tabela de tempo por número de árvores",
    label = "tab_iv_tempo_ntrees",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_tempo_ntrees.tex")

# Gráfico
tempo |> 
  pivot_longer(cols = tempo_medio:tempo_min,
               names_to = "tipo_tempo",
               values_to = "valor_tempo") |> 
  ggplot(aes(x = ntrees,
             y = valor_tempo,
             linetype = tipo_tempo,
             color = tipo_tempo)) +
  geom_line() +
  geom_point()
# Análise:
# Crescimento perfeitamente linear!

## 2. Acurácia de teste ----
df |> 
  slice_max(Rsquared_teste) |> 
  view()

### 2.1. Por horas ----
acc <- df |> 
  group_by(horas) |> 
  summarise(acc_medio = mean(acuracia_total_teste),
            acc_max = max(acuracia_total_teste),
            acc_min = min(acuracia_total_teste))

view(acc)

# Tabela
acc |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por horas no futuro",
    label = "tab_iv_acc_horas",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_acc_horas.tex")

# Gráfico
acc |> 
  pivot_longer(cols = acc_medio:acc_min,
               names_to = "tipo_acc",
               values_to = "valor_acc") |> 
  ggplot(aes(x = horas,
             y = valor_acc,
             linetype = tipo_acc,
             color = tipo_acc)) +
  geom_line() +
  geom_point()
# Análise:
# O valor para horas no futuro
# é basicamente constante para acurácia.
### 2.2. Por ntrees ----
acc <- df |> 
  group_by(ntrees) |> 
  summarise(acc_medio = mean(acuracia_total_teste),
            acc_max = max(acuracia_total_teste),
            acc_min = min(acuracia_total_teste))

view(acc)

# Tabela
acc |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por horas no futuro",
    label = "tab_iv_acc_ntrees",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_acc_ntrees.tex")

# Gráfico
acc |> 
  slice(-1:-2) |> 
  pivot_longer(cols = acc_medio:acc_min,
               names_to = "tipo_acc",
               values_to = "valor_acc") |> 
  ggplot(aes(x = ntrees,
             y = valor_acc,
             linetype = tipo_acc,
             color = tipo_acc)) +
  geom_line() +
  geom_point()
# Análise:
# O valor de acurácia, exceto para o primeiro ponto
# se mantém basicamente constante.

## 3. Acurácia para névoa ----
df |> 
  slice_max(bal_acc_nevoa)

### 3.1. Por horas ----
bal_acc_nevoa <- df |> 
  group_by(horas) |> 
  summarise(bal_acc_nevoa_medio = mean(bal_acc_nevoa),
            bal_acc_nevoa_max = max(bal_acc_nevoa),
            bal_acc_nevoa_min = min(bal_acc_nevoa))

view(bal_acc_nevoa)

# Tabela
bal_acc_nevoa |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por horas no futuro",
    label = "tab_iv_bal_acc_nevoa_horas",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_bal_acc_nevoa_horas.tex")

# Gráfico
bal_acc_nevoa |> 
  pivot_longer(cols = bal_acc_nevoa_medio:bal_acc_nevoa_min,
               names_to = "tipo_bal_acc_nevoa",
               values_to = "valor_bal_acc_nevoa") |> 
  ggplot(aes(x = horas,
             y = valor_bal_acc_nevoa,
             linetype = tipo_bal_acc_nevoa,
             color = tipo_bal_acc_nevoa)) +
  geom_line() +
  geom_point()
# Análise:
# Não aparenta haver padrão para mais horas no futuro

### 3.2. Por ntrees ----
bal_acc_nevoa <- df |> 
  group_by(ntrees) |> 
  summarise(bal_acc_nevoa_medio = mean(bal_acc_nevoa),
            bal_acc_nevoa_max = max(bal_acc_nevoa),
            bal_acc_nevoa_min = min(bal_acc_nevoa))

view(bal_acc_nevoa)

# Tabela
bal_acc_nevoa |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por horas no futuro",
    label = "tab_iv_bal_acc_nevoa_ntrees",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_bal_acc_nevoa_ntrees.tex")

# Gráfico
bal_acc_nevoa |> 
  pivot_longer(cols = bal_acc_nevoa_medio:bal_acc_nevoa_min,
               names_to = "tipo_bal_acc_nevoa",
               values_to = "valor_bal_acc_nevoa") |> 
  ggplot(aes(x = ntrees,
             y = valor_bal_acc_nevoa,
             linetype = tipo_bal_acc_nevoa,
             color = tipo_bal_acc_nevoa)) +
  geom_line() +
  geom_point()
# Análise:
# Sem padrão parao número de árvores.

## 4. Treino X Teste ----
### 4.1. Por horas ----
df |> 
  group_by(horas) |> 
  summarise(media_acc_treino = mean(acuracia_total_treino),
            media_acc_teste = mean(acuracia_total_teste)) |> 
  pivot_longer(cols = c("media_acc_treino","media_acc_teste"),
               names_to = "tipo_acuracia_total",
               values_to = "valor_acuracia_total") |> 
  ggplot(aes(x = horas,
             y = valor_acuracia_total,
             linetype = tipo_acuracia_total,
             color = tipo_acuracia_total)) +
  geom_line()
# Análise: Queda por acurácia 
# para horas no futuro
# Mas, o padrão não é constante

df |> 
  group_by(horas) |> 
  summarise(max_acc_treino = max(acuracia_total_treino),
            max_acc_teste = max(acuracia_total_teste)) |> 
  pivot_longer(cols = c("max_acc_treino","max_acc_teste"),
               names_to = "tipo_acuracia_total",
               values_to = "valor_acuracia_total") |> 
  ggplot(aes(x = horas,
             y = valor_acuracia_total,
             linetype = tipo_acuracia_total,
             color = tipo_acuracia_total)) +
  geom_line()
# O padrão é o mesmo para os máximos

### 4.3. Por ntrees ----
df |> 
  group_by(ntrees) |> 
  summarise(media_acc_treino = mean(acuracia_total_treino),
            media_acc_teste = mean(acuracia_total_teste)) |> 
  pivot_longer(cols = c("media_acc_treino","media_acc_teste"),
               names_to = "tipo_acuracia_total",
               values_to = "valor_acuracia_total") |> 
  ggplot(aes(x = ntrees,
             y = valor_acuracia_total,
             linetype = tipo_acuracia_total,
             color = tipo_acuracia_total)) +
  geom_line()
# Análise: PRAticamente constante

## 5. Por tudo ----
plot_ly(df,
        x = ~horas,
        y = ~ntrees,
        z = ~acuracia_total_teste,
        type = "scatter3d",
        mode = "points",
        marker = list(size = 3))
