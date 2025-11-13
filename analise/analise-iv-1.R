# Análise do arquivo iv-1 ----
# Modelo de classificação 80/20 com reposição
## 0. Setup ----
library(tidyverse)
library(knitr)
library(kableExtra)
library(plotly)

# TODO: Mudar nome para ser só arq-iv-1
df <- read_csv("exploracao/arq-iv-1-ntrees-hrs.csv")

tempo_total <- df |> 
  select(runtime_sec) |> 
  summarise(tempo_total = sum(runtime_sec))

print(tempo_total / 3600)
# Com valores repetidos: tempo total de aproximadamente 72h

print(nrow(df))
# 623 modelos treinados

df <- df |> 
  slice(-(1:166))  # Valores repetidos

tempo_total <- df |> 
  select(runtime_sec) |> 
  summarise(tempo_total = sum(runtime_sec))

print(tempo_total / 3600)
# Tempo total de treino aproximadamente 56h (sem repetições)

view(df)

glimpse(df)

print(nrow(df))
# 457 modelos treinados

head(df)

## 1. Tempo de treino ----
### 1.1. Por clusters ----
tempo <- df |> 
  group_by(clusters) |> 
  summarise(tempo_medio = mean(runtime_sec),
            tempo_max = max(runtime_sec),
            tempo_min = min(runtime_sec),
            tempo_total = sum(runtime_sec))

view(tempo)

# Tabela
tempo |> 
  kbl(
    format = "latex",
    caption = "Tabela de tempo por número de clusters",
    label = "tab_iv_tempo_cluster",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_tempo_cluster.tex")

# Gráfico
tempo |> 
  pivot_longer(cols = tempo_medio:tempo_min,
               names_to = "tipo_tempo",
               values_to = "valor_tempo") |> 
  ggplot(aes(x = clusters,
             y = valor_tempo,
             linetype = tipo_tempo,
             color = tipo_tempo)) +
  geom_line() +
  geom_point()
# Análise
# Em geral, aumenta o tempo, mas há poucos dados
# Para se tirar uma conclusão boa

### 1.2. Por hrs ----
tempo <- df |> 
  group_by(hrs) |> 
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
    label = "tab_iv_tempo_hrs",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_tempo_hrs.tex")

# Gráfico
tempo |> 
  pivot_longer(cols = tempo_medio:tempo_min,
               names_to = "tipo_tempo",
               values_to = "valor_tempo") |> 
  ggplot(aes(x = hrs,
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
  slice_max(acuracia_total_teste) |> 
  view()

### 2.1. Por clusters ----
acc <- df |> 
  group_by(clusters) |> 
  summarise(acc_medio = mean(acuracia_total_teste),
            acc_max = max(acuracia_total_teste),
            acc_min = min(acuracia_total_teste))

view(acc)

# Tabela
acc |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por número de clusters",
    label = "tab_iv_acc_cluster",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_acc_cluster.tex")

# Gráfico
acc |> 
  pivot_longer(cols = acc_medio:acc_min,
               names_to = "tipo_acc",
               values_to = "valor_acc") |> 
  ggplot(aes(x = clusters,
             y = valor_acc,
             linetype = tipo_acc,
             color = tipo_acc)) +
  geom_line() +
  geom_point()
# Análise:
# Podemos ver que a diferença entre os mínimos
# e os máximos é consderável. Outra coisa que é notável
# é que a média está próxima da acurácia máxima.
# Dito isso, há um aumento de acuracia para a média e 
# para a acurácia mínima.

### 2.2. Por hrs ----
acc <- df |> 
  group_by(hrs) |> 
  summarise(acc_medio = mean(acuracia_total_teste),
            acc_max = max(acuracia_total_teste),
            acc_min = min(acuracia_total_teste))

view(acc)

# Tabela
acc |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por horas no futuro",
    label = "tab_iv_acc_hrs",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_acc_hrs.tex")

# Gráfico
acc |> 
  pivot_longer(cols = acc_medio:acc_min,
               names_to = "tipo_acc",
               values_to = "valor_acc") |> 
  ggplot(aes(x = hrs,
             y = valor_acc,
             linetype = tipo_acc,
             color = tipo_acc)) +
  geom_line() +
  geom_point()
# Análise:
# O valor para horas no futuro
# é basicamente constante para acurácia.
### 2.3. Por ntrees ----
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

### 3.1. Por clusters ----
bal_acc_nevoa <- df |> 
  group_by(clusters) |> 
  summarise(bal_acc_nevoa_medio = mean(bal_acc_nevoa),
            bal_acc_nevoa_max = max(bal_acc_nevoa),
            bal_acc_nevoa_min = min(bal_acc_nevoa))

view(bal_acc_nevoa)

# Tabela
bal_acc_nevoa |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por número de clusters",
    label = "tab_iv_bal_acc_nevoa_cluster",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_bal_acc_nevoa_cluster.tex")

# Gráfico
bal_acc_nevoa |> 
  pivot_longer(cols = bal_acc_nevoa_medio:bal_acc_nevoa_min,
               names_to = "tipo_bal_acc_nevoa",
               values_to = "valor_bal_acc_nevoa") |> 
  ggplot(aes(x = clusters,
             y = valor_bal_acc_nevoa,
             linetype = tipo_bal_acc_nevoa,
             color = tipo_bal_acc_nevoa)) +
  geom_line() +
  geom_point()
# Análise:
# Apesar dos poucos pontos de dados
# Aperanta haver uma aumento claro para
# número maiores de clusters
# Um estudo com mais cluster se demonstra necessário

### 3.2. Por hrs ----
bal_acc_nevoa <- df |> 
  group_by(hrs) |> 
  summarise(bal_acc_nevoa_medio = mean(bal_acc_nevoa),
            bal_acc_nevoa_max = max(bal_acc_nevoa),
            bal_acc_nevoa_min = min(bal_acc_nevoa))

view(bal_acc_nevoa)

# Tabela
bal_acc_nevoa |> 
  kbl(
    format = "latex",
    caption = "Tabela de acurácia por horas no futuro",
    label = "tab_iv_bal_acc_nevoa_hrs",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-IV-2025-10-31/res/tab_iv_bal_acc_nevoa_hrs.tex")

# Gráfico
bal_acc_nevoa |> 
  pivot_longer(cols = bal_acc_nevoa_medio:bal_acc_nevoa_min,
               names_to = "tipo_bal_acc_nevoa",
               values_to = "valor_bal_acc_nevoa") |> 
  ggplot(aes(x = hrs,
             y = valor_bal_acc_nevoa,
             linetype = tipo_bal_acc_nevoa,
             color = tipo_bal_acc_nevoa)) +
  geom_line() +
  geom_point()
# Análise:
# Não aparenta haver padrão para mais horas no futuro

### 3.3. Por ntrees ----
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
### 4.1. Por clusters ----
df |> 
  group_by(clusters) |> 
  summarise(media_acc_treino = mean(acuracia_total_treino),
            media_acc_teste = mean(acuracia_total_teste)) |> 
  pivot_longer(cols = c("media_acc_treino",
                        "media_acc_teste"),
              names_to = "tipo_acuracia_total",
              values_to = "valor_acuracia_total") |> 
  ggplot(aes(x = clusters,
             y = valor_acuracia_total,
             linetype = tipo_acuracia_total,
             color = tipo_acuracia_total)) +
  geom_line()
  # Análise:
# Cresce quase exponencial

### 4.2. Por hrs ----
df |> 
  group_by(hrs) |> 
  summarise(media_acc_treino = mean(acuracia_total_treino),
            media_acc_teste = mean(acuracia_total_teste)) |> 
  pivot_longer(cols = c("media_acc_treino","media_acc_teste"),
               names_to = "tipo_acuracia_total",
               values_to = "valor_acuracia_total") |> 
  ggplot(aes(x = hrs,
             y = valor_acuracia_total,
             linetype = tipo_acuracia_total,
             color = tipo_acuracia_total)) +
  geom_line()
# Análise: Queda por acurácia 
# para horas no futuro
# Mas, o padrão não é constante

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
plot_ly(df |> filter(clusters == 2),
        x = ~hrs,
        y = ~ntrees,
        z = ~acuracia_total_teste,
        type = "scatter3d",
        mode = "points",
        marker = list(size = 3))

plot_ly(df |> filter(clusters == 4),
        x = ~hrs,
        y = ~ntrees,
        z = ~acuracia_total_teste,
        type = "scatter3d",
        mode = "points",
        marker = list(size = 3))

plot_ly(df |> filter(clusters == 6),
        x = ~hrs,
        y = ~ntrees,
        z = ~acuracia_total_teste,
        type = "scatter3d",
        mode = "points",
        marker = list(size = 3))
