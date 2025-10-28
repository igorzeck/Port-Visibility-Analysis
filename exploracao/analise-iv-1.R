# Análise do arquivo iv-1 ----
# Modelo de classificação 80/20 com reposição
## 0. Setup ----
library(tidyverse)
library(knitr)
library(kableExtra)
library(plotly)

# TODO: Mudar nome para ser só arq-iv-1
df <- read_csv("exploracao/arq-iv-1-ntrees-hrs.csv")

glimpse(df)

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

### 1.4 Por tudo ----
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
