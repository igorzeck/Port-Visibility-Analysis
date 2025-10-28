# Análise dos modelos treinados ----
# Dataset de regressão dividido meio a meio
# Com lag de visibilidade
# 0. Setup ----
# install.packages(c("knitr", "kableExtra"))
library(tidyverse)
library(knitr)
library(kableExtra)

## 1. Arquivo Tamanho de árvores amostradas sequencialmente ----
# e com lag de visibilidade
df_j4 <- read_csv("exploracao/arq-j4-tam-ntrees.csv")

str(df_j4)
view(head(df_j4))

# Melhor resultado:
df_j4 |> 
  filter(Rsquared_teste == max(Rsquared_teste, na.rm = TRUE)) |> 
  view()
# Análise:
# Resultados ruins, mas que fazem sentido!
# Acurácia de 0.57...
# Prova que havia intoxicação de dados!

# Análse:
# Todos sem grandes variações. Contudo, grande acerto
# Para névoa!

### 1.1. Tabela de resultados
df_j4 |>
  arrange(desc(Rsquared_teste)) |>
  head() |> 
  view()

df_j4 |>
  slice_max(order_by = Rsquared_teste, n = 10, with_ties = TRUE) |>
  select(ntrees, MAE_treino, MAE_teste, Rsquared_treino, Rsquared_teste) |>
  kbl(
    format = "latex",
    caption = "Resultados do modelo Random Forest",
    label = "tab_j4_1",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/tab_j4_resultados.tex")

##### 1.1.1 Estatísticas do R² ----
df_j4 |> 
  summarise(Rsquared_teste_media = mean(Rsquared_teste),
            Rsquared_teste_min = min(Rsquared_teste),
            Rsquared_teste_max = max(Rsquared_teste))

##### 1.1.2 ntrees com maior acurácia
df_j4 |> 
  arrange(desc(Rsquared_teste)) |> 
  relocate(ntrees, Rsquared_teste) |> 
  head() |> 
  view()

### 1.2. Gráficos ----
# Aqui estão os gráficos relevantes:
# Relação de acurácia por quantidade de árvores para a melhor porcentagem

#### 1.2.1. ntrees X acurácia ----
df_j4 |> 
  ggplot(aes(x = ntrees, y = Rsquared_teste)) +
  geom_point() +
  geom_line() +
  labs(title = "ntrees X Rsquared_teste",
       y = "Rsquared_teste") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(
    breaks = c(unique(df_j4$ntrees)),
    labels = c(unique(df_j4$ntrees))
  )

df_j4 |> 
  slice(-1) |> 
  ggplot(aes(x = ntrees, y = Rsquared_teste)) +
  geom_point() +
  geom_line() +
  labs(title = "ntrees X Rsquared_teste",
       y = "Rsquared_teste") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(
    breaks = c(unique(df_j4$ntrees)),
    labels = c(unique(df_j4$ntrees))
  )
# Análise:
# Curva perfeitamente razoável

# Análise:
# Curva sensível. Faz sentido a ascenção "linear"
# Análise:
# Padrão praticamente aleatório! Modelo defintiviamente não
# Aprendeu algo aqui...

##### 1.4.1 Mínimo, Máximo, Mediano e Médio ----
df_j4 |>
  group_by(ntrees) |> 
  summarise(tempo_medio_min = min(runtime_sec),
            tempo_medio_max = max(runtime_sec),
            tempo_medio_median = median(runtime_sec),
            tempo_medio_medio = mean(runtime_sec)) |> 
  pivot_longer(
    cols = starts_with("tempo_medio"),
    names_to = "estatistica",
    values_to = "valor"
  ) |>
  ggplot(aes(x = ntrees, y = valor)) +
  geom_line() +
  geom_point() +
  facet_wrap(~estatistica, scales = "free_y") +
  labs(
    x = "ntrees",
    y = "Tempo médio (s)"
  ) +
  theme_minimal()
# Análise: Todas bem lineares

## 2. Tabelas ----
### 2.1. Top 10 modelos ----
df_j4 |>
  slice_max(order_by = Rsquared_teste,
            n = 10,
            with_ties = TRUE) |> 
  select(Rsquared_teste,
         Rsquared_treino,
         MAE_treino,
         MAE_teste,
         ntrees)

# Análise:
# Todos os 10 melhor modelos são para  agora 1

#### 2.2.1, Export ----
df_j4 |>
  slice_max(order_by = Rsquared_teste,
            n = 10,
            with_ties = TRUE) |> 
  select(Rsquared_teste,
         Rsquared_treino,
         MAE_treino,
         MAE_teste,
         ntrees) |> 
  kbl(
    format = "latex",
    caption = "Top 10 modelos de classificação",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/j4-tab-top10.tex")

#### 3. Gerais do melhor modelo ----
df_j4 |>
  slice_max(order_by = Rsquared_teste, n = 1, with_ties = TRUE) |> 
  kbl(
    format = "latex",
    caption = "Melhor modelo de classificação",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/j4-tab-melhor.tex")
