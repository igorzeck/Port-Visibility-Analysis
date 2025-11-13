# Análise dos modelos treinados ----
# Dataset sequencialmente dividido meio ameio
# com a variável de lag de visbilidade
# 0. Setup ----
# install.packages(c("knitr", "kableExtra"))
library(tidyverse)
library(knitr)
library(kableExtra)

# TODO: Faria mais sentido olhar o modelo mais promissor baseado na acurácia média!

## 1. Arquivo Tamanho de árvores amostradas sequencialmente ----
# e com lag de visibilidade
df_j3 <- read_csv("exploracao/arq-j3-tam-ntrees.csv")

str(df_j3)
view(head(df_j3))

# Melhor resultado:
df_j3 |> 
  filter(bal_acc_nevoa == max(bal_acc_nevoa, na.rm = TRUE)) |> 
  view()
# Acurácia de 60% para névoa e 
# E ótimo ppv para ambas categorias

# Melhor para cada hora
df_j3 |>
  group_by(horas) |> 
  slice_max(order_by = bal_acc_nevoa, n = 1, with_ties = TRUE) |> 
  relocate(horas, bal_acc_nevoa, ppv_nevoa, sens_nevoa, spec_nevoa)
# Análse:
# Todos sem grandes variações. Contudo, grande acerto
# Para névoa!

### 1.1. Tabela de resultados
df_j3 |>
  arrange(desc(bal_acc_nevoa)) |>
  head() |>
  arrange(ppv_nevoa,bal_acc_nevoa,bal_acc_visivel) |> 
  view()

df_j3 |>
  slice_max(order_by = bal_acc_nevoa, n = 10, with_ties = TRUE) |>
  select(horas, ntrees, ppv_nevoa,bal_acc_nevoa,bal_acc_visivel) |>
  kbl(
    format = "latex",
    caption = "Resultados do modelo Random Forest",
    label = "tab_j3_1",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-III-2025-10-18/res/tab_j3_resultados.tex")

# Melhor acurácia média por horas de árvores
df_j3 |> 
  group_by(horas) |> 
  summarise(bal_acc_media = mean(bal_acc_nevoa),
            bal_acc_min = min(bal_acc_nevoa),
            bal_acc_max = max(bal_acc_nevoa))

##### 1.1.1 Melhor acurácia média por horas no futuro ----
df_j3 |> 
  group_by(horas) |> 
  summarise(bal_acc_nevoa_media = mean(bal_acc_nevoa),
            bal_acc_nevoa_min = min(bal_acc_nevoa),
            bal_acc_nevoa_max = max(bal_acc_nevoa)) |> 
  arrange(desc(bal_acc_nevoa_max)) |> 
  kbl(
    format = "latex",
    caption = "Melhor acurácia por horas",
    label = "tab_j3_2",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".relatorios/relatorio-III-2025-10-18/res/tab_j3_2.tex")

##### 1.1.2 ntrees com maior acurácia
df_j3 |> 
  arrange(desc(bal_acc_nevoa)) |> 
  relocate(ntrees, bal_acc_nevoa) |> 
  head() |> 
  view()

### 1.2. Gráficos ----
# Distribuição geral de acurácia por porcentagem e número de árvores
# install.packages("plotly")
library(plotly)

#### 1.2.1. Acurácia X ntrees X hora ----
plot_ly(df_j3,
        x = ~horas,
        y = ~ntrees,
        z = ~bal_acc_nevoa,
        type = "scatter3d",
        mode = "lines",
        color = ~as.factor(horas),
        line = list(width = 4),
        marker = list(size = 3)) %>%
  layout(title = "Acurácia X ntrees X horas")
# Claramente o melhor foi o com 85% do dataset

#### 1.2.3. ppv_nevoa X ntrees X hora ----
# Distribuição geral de acurácia por número de árvores
plot_ly(df_j3,
        x = ~horas,
        y = ~ntrees,
        z = ~ppv_nevoa,
        type = "scatter3d",
        mode = "lines",
        color = ~as.factor(horas),
        line = list(width = 4),
        marker = list(size = 3)) %>%
  layout(title = "PPV X ntrees X horas")

#### 1.2.4. acurácia de teste X ntrees  X hora-1 ----
# Distribuição geral de acurácia número de árvores
plot_ly(df_j3,
        x = ~horas,
        y = ~ntrees,
        z = ~acuracia_total_teste,
        type = "scatter3d",
        mode = "lines",
        color = ~as.factor(horas),
        line = list(width = 4),
        marker = list(size = 3)) %>%
  layout(title = "PPV X ntrees X porcentahem X horas")

# Aqui estão os gráficos relevantes:
# Relação de acurácia por quantidade de árvores para a melhor porcentagem

#### 1.3.1. horas X acurácia para modelo mais promissor (ntrees = 121) ----
df_j3 |> 
  filter(ntrees == 121) |> 
  ggplot(aes(x = horas, y = bal_acc_nevoa)) +
  geom_point() +
  geom_line() +
  labs(title = "horas X acurácia (névoa)",
       y = "Acurácia (névoa)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(
    breaks = c(unique(df_j3$horas)),
    labels = c(unique(df_j3$horas))
  )
# Análise:
# Como esperado, para horas meores há um decréscimo de horas

df_j3 |>
  group_by(horas) |> 
  slice_max(order_by = acuracia_total_teste,
            n = 1,
            with_ties = TRUE) |> 
  select(bal_acc_nevoa,
         ntrees,
         horas,
         acuracia_total_teste) |> 
  ggplot(aes(x = horas, y = acuracia_total_teste)) +
  scale_x_continuous(breaks = unique(df_j3$horas),
                     labels = unique(df_j3$horas)) +
  geom_point() +
  geom_line()
# Análise:
# Sem padrão

# Análise:
# Curva sensível. Faz sentido a ascenção "linear"

#### 1.3.2. Melhores modelos para cada hora ----
df_j3 |>
  group_by(horas) |> 
  slice_max(order_by = bal_acc_nevoa,
            n = 1,
            with_ties = TRUE) |> 
  select(bal_acc_nevoa,
         ntrees,
         horas) |> 
  ggplot(aes(x = horas, y = bal_acc_nevoa)) +
  geom_line(linetype = "dotted") +
  geom_point(aes(shape = as.factor(ntrees),
                 color = as.factor(ntrees)),
             size = 5) +
  scale_x_continuous(
    breaks = c(unique(df_j3$horas)),
    labels = c(unique(df_j3$horas))
  )

#### 1.3.3. Melhores modelos para cada ntrees ----
df_j3 |>
  group_by(ntrees) |> 
  slice_max(order_by = bal_acc_nevoa,
            n = 1,
            with_ties = FALSE) |> 
  select(bal_acc_nevoa,
         ntrees,
         horas) |> 
  ggplot(aes(x = ntrees, y = bal_acc_nevoa)) +
  geom_line(linetype = "dotted") +
  geom_point(aes(shape = as.factor(horas),
                 color = as.factor(horas)),
             size = 5) +
  scale_x_continuous(
    breaks = c(unique(df_j3$ntrees)),
    labels = c(unique(df_j3$ntrees))
  )
view(df_j3)
# Análise:
# Os melhores modelos preiram 1h no futuro

### 1.4. Runtime ----
#### 1.4.1 Por ntrees ----
##### 1.4.1.1 Médio ----
df_j3 |>
  group_by(ntrees) |> 
  summarise(tempo_medio = mean(runtime_sec)) |> 
  ggplot(aes(x = ntrees, y = tempo_medio)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = c(unique(df_j3$ntrees)),
    labels = c(unique(df_j3$ntrees))
  )
# Análise:
# Relação perfeitamente linear

##### 1.4.1.2 Mínimo, Máximo, Mediano e Médio ----
df_j3 |>
  group_by(ntrees) |> 
  summarise(tempo_medio_min = min(runtime_sec),
            tempo_medio_max = max(runtime_sec),
            tempo_medio_mediano = median(runtime_sec),
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
df_j3 |>
  slice_max(order_by = bal_acc_nevoa,
            n = 10,
            with_ties = TRUE) |> 
  select(bal_acc_nevoa,
         ntrees,
         horas)

# Análise:
# Todos os 10 melhor modelos são para  agora 1