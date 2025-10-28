# Análise do modelo treinado J1 ----
# O modelo foi dividio sequencialmente por um cutoff
# 0. Setup ----
# install.packages(c("knitr", "kableExtra"))
library(tidyverse)
library(knitr)
library(kableExtra)

## 1. Arquivo Tamanho de árvores amostradas sequencialmente ----
# e com lag de visibilidade
df_j1 <- read_csv("exploracao/arq-j1-tam-ntrees.csv")

str(df_j1)
view(head(df_j1))

# Melhor resultado:
df_j1 |> 
  filter(bal_acc_nevoa == max(bal_acc_nevoa, na.rm = TRUE)) |> 
  view()

# Melhor para cada hora
df_j1 |>
  group_by(horas) |> 
  summarise(max_acc = max(bal_acc_nevoa))

### 1.1. Tabela de resultados ----
df_j1 |>
  arrange(desc(bal_acc_nevoa)) |>
  head() |>
  select(porcentagem, ntrees, bal_acc_nevoa,bal_acc_visivel, acuracia_total_teste, ppv_nevoa) |> 
  view()

df_j1 |>
  slice_max(order_by = bal_acc_nevoa, n = 10, with_ties = TRUE) |>
  select(porcentagem, ntrees, bal_acc_nevoa,bal_acc_visivel, acuracia_total_teste, ppv_nevoa) |>
  kbl(
    format = "latex",
    caption = "Resultados do modelo Random Forest",
    label = "tab_j1_1",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/tab_j1_resultados.tex")

##### 1.1.1 Melhor acurácia média por porcentagem ----
df_j1 |> 
  filter(horas == 1) |> 
  group_by(porcentagem) |> 
  summarise(bal_acc_nevoa_media = mean(bal_acc_nevoa),
            bal_acc_nevoa_min = min(bal_acc_nevoa),
            bal_acc_nevoa_max = max(bal_acc_nevoa)) |> 
  arrange(desc(bal_acc_nevoa_media)) |> 
  kbl(
    format = "latex",
    caption = "Melhor acurácia por porcentagem",
    label = "tab_j1_2",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/tab_j1_2.tex")

##### 1.1.2 ntrees com maior acurácia ----
df_j1 |> 
  arrange(desc(bal_acc_nevoa)) |> 
  relocate(ntrees, bal_acc_nevoa) |> 
  head() |> 
  view()

### 1.2. Gráficos ----
##### 1.2.1. Acurácia X ntrees X porcentagem X horas ----
# Distribuição geral de acurácia por porcentagem e número de árvores
install.packages("plotly")
library(plotly)

plot_ly(df_j1, x = ~porcentagem,
        y = ~ntrees,
        z = ~bal_acc_nevoa,
        type = "scatter3d",
        mode = "markers",
        color = ~as.factor(horas),
        marker = list(size = 5)) %>%
  layout(title = "Acurácia X ntrees X porcentagem X horas")

#### 1.2.2. Acurácia X ntrees X porcentagem X hora-1 ----

plot_ly(df_j1 |> filter(horas == 1),
        x = ~porcentagem,
        y = ~ntrees,
        z = ~bal_acc_nevoa,
        type = "scatter3d",
        mode = "lines",
        color = ~as.factor(porcentagem),
        line = list(width = 4),
        marker = list(size = 3)) %>%
  layout(title = "Acurácia X ntrees X porcentahem X horas")

#### 1.2.3. ppv_nevoa X ntrees X porcentagem X hora-1 ----
# Distribuição geral de acurácia por porcentagem e número de árvores
plot_ly(df_j1 |> filter(horas == 1),
        x = ~porcentagem,
        y = ~ntrees,
        z = ~ppv_nevoa,
        type = "scatter3d",
        mode = "lines",
        color = ~as.factor(porcentagem),
        line = list(width = 4),
        marker = list(size = 3)) %>%
  layout(title = "PPV X ntrees X porcentagem X horas")

#### 1.2.4. acurácia de teste X ntrees X porcentagem X hora-1 ----
# Distribuição geral de acurácia por porcentagem e número de árvores
plot_ly(df_j1 |> filter(horas == 1),
        x = ~porcentagem,
        y = ~ntrees,
        z = ~acuracia_total_teste,
        type = "scatter3d",
        mode = "lines",
        color = ~as.factor(porcentagem),
        line = list(width = 4),
        marker = list(size = 3)) %>%
  layout(title = "PPV X ntrees X porcentagem X horas")

# Aqui estão os gráficos relevantes:
# Relação de acurácia por quantidade de árvores para a melhor porcentagem

#### 1.3.1. ntrees X acurácia para modelo mais promissor (porcentagem de 15%) ----
df_j1 |> 
  filter(horas == 1, porcentagem <= 0.16) |> 
  ggplot(aes(x = ntrees, y = bal_acc_nevoa)) +
  geom_point() +
  geom_line() +
  labs(title = "ntrees X acurácia (névoa)",
       y = "Acurácia (névoa)") +
  theme(plot.title = element_text(hjust = 0.5))
# Análise:
# Qqqeda abruta no final atualmente inexplicável, demais acurácias
# praticamente constantes

#### 1.3.2. Acurácia por porcentagem do dataset ----
# utilizado ntrees mais promissor "41"
df_j1 |>
  filter(horas == 1, ntrees == 41) |> 
  ggplot(aes(x = porcentagem, y = bal_acc_nevoa)) +
  geom_point() +
  geom_line() +
  labs(title = "porcentagem X acurácia (névoa)",
       y = "Acurácia (névoa)") +
  theme(plot.title = element_text(hjust = 0.5))
# Análise:
# Curva suspeita. Possível intoxicação de daods?
# Por, agora assume-se que a melhor porcentagem para treino é 0.15

#### 1.3.3. Melhores modelos para cada hora ----
df_j1 |>
  group_by(horas) |> 
  slice_max(order_by = bal_acc_nevoa,
            n = 1,
            with_ties = TRUE) |> 
  select(bal_acc_nevoa,
         porcentagem,
         ntrees,
         horas) |> 
  ggplot(aes(x = horas, y = bal_acc_nevoa)) +
  scale_x_continuous(breaks = unique(df_j1$horas),
                     labels = unique(df_j1$horas)) +
  geom_point() +
  geom_line()

df_j1 |>
  group_by(horas) |> 
  slice_max(order_by = acuracia_total_teste,
            n = 1,
            with_ties = TRUE) |> 
  select(bal_acc_nevoa,
         porcentagem,
         ntrees,
         horas,
         acuracia_total_teste) |> 
  ggplot(aes(x = horas, y = acuracia_total_teste)) +
  scale_x_continuous(breaks = unique(df_j1$horas),
                     labels = unique(df_j1$horas)) +
  geom_point() +
  geom_line()

df_j1 |>
  group_by(horas) |> 
  slice_max(order_by = bal_acc_nevoa,
            n = 1,
            with_ties = TRUE) |> 
  select(bal_acc_nevoa,
         porcentagem,
         ntrees,
         horas) |> 
  ggplot(aes(x = horas, y = bal_acc_nevoa)) +
  geom_line(linetype = "dotted") +
  geom_point(aes(shape = as.factor(ntrees),
                 color = as.factor(porcentagem)),
             size = 5) +
  scale_x_continuous(
    breaks = c(unique(df_j1$horas)),
    labels = c(unique(df_j1$horas))
  )
  
# Análise:
# Pode-se notar que em alguns casos o melhor número de árvores foi 1!
# Houve uma crescente , mas em geral, a tencẽncia é diminuir a acurácia

#### 1.3.3. Melhores modelos para cada ntrees ----
df_j1 |>
  group_by(ntrees) |> 
  slice_max(order_by = bal_acc_nevoa,
            n = 1,
            with_ties = TRUE) |> 
  select(bal_acc_nevoa,
         porcentagem,
         ntrees,
         horas) |> 
  ggplot(aes(x = ntrees, y = bal_acc_nevoa)) +
  geom_line(linetype = "dotted") +
  geom_point(aes(shape = as.factor(horas),
                 color = as.factor(porcentagem)),
             size = 5) +
  scale_x_continuous(
    breaks = c(unique(df_j1$ntrees)),
    labels = c(unique(df_j1$ntrees))
  )
view(df_j1)
# Análise:
# Houve apenas duas horas com melhor performance: 1 e 7
# Vale notar que a porcentagem dominante foi 15% e 
# a hora dominante foi para 1 hora no futuro

### 1.4. Runtime ----
#### 1.4.1 Por ntrees ----
##### 1.4.1.1 Médio ----
df_j1 |>
  group_by(ntrees) |> 
  summarise(tempo_medio = mean(runtime_sec)) |> 
  ggplot(aes(x = ntrees, y = tempo_medio)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = c(unique(df_j1$ntrees)),
    labels = c(unique(df_j1$ntrees))
  )
# Análise:
# Relação perfeitamente linear

##### 1.4.1.2 Mínimo, Máximo, Mediano e Médio ----
df_j1 |>
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

#### 1.4.2 Por porcentagem ----
##### 1.4.2.1 Médio ----
df_j1 |>
  group_by(porcentagem) |> 
  summarise(tempo_medio = mean(runtime_sec)) |> 
  ggplot(aes(x = porcentagem, y = tempo_medio)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = c(unique(df_j1$porcentagem)),
    labels = c(unique(df_j1$porcentagem))
  )
# Análise:
# Relação praticamente perfeitamente linear

##### 1.4.2.2 Mínimo, Máximo, Mediano e Médio ----
df_j1 |>
  group_by(porcentagem) |> 
  summarise(tempo_medio_min = min(runtime_sec),
            tempo_medio_max = max(runtime_sec),
            tempo_medio_mediano = median(runtime_sec),
            tempo_medio_medio = mean(runtime_sec)) |> 
  pivot_longer(
    cols = starts_with("tempo_medio"),
    names_to = "estatistica",
    values_to = "valor"
  ) |>
  ggplot(aes(x = porcentagem, y = valor)) +
  geom_line() +
  geom_point() +
  facet_wrap(~estatistica, scales = "free_y") +
  labs(
    x = "Porcentagem",
    y = "Tempo médio (s)"
  ) +
  theme_minimal()

## 2. Tabelas ----
### 2.1. Top 10 modelos ----
df_j1 |>
  slice_max(order_by = bal_acc_nevoa,
            n = 10,
            with_ties = TRUE) |> 
  select(bal_acc_nevoa,
         porcentagem,
         ntrees,
         horas)

# Análise:
# Todos os 10 melhor modelos são para  agora 1

#### 2.2.1, Export ----
df_j1 |>
  slice_max(order_by = bal_acc_nevoa, n = 10, with_ties = TRUE) |> 
  select(bal_acc_nevoa, porcentagem, ntrees, horas) |> 
  kbl(
    format = "latex",
    caption = "Top 10 modelos de classificação",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/tab-top10.tex")

#### 3. Gerais do melhor modelo ----
df_j1 |>
  slice_max(order_by = bal_acc_nevoa, n = 1, with_ties = TRUE) |> 
  kbl(
    format = "latex",
    caption = "Melhor modelo de classificação",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/tab-melhor.tex")
