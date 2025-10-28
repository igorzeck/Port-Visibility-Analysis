# Análise dos modelos treinados ----
# O arquivo procurou entender o impacto sem
# Lag de visibilidade
# Com seleção de dados por amostragem
# 0. Setup ----
# install.packages(c("knitr", "kableExtra"))
library(tidyverse)
library(knitr)
library(kableExtra)

## 1. Arquivo Tamanho de árvores amostradas sequencialmente ----
# e com lag de visibilidade
df_j2 <- read_csv("exploracao/arq-j2-tam-ntrees.csv")

str(df_j2)
view(head(df_j2))

# Melhor resultado:
df_j2 |> 
  filter(bal_acc_nevoa == max(bal_acc_nevoa, na.rm = TRUE)) |> 
  view()
# Análise:
# Foi pior do que o que tinha acesso a variável lag

# Melhor para cada hora
df_j2 |>
  group_by(horas) |> 
  slice_max(order_by = bal_acc_nevoa, n = 1, with_ties = TRUE) |> 
  relocate(horas, bal_acc_nevoa, ppv_nevoa, sens_nevoa, spec_nevoa)
# Análse:
# Todos sem grandes variações. Contudo, grande acerto
# Para névoa!

### 1.1. Tabela de resultados
df_j2 |>
  arrange(desc(bal_acc_nevoa)) |>
  head() |>
  arrange(ppv_nevoa,bal_acc_nevoa,bal_acc_visivel) |> 
  view()

df_j2 |>
  slice_max(order_by = bal_acc_nevoa, n = 10, with_ties = TRUE) |>
  select(porcentagem, horas, ntrees, ppv_nevoa,bal_acc_nevoa,bal_acc_visivel) |>
  kbl(
    format = "latex",
    caption = "Resultados do modelo Random Forest",
    label = "tab_j2_1",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/tab_j2_resultados.tex")

# Melhor acurácia média por porcentagem de árvores
df_j2 |> 
  filter(horas == 1) |> 
  group_by(porcentagem) |> 
  summarise(bal_acc_media = mean(bal_acc_nevoa),
            bal_acc_min = min(bal_acc_nevoa),
            bal_acc_max = max(bal_acc_nevoa))

##### 1.1.1 Melhor acurácia média por horas no futuro ----
df_j2 |> 
  group_by(horas) |> 
  summarise(bal_acc_nevoa_media = mean(bal_acc_nevoa),
            bal_acc_nevoa_min = min(bal_acc_nevoa),
            bal_acc_nevoa_max = max(bal_acc_nevoa)) |> 
  arrange(desc(bal_acc_nevoa_media)) |> 
  kbl(
    format = "latex",
    caption = "Melhor acurácia por horas",
    label = "tab_j2_2",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/tab_j2_2.tex")

##### 1.1.2 ntrees com maior acurácia
df_j2 |> 
  arrange(desc(bal_acc_nevoa)) |> 
  relocate(ntrees, bal_acc_nevoa) |> 
  head() |> 
  view()

### 1.2. Gráficos ----
# Distribuição geral de acurácia por porcentagem e número de árvores
# install.packages("plotly")
library(plotly)

#### 1.2.1. Acurácia X ntrees X porcentagem X hora-1 ----
plot_ly(df_j2 |> filter(horas == 1),
        x = ~porcentagem,
        y = ~ntrees,
        z = ~bal_acc_nevoa,
        type = "scatter3d",
        mode = "lines",
        color = ~as.factor(porcentagem),
        line = list(width = 4),
        marker = list(size = 3)) %>%
  layout(title = "Acurácia X ntrees X 1h")
# Claramente o melhor foi o com 85% do dataset

#### 1.2.3. ppv_nevoa X ntrees X porcentagem X hora-1 ----
# Distribuição geral de acurácia por porcentagem e número de árvores
plot_ly(df_j2 |> filter(horas == 1),
        x = ~porcentagem,
        y = ~ntrees,
        z = ~ppv_nevoa,
        type = "scatter3d",
        mode = "lines",
        color = ~as.factor(porcentagem),
        line = list(width = 4),
        marker = list(size = 3)) %>%
  layout(title = "PPV X ntrees X porcentahem X horas")

#### 1.2.4. acurácia de teste X ntrees X porcentagem X hora-1 ----
# Distribuição geral de acurácia por porcentagem e número de árvores
plot_ly(df_j2 |> filter(horas == 1),
        x = ~porcentagem,
        y = ~ntrees,
        z = ~acuracia_total_teste,
        type = "scatter3d",
        mode = "lines",
        color = ~as.factor(porcentagem),
        line = list(width = 4),
        marker = list(size = 3)) %>%
  layout(title = "PPV X ntrees X porcentahem X horas")

# Aqui estão os gráficos relevantes:
# Relação de acurácia por quantidade de árvores para a melhor porcentagem

#### 1.3.1. ntrees X acurácia para modelo mais promissor (porcentagem de 85%) ----
df_j2 |> 
  filter(horas == 1, porcentagem == .85) |> 
  ggplot(aes(x = ntrees, y = bal_acc_nevoa)) +
  geom_point() +
  geom_line() +
  labs(title = "ntrees X acurácia (névoa)",
       y = "Acurácia (névoa)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(
    breaks = c(unique(df_j2$ntrees)),
    labels = c(unique(df_j2$ntrees))
  )
# Análise:
# Segue uma curva mais padrão que a J1, mas ainda há
# Maior acurácia para menos árvores

df_j2 |>
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
  scale_x_continuous(breaks = unique(df_j2$horas),
                     labels = unique(df_j2$horas)) +
  geom_point() +
  geom_line()

#### 1.3.2. Acurácia por porcentagem do dataset ----
# utilizado ntrees mais promissor "41" (gráfico seção 1.3.1.)
df_j2 |>
  filter(horas == 1, ntrees == 41) |> 
  ggplot(aes(x = porcentagem, y = bal_acc_nevoa)) +
  geom_point() +
  geom_line() +
  labs(title = "porcentagem X acurácia (névoa)",
       y = "Acurácia (névoa)") +
  theme(plot.title = element_text(hjust = 0.5))
# Análise:
# Curva sensível. Faz sentido a ascenção "linear"

#### 1.3.3. Melhores modelos para cada hora ----
df_j2 |>
  group_by(horas) |> 
  slice_max(order_by = bal_acc_nevoa,
            n = 1,
            with_ties = TRUE) |> 
  select(bal_acc_nevoa,
         porcentagem,
         ntrees,
         horas) |> 
  ggplot(aes(x = horas, y = bal_acc_nevoa)) +
  geom_point() +
  geom_line()

df_j2 |>
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
    breaks = c(unique(df_j2$horas)),
    labels = c(unique(df_j2$horas))
  )

# Análise:
# Padrão praticamente aleatório! Modelo defintiviamente não
# Aprendeu algo aqui...

#### 1.3.4. Melhores modelos para cada ntrees ----
df_j2 |>
  group_by(ntrees) |> 
  slice_max(order_by = bal_acc_nevoa,
            n = 1,
            with_ties = FALSE) |> 
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
    breaks = c(unique(df_j2$ntrees)),
    labels = c(unique(df_j2$ntrees))
  )
view(df_j2)
# Análise:
# Os melhores modelos não previam uma hora no futuro...
# Pode ter sido apenas sorte!

### 1.4. Runtime ----
#### 1.4.1 Por ntrees ----
##### 1.4.1.1 Médio ----
df_j2 |>
  group_by(ntrees) |> 
  summarise(tempo_medio = mean(runtime_sec)) |> 
  ggplot(aes(x = ntrees, y = tempo_medio)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = c(unique(df_j2$ntrees)),
    labels = c(unique(df_j2$ntrees))
  )
# Análise:
# Relação perfeitamente linear

##### 1.4.1.2 Mínimo, Máximo, Mediano e Médio ----
df_j2 |>
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

#### 1.4.2 Por porcentagem ----
##### 1.4.2.1 Médio ----
df_j2 |>
  group_by(porcentagem) |> 
  summarise(tempo_medio = mean(runtime_sec)) |> 
  ggplot(aes(x = porcentagem, y = tempo_medio)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = c(unique(df_j2$porcentagem)),
    labels = c(unique(df_j2$porcentagem))
  )
# Análise:
# Relação praticamente perfeitamente linear

##### 1.4.2.2 Mínimo, Máximo, Mediano e Médio ----
df_j2 |>
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
# Análise: ligeiramente curvas, mas para todos os efeitos
# São lineares

## 2. Tabelas ----
### 2.1. Top 10 modelos ----
df_j2 |>
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
df_j2 |>
  slice_max(order_by = bal_acc_nevoa, n = 10, with_ties = TRUE) |> 
  select(bal_acc_nevoa, porcentagem, ntrees, horas) |> 
  kbl(
    format = "latex",
    caption = "Top 10 modelos de classificação",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/j2-tab-top10.tex")

#### 3. Gerais do melhor modelo ----
df_j2 |>
  slice_max(order_by = bal_acc_nevoa, n = 1, with_ties = TRUE) |> 
  kbl(
    format = "latex",
    caption = "Melhor modelo de classificação",
    booktabs = TRUE,
    digits = 3
  ) |>
  kable_styling(latex_options = c("scale_down")) |> 
  save_kable(".Relatórios/relatorio-III-2025-10-18/res/j2-tab-melhor.tex")
