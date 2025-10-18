# Análise dos modelos treinados ----
# 0. Setup ----
library(tidyverse)

df <- read_csv("datasets/dataset-definitivo.csv") |> 
  janitor::clean_names()

str(df)

# 0.1 Incidência de névoa ----
df |> 
  relocate(vis_m) |> 
  filter(vis_m <= 2e3)

df |> 
  relocate(vis_m) |> 
  filter(vis_m <= 2e3) |>
  group_by(year(datetime)) |> 
  summarise(n = n())

# 0.1.1. Incidência de névoa nos primeiro 15% do dataset ----
# 15 % do dataset...
df[floor(0.15 * nrow(df)),]

df |> 
  filter(datetime <= "2013-03-07") |> 
  relocate(vis_m) |> 
  filter(vis_m <= 2e3)

# 0.1.2. Incidência de névoa nos primeiro 15% do dataset ----
# 85% do dataset...
df[floor(0.85 * nrow(df)),]

df |> 
  filter(datetime <= "2023-06-11") |> 
  relocate(vis_m) |> 
  filter(vis_m <= 2e3)


# 1. Arquivo Tamanho de árvores amostradas sequencialmente ----
# e com lag de visibilidade
df_j1 <- read_csv("exploracao/arq-j1-tam-ntrees.csv")

str(df_j1)

df_j1 |> 
  arrange(desc(ppv_nevoa)) |> 
  relocate(ppv_nevoa) |> 
  view()

# 2. Arquivo com tamanho árvore aleatório e sem lag de vis ----
df_j2 <- read_csv("exploracao/arq-j2-tam-ntrees.csv")

str(df_j2)

df_j2 |> 
  arrange(desc(ppv_nevoa)) |> 
  relocate(ppv_nevoa) |> 
  view()
# Aparenta ter uma performance melhor, mas demanda mais dados

# 3. Arquivo com tamanho árvores aleatório e tam 50/50 ----
df_j3 <- read_csv("exploracao/arq-j3-tam-ntrees.csv")

str(df_j3)

df_j3 |> 
  arrange(desc(ppv_nevoa)) |> 
  relocate(ppv_nevoa) |> 
  view()
# Aparente ter ótimos resultados, na realidade! Dar uma olhada nas classes
# mais importantes poderia ser uma boa

# 4. Arquivo com regressão e tam 50/50----
df_j4 <- read_csv("exploracao/arq-j4-tam-ntrees.csv")

str(df_j4)

df_j4 |> 
  arrange(desc(Rsquared_teste)) |> 
  relocate(Rsquared_teste) |> 
  view()
