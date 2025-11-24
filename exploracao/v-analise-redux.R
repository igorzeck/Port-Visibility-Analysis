# Análise V ----
# Arquivo de análise dos modelos treinados para o relatório V
library(tidyverse)

## V1 ----
# Redux foi criado com uma versão menor do dataset (13 mil entradas)
df1_redux <- read_csv("exploracao/arq-v-1-redux.csv")
df1 <- read_csv("exploracao/arq-v-1.csv")

glimpse(df1_redux)
glimpse(df1)
### Gráficos da acurácia do redux ----
df1_redux |> 
  group_by(ntrees) |> 
  summarise(
    acc_media_treino = mean(acuracia_total_treino),
    acc_media_teste = mean(acuracia_total_teste)
    ) |> 
  ggplot() +
  geom_line(aes(x = ntrees, y = acc_media_treino), color = "blue") +
  geom_point(aes(x = ntrees, y = acc_media_treino), color = "blue") +
  geom_line(aes(x = ntrees, y = acc_media_teste), color = "red") +
  geom_point(aes(x = ntrees, y = acc_media_teste), color = "red") +
  scale_x_continuous(breaks = seq(1,201,20))
# Acurácia de teste é maior que a de treino... Novamente essa anomalia

### Gráficos da acurácia ----
df1 |> 
  group_by(ntrees) |> 
  summarise(
    acc_media_treino = mean(acuracia_total_treino),
    acc_media_teste = mean(acuracia_total_teste)
  ) |> 
  ggplot() +
  geom_line(aes(x = ntrees, y = acc_media_treino), color = "blue") +
  geom_point(aes(x = ntrees, y = acc_media_treino), color = "blue") +
  geom_line(aes(x = ntrees, y = acc_media_teste), color = "red") +
  geom_point(aes(x = ntrees, y = acc_media_teste), color = "red") +
  scale_x_continuous(breaks = seq(1,201,20))
