# Arquivo que busca entender o impacto do lag na acurácia ----
## 1. Setup ----
df <- read_csv("dataset-definitivo.csv")
# Procura saber qual a chance de a hora atual estar no
# treino e a anterior no dataset, levando
# em conta apenas 1h de lag

### 1.2. Adição de lag no treino
df <- df %>%
  mutate(
    datetime_ant = lag(datetime, 1)
  )

## 2. Divisão do dataset ----
set.seed(42)
prop <- 0.2
sample_size <- floor(prop * nrow(df))
train_indices <- sample(seq_len(nrow(df)), size = sample_size)

df_treino <- df[train_indices, ]
df_teste <- df[-train_indices, ]

## 3. Probabilidade da hora anterior ser usada no teste do treino ----
### 3.1. Probabilisticamente ----
# P(X - 1 em treino | X em treino) = (P(X em treino ^ X - 1 em treino)) / P(X em treino)
# P(X - 1 em treino | X em treino) = (prop * prop) / prop
p <- prop
paste0(round(p, 3), "%")
### 3.2. Empiricamente ---- 
intersecao <- function(var1, var2, tam) {
  print(sum(var1 %in% var2))
  val <- sum(var1 %in% var2) / tam
  paste0(round(val * 100, 3), "%")
}

sum(df_treino$datetime_ant %in% df_treino$datetime)
dim(df_treino)
intersecao(df_treino$datetime_ant,
           df_treino$datetime,
           dim(df_treino)[1])

# Há impacto não insignificante!
# Verificando modelo G2
source("libs.R")
modelo_g2 <- readRDS("c-G2-modelo.rds")
varImp(modelo_g2)
modelo_g2$finalModel$variable.importance

varImp(modelo_g2) %>%
  ggplot(aes(x = reorder(variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Importance",
    title = "Variable Importance (ranger via caret)"
  ) +
  theme_minimal(base_size = 14)
