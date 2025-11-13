# Análise do distro. de névoa ----
# TODO: Passar para tex
# 0. Setup
library(tidyverse)

df <- read_csv("datasets/dataset-definitivo.csv") |> 
  janitor::clean_names()

str(df)

# 1. Incidência de névoa ----
df |> 
  relocate(vis_m) |> 
  filter(vis_m <= 2e3)

df |> 
  relocate(vis_m) |> 
  filter(vis_m <= 2e3) |>
  group_by(year(datetime)) |> 
  summarise(n = n())