# --- 1. INSTALAÇÃO E CARREGAMENTO DOS PACOTES ---
# Instala os pacotes necessários caso eles ainda não estejam instalados
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("ranger")) install.packages("ranger", dependencies = TRUE)
if (!require("doParallel")) install.packages("doParallel", dependencies = TRUE)
if (!require("tictoc")) install.packages("tictoc", dependencies = TRUE)
if (!require("janitor")) install.packages("janitor", dependencies = TRUE)

# Carrega todos os pacotes na sessão
library(tidyverse)
library(caret)
library(ranger)
library(doParallel)
library(tictoc)