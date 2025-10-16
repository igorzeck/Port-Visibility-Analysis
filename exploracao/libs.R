# --- 1. INSTALAÇÃO E CARREGAMENTO DOS PACOTES ---
# Instala os pacotes necessários caso eles ainda não estejam instalados
if (!require("readr")) install.packages("readr", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("ranger")) install.packages("ranger", dependencies = TRUE)
if (!require("doParallel")) install.packages("doParallel", dependencies = TRUE)
if (!require("tictoc")) install.packages("tictoc", dependencies = TRUE)
if (!require("janitor")) install.packages("janitor", dependencies = TRUE)

# Carrega todos os pacotes na sessão
library(readr)
library(dplyr)
library(caret)
library(ranger)
library(doParallel)
library(tictoc)