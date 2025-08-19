# Script para juntar os .csv em um dataset unificado
# - Importações -
import pandas as pd
import numpy as np

# - Extração -
df_lmlt = pd.read_csv('datasets/lmlt-galeao.csv', dtype={'data':'object','hora':'object'})
df_info = pd.read_csv('datasets/info-SBGL-2011-01-01-2025-08-11.csv', parse_dates=['datetime'], index_col=0)
df_metar = pd.read_csv('datasets/metar-SBGL-2011-01-01-2025-08-11.csv', parse_dates=['datetime'], index_col=0)

# - Limpeza e transformações -
df_metar.drop(['tipo-report', 'id-estacao'], axis=1, inplace=True)

df_lmlt['datetime'] = pd.to_datetime(df_lmlt['data'] + ' ' + df_lmlt['hora'], format='%Y%m%d %H%M')
df_lmlt.set_index('datetime', inplace=True)
df_lmlt.drop(['data', 'hora'], axis=1, inplace=True)

df_lmlt.rename({'valor':'lmlt(c)'}, axis=1, inplace=True)

# Kelvin para Clesius
df_lmlt['lmlt(c)'] -= 273.15

# - Junção dos datasets -
df_exp = df_metar.join(df_lmlt, on='datetime').join(df_info, on='datetime')

# - Transformações finais -
df_exp.bfill(inplace=True)
df_exp.dropna(inplace=True)

df_exp.rename({'velocidade_vento(m/s)':'velocidade-vento(mps)', 'ponto_orvalho(c)':'ponto-orvalho(c)'}, axis=1, inplace=True)

# Arredondamento
for col in df_exp.select_dtypes(include=np.number).columns:
    df_exp[col] = df_exp[col].round(2)

# - Exportação -
print("Arquivo sendo exportando...")

df_exp.to_csv('datasets/dataset-definitivo.csv')

print("Arquivo exportado com sucesso!")