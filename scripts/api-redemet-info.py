# --- Script para a extração de Informações meteorológicas pelo API REDEMET ---
# Link para instruções de acesso: https://ajuda.decea.mil.br/base-de-conhecimento/api-redemet-o-que-e/
# Este script gera dois arquivos:
# CSV contendo as informações
# LOG contendo as informações que não puderam ser extraídas
import subprocess as sp
import json
import datetime
from datetime import datetime as dt
from os.path import exists

# Chave fornecida pelo DECEA (Necessário solicitar)
api_key=''

# Parâmetros de formatação
dt_pstr = "%Y%m%d"
dt_pstr_data = "%Y-%m-%d"
dt_pstr_comp = "%Y-%m-%d %H:%M:%S"

# Parâmetros modificáveis
data_ini = datetime.date(2011, 1, 1)
# data_fim = datetime.date(2011, 1, 2)
data_fim = datetime.date(2025, 8, 11)
localidade='SBGL'

data_ini_str = data_ini.strftime(dt_pstr + "00")
data_fim_str = data_fim.strftime(dt_pstr + "23")

# Arquivo
arq_result=f"info-{localidade}-{data_ini.strftime(dt_pstr_data)}-{data_fim.strftime(dt_pstr_data)}.csv"
arq_log=f"info-{localidade}-{data_ini.strftime(dt_pstr_data)}-{data_fim.strftime(dt_pstr_data)}.log"


log_list = []

# -- Checagem inicial --
if not api_key:
    print("Chave API não preenchida!")
    exit(-1)

## -- Funções --
# Extração da umidade relativa é feita hora a hora por de uma solicitação diferente
# Com informações meteorológicas de aerodromos
# Note que esse não contém correções, então pode haver inacurácia com os METARs
def extrair_ur():
    arq_temp = ".temp-ur"
    arq_temp_log = ".temp-ur.log"
    # Transforma date em datetime (combinando com time à meia noite)
    # Inicia de onde parou o arquivo temporário
    cont = 0
    if exists(arq_temp):
        with open(arq_temp, 'r') as arqin:
            linha_final = ''
            for linha in arqin:
                if linha != '\n':
                    linha_final = linha
                    cont += 1
                    pass
            if linha_final:
                dt_meio = dt.strptime(linha_final.split(',')[0], dt_pstr_comp)
    else:
        dt_meio = dt.combine(data_ini, dt.min.time())
    dt_ini = dt.combine(data_ini, dt.min.time())
    dt_fim = dt.combine(data_fim, dt.min.time())

    linhas_brutas = []
    cont_max = (dt_fim - dt_ini).days * 24

    while dt_meio < dt_fim:
        # Esse mais 1 dia é para evitar duplicagem em cada loop
        dt_meio += datetime.timedelta(hours=1)
        if dt_meio > dt_fim:
            dt_meio = dt_fim

        # 0. Comando atual
        data_meio_str = dt_meio.strftime(dt_pstr + "23")

        # Prints
        print(f"Pegando dados de {dt_meio}...",end=" ")
        print(cont, cont_max, f"{cont/cont_max*100:.2f}%")
        url_info = f"https://api-redemet.decea.mil.br/aerodromos/info?api_key={api_key}&localidade={localidade}&datahora={data_meio_str}"
        # Definição do comando curl
        cmd = ['curl',
            '',
                url_info]
        # Passo 1 - curl (dados em json)
        _curl = sp.run(cmd, capture_output=True, text=True)

        # Passo 2 - json
        data = json.loads(_curl.stdout)

        # Passo 3 - pega as mensagens
        _ur_dt = str(dt_meio)
        try:
            _ur = data['data']['ur']

            # Passo 4 - tratamento e normalização
            _ur_val: str = _ur.split("%")[0]
        except Exception as e:
            err_str = f"ERRO: {e}\n"
            print(err_str, end="")
            _ur_val = 'NA'
            with open(arq_temp_log, 'a') as arqout:
                arqout.write((_ur_dt + ',' + _ur_val + '\n'))
        if _ur_val.isnumeric():
            _ur_norm = str(int(_ur.split("%")[0])/100)
        else:
            with open(arq_temp_log, 'a') as arqout:
                arqout.write((_ur_dt + ',' + _ur_val + '\n'))
            _ur_norm = 'NA'

        # Adiciona a um arquivo temporário, para caso haja algum problema
        # Não se iniciar do zero
        with open(arq_temp, 'a') as arqout:
            arqout.write((_ur_dt + ',' + _ur_norm + '\n'))
        linhas_brutas.extend([[_ur_norm]])
        cont += 1
    return linhas_brutas

# todas as linhas coletadas com o wget
linhas_csv = ['datetime','umidade-relativa' + '\n']

linhas_csv.extend(extrair_ur())
print(linhas_csv)
print("Sucesso!")
print("Criando arquivo .csv e de log...")
## Passo 4 - Arquivos finais
with open(arq_result, 'w', encoding='utf-8') as arqout:
    arqout.writelines(linhas_csv)

if log_list:
    with open(arq_log, 'w', encoding='utf-8') as arqout:
        arqout.writelines(log_list)