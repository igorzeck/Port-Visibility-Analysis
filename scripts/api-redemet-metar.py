# --- Script para a extração de METARs pelo API REDEMET ---
# Link para instruções de acesso: https://ajuda.decea.mil.br/base-de-conhecimento/api-redemet-o-que-e/
# Este script gera dois arquivos:
# CSV contendo os METARS
# LOG contendo os METARS que não puderam ser decodificados
import subprocess as sp
import json
from metar.Metar import Metar
import datetime
from datetime import datetime as dt
from math import ceil
from os.path import isfile

# Chave fornecida pelo DECEA (Necessário solicitar)
api_key=''

# Parâmetros de formatação
dt_pstr = "%Y%m%d"
dt_pstr_data = "%Y-%m-%d"
dt_pstr_comp = "%Y-%m-%d %H:%M:%S"

# Parâmetros modificáveis
data_ini = datetime.date(2011, 1, 1)
data_fim = datetime.date(2025, 8, 11)
localidade='SBGL'

data_ini_str = data_ini.strftime(dt_pstr + "00")
data_fim_str = data_fim.strftime(dt_pstr + "23")

# Arquivo
arq_result=f"metar-{localidade}-{data_ini.strftime(dt_pstr_data)}-{data_fim.strftime(dt_pstr_data)}.csv"
arq_log=f"metar-{localidade}-{data_ini.strftime(dt_pstr_data)}-{data_fim.strftime(dt_pstr_data)}.log"

colunas = [
    "datetime",  # UTC - 0
    "tipo-report",  # Código METAR, SPECI, ou METAR COR
    "id-estacao",
    "temp-ar(c)",
    "ponto_orvalho(c)",
    "velocidade_vento(m/s)",
    "dir-vento(graus)",
    "vis(m)",
    "pressao(mb)",
    "umidade-relativa"
]

log_list = []

# -- Checagem inicial --
if not api_key:
    print("Chave API não preenchida!")
    exit(-1)

## -- Funções --
def decod_metar(linhas: list[str]):
    """ Limpa a linha e padroniza utilizando a biblioteca que interpreta o METAR """
    linhas_filtradas = []
    ultimo_timestamp = data_ini.strftime(dt_pstr_comp)
    for linha in linhas:
        if "METAR" in linha or "SPECI" in linha:
            timestamp, metar_raw = linha.strip().split("<>", 1)
            timestamp_dt = dt.strptime(timestamp, dt_pstr_comp).strftime(dt_pstr_comp)
            metar_str = metar_raw.rstrip("=")

            # Retira string que indica correção
            is_cor = False
            if " COR" in linha:
                metar_str = metar_str.replace(" COR", "")
                is_cor = True
            try:
                report = Metar(metar_str)
                # Hora do report
                if report.time:
                    # Report.time só contém dia e hora
                    timestamp_final = dt.strptime(timestamp, "%Y-%m-%d %H:%M:%S").strftime("%Y-%m-") + report.time.strftime("%d %H:%M:%S")
                else:
                    timestamp_final = timestamp_dt
                parts = [
                    timestamp_final,
                    report.type if not is_cor else (report.type + " COR"),
                    report.station_id or "",
                    str(report.temp.value("C")) if report.temp else "",
                    str(report.dewpt.value("C")) if report.dewpt else "",
                    str(report.rel_umidity.value()) if report.rel_umidity else "",
                    str(report.wind_speed.value("mps")) if report.wind_speed else "",
                    str(report.wind_dir.value()) if report.wind_dir else "",
                    str(report.vis.value("m")) if report.vis else "",
                    str(report.press.value("mb")) if report.press else "",
                ]
                # Mantém apenas os corrigidos
                if is_cor and (ultimo_timestamp == timestamp_final):
                    linhas_filtradas.pop()
                linhas_filtradas.append(",".join(parts) + "\n")
                ultimo_timestamp = timestamp_final
            except Exception as e:
                err_str = f"ERRO ao decodar o METAR: {metar_str}\n"
                linhas_filtradas.append(['NA']) # Para manter coesão temporal
                print(err_str, end="")
                log_list.append(err_str)
        else:
            err_str = f"Linha sem METAR ou SPECI: {metar_str}\n"
            print(err_str, end="")
            log_list.append(err_str)
    return linhas_filtradas

# -- Extração --
def extrair_metar():
    data_meio = data_ini
    linhas_brutas = []
    intervalo_horas=8700
    intervalo=datetime.timedelta(hours=intervalo_horas) # Máximo de entradas é 8760 permitidas em uma chamada...
    cont = 0
    cont_max = (data_fim - data_ini) / intervalo


    while data_meio < data_fim:
        # Esse mais 1 dia é para evitar duplicagem em cada loop
        data_meio_ant = data_meio + datetime.timedelta(1)
        data_meio += intervalo
        if data_meio > data_fim:
            data_meio = data_fim

        # 0. Comando atual
        data_meio_ant_str = data_meio_ant.strftime(dt_pstr + "00")
        data_meio_str = data_meio.strftime(dt_pstr + "23")

        # Prints
        print(f"Pegando dados de {data_meio_ant} 00:00 até {data_meio} 23:00...",end=" ")
        print((cont, ceil(cont_max)), f"{cont/cont_max*100:.2f}%")
        pagina = 1
        while True:
            # Tem que ir de página em página (cada uma com 150 entradas) pode pegar pelo json
            url_metar = f"https://api-redemet.decea.mil.br/mensagens/metar/{localidade}?&api_key={api_key}&data_ini={data_meio_ant_str}&data_fim={data_meio_str}&page={pagina}"
            # Definição do comando curl
            cmd = ['curl',
                '',
                    url_metar]
            # Passo 1 - curl (dados em json)
            _curl = sp.run(cmd, capture_output=True, text=True)

            # Passo 2 - json
            if _curl.returncode == 0:
                data = json.loads(_curl.stdout)
                # Passo 3 - pega as mensagens (METARs)
                # Data completa da emissão do METAR e METAR em si (mensagem)
                linhas_brutas.extend([item['validade_inicial'] + "<>" + item['mens'] for item in data['data']['data']])
                # Verifica se chegou na página finl
                if pagina == data['data']['last_page']:
                    break
                else:
                    # Senão vai para próxima
                    pagina += 1
            else:
                print(f"Erro {_curl.stderr}")
                break # Prossegue com o resto do script até a data que conseguiu capturar
    cont += 1
    return linhas_brutas


# todas as linhas coletadas com o wget
linhas_csv = [",".join(colunas) + "\n"]

linhas_metars = extrair_metar()
linhas_csv.extend(decod_metar(linhas_metars))
print("Sucesso!")
print("Criando arquivo .csv e de log...")
## Passo 4 - Arquivos finais
with open(arq_result, 'w', encoding='utf-8') as arqout:
    arqout.writelines(linhas_csv)

if log_list:
    with open(arq_log, 'w', encoding='utf-8') as arqout:
        arqout.writelines(log_list)