#!/bin/bash
# - ATENÇÃO -
# ESTE CÓDIGO SÓ FUNCIONA EM MÁQUINAS LINUX E ASSUME QUE
# TODOS OS GRIBS RELANTES ESTEJAM EM SEU DIRETÓRIO
# - ATENÇÃO -
# Script para transformar todos Gribs do direitório em um csv
# Aceita 1 argumento, sendo o nome do arquivo (com extensão)
arqs=($(ls datasets/*.grib))
if [ -z $1 ]; then
	arq_final="datasets/lmlt-galeao.csv"
else
	arq_final=$1
fi
# Limpa arquivo de exportação
echo "data,hora,valor" > $arq_final
for arq in "${arqs[@]}"; do
	echo "Lidando com o arquivo $arq..."
	grib_get -p validityDate:string,validityTime:string,values "$arq"  | sed 's/\s\+/,/g' >> "$arq_final"
done
echo "Finalizado! Exportado para $arq_final"
