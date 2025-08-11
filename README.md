# README *Port Visibility ANalysis*

Este é um repositório público contendo os contextos, scripts e datasets finalizados da pesquisa e subsequente criação de um modelo Random Forest para nevoeiros maritmos feita ao CENEP.

## Requisitos

Para os scripts do REDEMET é necessário uma chave API que pode ser solicitada [aqui](https://api-redemet.decea.mil.br/cadastro-api/). Além disso, os scripts e arquivos de análise ASSUMEM QUE OS *DATASETS* ESTÃO EM SUAS MESMAS PASTAS.

Mais informações em relação a API REDEMET em específico podem ser encontradas [aqui](https://ajuda.decea.mil.br/base-de-conhecimento/api-redemet-o-que-e/)

## Scritps

Para se executar os scripts e coletar os dados desde o início é necessário fazê-lo em certas etapas.

### Passo a passo

É recomendável a criação de uma *Virtual Enviroment* (Venv) para que se possa melhor isolas os módulos.

Um dos *scripts* utiliza a biblioteca Python Metar, que pode ser instalada no ambiente por:

`pip install metar`

Com a biblioteca devidamente instalada executa-se ou `/scripts/api-redemet-info.py` ou `/scripts/api-redemet-metar.py`, ou ambos, e por fim, junta-se ambos arquivos gerados com o arquivo `/scripts/api-redemet-juncao`. O primeiro para a coleta de informes meteorológicos assim como medido no aerodromo de interesse (Galeão - localidade: "SBGL"). Vale ressaltar que o script de informações pode demorar até 12 horas para ser executado por completo, enquanto o do Metar pode vir a demorar até 30 minutos.

## Análises

As análises, assim como constam em arquivo .jpynb estão na pasta `analise`, sendo necessário para que possam vir a rodar corretamente que o caminho dos datasets sejam corretamente especificados e que as bibliotecas adequadas sejam corretamente instaladas.