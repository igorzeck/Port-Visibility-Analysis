# README *Port Visibility Analysis*

Este é um repositório público contendo os contextos, scripts e datasets finalizados da pesquisa e subsequente criação de um modelo Random Forest para nevoeiros maritmos feita ao CENEP.

## Requisitos do Repositório

Para os scripts do REDEMET é necessário uma chave API que pode ser solicitada [aqui](https://api-redemet.decea.mil.br/cadastro-api/).

Os arquivos podem ser rodados com a inclusão da chave API no arquivo scripts/api-key.txt no diretório raiz.

Mais informações em relação a API REDEMET em específico podem ser encontradas [aqui](https://ajuda.decea.mil.br/base-de-conhecimento/api-redemet-o-que-e/)

## Scritps

Para se executar os scripts (na pasta `scripts`) e coletar os dados desde o início é necessário fazê-lo em certas etapas, essas etapas devem ser seguidas de acordo com a numeração precedente aos scripts (do 01 ao 04).

Recomenda-se para a execução plena e sem empecilhos dos scripts que essa seja feita em um ambiente com todas as bibliotecas listas no arquivo `requirements.txt` ou, na ausência disso, que se execute em um terminal ligado ao ambiente virtual o seguinte comando: `pip install requirements.txt`.

## Metodologia - Passo a Passo

Este trabalho é caracterizado como sendo de natureza quantitativa e de carácter exploratório, sendo que para sua realização foram utilizados métodos de Exploratory Data Analysis (EDA) para a coleta e exploração dos dados. A amostra da pesquisa contempla dados meteorológicos referentes ao entorno dos portos marítimos de Santos e, posteriormente,  de Galeão, coletados hora a hora de aeroportos do local e complementadas por uma base de dados contendo estimativas para a variável temperatura da superfície da água. Portanto, os dados amostrais são pontuais e não-probabilísticos.
Inicialmente, o objeto de pesquisa era o Porto de Santos, de onde os dados seriam coletados e utilizados na aplicação de um posterior modelo Random Forest. Contudo, devido a escassez de dados para o treino do modelo, foi necessária a troca de localidade, que passou a ser o aeroporto de Galeão, estando este assentado em uma local de relativa semelhança à Bacia de Santos.

### Conjunto de Dados
Duas grandes bases de dados foram utilizadas: a primeira o API REDEMET do Departamento de Controle do Espaço Aéreo (DECEA), contendo dentre outras coisas: os Meteorological Aerodrome Reports (METARs) coletados e distribuídos a partir do Aeroporto de Galeão (RJ) e, também, os do Aeroporto de Guarujá (SP); Os dados meteorológicos do aeroporto em si. Já a segunda base de dados foi a reanálise ERA5 do Copernicus Climate Change Service (C3S), serviço implementado pelo ECMWF como parte do The Copernicus Programme.
A escolha dos relatórios METARs de aeroportos costeiros ao invés de dados advindos direto do Porto de Santos se deve a quatro fatores:
Os dados obtidos por meio da autoridade portuária não eram de qualidade adequada ao nosso problema, não sendo extensos e acurados o suficiente;
Os dados coletados localmente por Boias Meteoceanográficas (BMOs) ou tais como as boias da Marinha, não se demonstraram adequados, por motivos melhor detalhado na seção 3.1;
Os dados dos aeroportos são de relativa importância para os pilotos e navegadores, espera-se que essa importância se reflita no rigor de qualidade da coleta e manutenção dos dados;
Os dados dos aeroportos são acessíveis e têm carácter oficial;
Este último fator sendo de grande importância para esta pesquisa, uma vez que se vê necessários os dados de maior qualidade possível para uma melhor viabilidade da eventual aplicação do modelo aqui proposto.
Inicialmente, procurou-se utilizar os dados de um aeródromo nas proximidades do Porto de Santos, sendo a melhor opção a antiga Base Aérea de Santos (BAST) e o atual Aeroporto de Guarujá. Contudo, após uma apuração da integridade e extensão dos dados, percebeu-se que estes careciam de conteúdo o suficiente, não havendo dados para os horários de 22h do dia anterior até 10h do dia seguinte, constituindo um gap (‘buraco’) de extensão e regularidade inaceitável, uma vez que que os dados carecem de coesão temporal, além disso, esses gaps também privam os dados de dinâmicas meteorológicas típicas do período noturno e matutino.
Portanto, a fim de se aproveitar dos dados de qualidade dos aeroportos, sendo estes padronizados e advindos de uma autoridade oficial, foi feita a escolha de pivotar o local da coleta de dados para um aeroporto que satisfizesse duas condições: (1) estava ilhado em uma região costeira semelhante à Bacia de Santos tanto em clima quanto em geografia; (2) estava com seu acervo de dados o mais completo possível, sem gaps regulares. Sendo assim, o local escolhido foi o Aeroporto de Galeão, que se encaixa perfeitamente em ambas as condições propostas.
Como já citado anteriormente esta pesquisa objetiva a eventual criação e aplicação de um modelo Random Forest, para tal procurou-se a maior quantidade de dados de qualidade possíveis, e a API REDEMET não continham por si só dados o suficiente: não havia dados de temperatura da superfície do mar, essenciais para a formação de correntes advectivas e consequente formação de nevoeiros marítimos (KORAČIN et al., 2013), sendo esse fenômeno o foco de nossa pesquisa. Procurou-se retificar esse problema utilizando a base de dados de estimativas do ERA5.

### Variáveis
Os dados do aeroporto de Galeão vieram em formato de Meteorological Aerodrome Report  (METAR), -  informes meteorológicos regulares medidos e registrados em aeródromos (DECEA, 2021) - acessados por meio do API REDEMET do DECEA. Após sua coleta os dados foram formatados em arquivos tabulares Comma Separated Values (CSV) por meio da biblioteca Python “Metar”. Segue abaixo os dados escolhidos e suas descrições, assim como consta no site do DECEA (2020):
- Report time: Dia e hora do informe, complementado com o carimbo de data/hora do requerimento;
- **Visibilidade horizontal** (variável alvo): medida em metros (m), variando de 0 m à 10.000m;
- **Pressão do ar**: convertida em milibar (MB);
- **Temperatura do ar** e do **ponto de orvalho**: ambas medidas em graus Celsius;
- **Direção do vento**: A primeira medida em graus (º) de 0 a 360º;
- **Velocidade do vento**: medida em nós (knots);

Os dados foram requisitados sob posse de uma chave API e coletados ano a ano. Após a extração os dados, codificados em texto, foram decodificados com auxílio da biblioteca Python “Metar”, de onde foram retirados o dia e hora de cada entrada, complementados ou substituídos com os dados dos horários do requerimento à API quando necessário. A biblioteca de decodificação também lidou, dentre outras coisas, com a conversão da velocidade do vento de nós (knots) para m/s (mps).
D
o API REDEMET foram também coletados dados de **umidade relativa**, hora a hora, mensurados no aeroporto de Galeão. Os dados de umidade relativa não são diretamente acessíveis pelos METARs e tiveram que ser coletados separadamente, também sob posse de uma chave API.

Juntou-se todos os METARs de todos os anos do período de interesse da pesquisa (01/01/2011 – 02/07/2025) em um único arquivo CSV, que posteriormente foi estendido com os dados de temperatura da superfície do mar.

omo citado anteriormente, houve também a tentativa da coleta de dados de temperatura da superfície do mar, ausentes nos METARs, essas tentativas foram inicialmente feitas a partir das boias da Marinha, por meio da interface PNBOIA, e mais tarde pelo sistema CHM-BNDO. Essa última se mostrou mais promissora, contudo os dados dela não eram extensos o suficiente, uma vez que compreendiam apenas o período de 2020 a 2025). Houve também a tentativa de obter dados direto do Porto de Santos e dos seus terminais, também sem sucesso.

Contemplando esses problemas, decidiu-se utilizar os dados de estimativas - de alta precisão - da reanálise do ERA5 para um quadrante contendo Galeão (RJ) e Santos (SP). O uso do ERA5 foi aceito levando-se em conta a autoridade e seu grande uso nacional e internacional. A variável extraída foi a Lake mix-layer temperature (LMLT), variável representando, nesse quadrante, a temperatura da superfície de água costeira de ambas regiões (ECMWF, 2025). Selecionamos a variável LMLT para Galeão utilizando as seguintes especificações:
- Product type: Reanalasys;
- Variable: Lake mix-layer Temperature;
- Year: 2011 - 2025;
- Month: January - December;
- Day: 01 - 30;
- Time: 00:00 - 23:00;
- Geographical Area em Sub-region extraction: North: -22.80°, West: -43.27°, South: -22.81°, East: -43.26°;
- Data format: GRIB;
- Download format: Zip.

Após requisitados, os dados foram recebidos em formato General Regularly Distributed Information in Binary Form (GRIB). Conforme a ECMWF (2023) GRIB é um formato de arquivos binários utilizados primariamente para armazenamento de dados meteorológicos. Este formato de dados é a escolha padrão de extensão de arquivos meteorológicos para instituições como o World Meteorological Center (WMO) e o próprio ECMWF.

Utilizou-se uma biblioteca de ferramentas disponibilizada pelo próprio ECMWF para a leitura e posterior conversão dos dados: ecCodes, sendo descrito pelo ECMWF (2023) como sendo uma biblioteca que oferece interface para Python, C, Fortran e ferramentas de command-line (CLI) para lidar com os dados em formato GRIB e análogos. Nesta pesquisa utilizou-se, com auxílio de um computador Linux, as ferramentas de CLI em conjunto com uma variante da “ecCodes” para Python - a biblioteca “Cfgrib” - que em conjunto com a biblioteca “Xarray” pôde realizar a leitura de dados gribs.

Por fim, houve a unificação dos arquivos do ERA5 de LMLT e do arquivo dos METARs de Galeão em um dataset definitivo, de onde as análises foram então realizadas. No entanto, o conjunto gerado continha uma leve assimetria entre os horários dos METARs e das duas outras bases de dados. Os dados contidos nos METARs, em geral, continham os valores pontualmente para uma dada hora (por exemplo 16h, 17h, 18h,...), no entanto, poderiam haver mensagens irregulares (como oriundos de mensagens de correção) para um horário intermediário (por exemplo 16h 17min, 17h, 18h,...). 

Após a junção das bases, esses horários intermediários figuravam como ausentes para as colunas externas ao conjunto dos METARs, nominalmente a de umidade relativa e a de temperatura da superfície da água, preenchidas por valores Not Assigned (NA) nestes casos. Optou-se pela aplicação de um backwards fill (preenchimento retroativo) sobre o dataset, efetivamente repetindo os valores – de trás para a frente – da entrada mais recente sobre a entrada mais antiga, ou seja, para um horário intermediário de 16h 17min, os valores de umidade relativa e temperatura da água seriam preenchidos com os seus valores das 17h.

A aplicação desse preenchimento de dados também resolve outros dois problema, o primeiro seria que os valores “NA” naturais dos datasets são substituídos por valores mais sensíveis do que zeros ou algum outro valor fixo, o segundo é que se mantém uma coesão temporal, sem a remoção de um horário qualquer por ausência de valores. Finalmente, retirou-se “NAs” remanescentes, advindos de as bases não compreenderem a mesma data final e portanto terem valores finais impossíveis de preencher retroativamente.

## Análises

As análises, assim como constam em arquivo .jpynb estão na pasta `analise`, sendo necessário para que possam vir a rodar corretamente que o caminho dos datasets sejam corretamente especificados e que as bibliotecas adequadas sejam corretamente instaladas.

[
**Detalhes da análise em si**
]