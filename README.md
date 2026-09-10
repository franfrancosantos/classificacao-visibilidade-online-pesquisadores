# Classificação da Visibilidade Online de pesquisadores

Código utilizado para calcular os indicadores de Cobertura e Intensidade altmétrica de um conjunto de 786.968 pesquisadores (ORCIDs) extraídos da OpenAlex.
Os indicadores são utilizados em uma classificação de visibilidade online proposta.
Para tanto foram relacionados aos pesquisadores todos os DOIs rastreados na OpenAlex (totalizando 19.658.734 DOIs únicos), 
O cálculo dos indicadores altmétricos considerou os 7.428.060 DOIs com atenção capturada pela Altmetric e 18.387.092 que tinham leitores registrados no Mendeley. 

Portanto, esse código:
1) Realiza a estatística descretiva dos dados.
2) Calcula a Cobertura altmétrica por pesquisador, considerando as menções aos seus DOIs no X, em Notícias, Patentes e leitores no Mendeley.
3) Calcula a Intensidade altmétrica por pesquisador, considerando as menções aos seus DOIs no X, em Notícias, Patentes e leitores no Mendeley..
4) Categoriza a visibilidade dos pesquisadores entre alta ou baixa de acordo com a mediana da área do pesquisador.
5) Classifica os pesquisadores nos quadrantes de visibilidade considerando os níveis de Coberuta e Intensidade em cada uma das fontes.
6) Gera os gráficos de dispersão e as tabelas com os níveis de visibilidade dos pesquisadores agrupados por área, em cada uma das fontes altmétricas.
7) Classifica os pesquuisadores em 10 classes de visibilidade online propostas.

Os arquivos base para rodar o código estão disponíveis em:
