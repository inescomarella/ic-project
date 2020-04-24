Introdução
==========

Modelo de ocupação single-season single-species para espécies não
marcadas de acordo com o proposto por MacKenzie et al. (2002).

Motivação
=========

Essas análises fazem parte do meu projeto de Iniciação Científica
entitulado “Seleção de Hábitat e relações competitivas entre carnívoros
em área fragmentada”.

Premissas para a análise
========================

Seguindo o estabelecido por MacKenzie et al. (2002):

Serão consideradas situações de levantamentos de espécies em N locais
específicos realizados em T ocasiões distintas no tempo. Os locais são
ocupados pelas espécies de interesse durante o período da pesquisa, sem
novos locais sendo ocupados após o início da pesquisa e nenhum local
abandonado antes da interrupção da pesquisa (ou seja, os locais são
“fechados”). Em cada ocasião de amostragem, os pesquisadores usam
métodos de amostragem projetados para detectar as espécies de interesse.
As espécies nunca são falsamente detectadas em um local quando ausentes,
e uma espécie pode ou não ser detectada em um local quando presente. A
detecção das espécies em um local também é considerada independente da
detecção das espécies em todos os outros locais. Os dados resultantes
para cada site podem ser registrados como um vetor de 1 e 0, denotando
detecção e não detecção, respectivamente, para as ocasiões em que o site
foi amostrado. O conjunto de tais históricos de detecção é usado para
estimar a quantidade de interesse, a proporção de locais ocupados pelas
espécies.

Lista de arquivos
======

* R/1-modeling.R

* R/2-exporting.R

* R/3-compiling-results.R

* R/plot-function.R

* data/sp.csv

* data/covariates.csv


Descrição dos arquivos
=======

* **1-modeling.R**: Primeiro script a ser executado, realiza as análises de ocupação de de detecção da espécie. 

* **2-exporting.R**: Exporta as tabelas geradas pelo "1-modeling.R" para a pasta "output", e as imagens para a pasta "figs".

* **3-compiling-results.R**: Compila os resultados gerados pelo "2-exporting.R" em um arquivo único na pasta "results".

* **plot-function.R**: Função para gerar gráficos com o eixo x inclinado, lido no "3-compiling-results.R"

* **sp.csv**: Dados de detecção/não-detecção da espécie. 1 linha para cada site, 1 coluna para cada pesquisa replicada. 1 indica detecção, 0 indica não detecção, células em branco indica sem amostragem.

* **covariates.csv**: Covariáveis para modelos de probabilidade de ocorrência e de detecção. 1 linha para cada site, 1 coluna para cada covariável.



Referências
===========

MacKenzie, Darryl I., James D. Nichols, Gideon B. Lachman, Sam Droege,
Andrew A. Royle, and Catherine A. Langtimm. 2002. “Estimating site
occupancy rates when detection probabilities are less than one.”
*Ecology* 83 (8): 2248–55.
<https://doi.org/10.1890/0012-9658(2002)083%5B2248:ESORWD%5D2.0.CO;2>.
