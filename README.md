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

Estrutura de pastas sugerida
============================

O script segue uma estrutura de pastas relativa, como segue:

    .
    ├── _R
    |   ├── 1-modeling.R
    |   └── 2-manipulating-output.R
    ├── _data
    |   ├── VariaveisExp.xlsx
    |   └── occu-7x1.xlsx
    ├── _output
    ├── _figs
    └── _results

Inputs
======

São necessários 2 inputs, 1. as variáveis que serão usadas nos modelos e
a 2. tabela com histórico de detecção.

-   As variáveis devem ser selecionadas com base na biologia da espécie
    e devem ser independentes. Para aprofundamento seleção de variáveis
    recomendo a leitura de Burnham and Anderson (2001).

-   A tabela com o histórico de detecção da espécie deve ser preenchida
    apenas por 0 e 1, sendo que 0 significa não-detecção, em caso
    ausência de dado (por exemplo, no caso de armadilhas fotográficas
    com diferentes períodos de amostragem) a célula deve ser deixada
    vazia.

Script para a leitura dos arquivos em excel, logo é necessário adaptação
em caso de formatos diferentes e em caso de estrutura de pastas
diferentes.

Modelagem
=========

Após o input e a preparação dos dados vem o principal, que é a
modelagem. Ela é dividida em duas etapas, na primeira é estimada a
probabilidade de detectar a espécie e essa probabilidade é usada em uma
segunda etapa para estimar o parâmetro da ocupação.

A forma como iremos nos referir aos modelos será por `psi()p()`, esta
função irá descrever as variáveis explanatórias da detecção -`psi()`- e
da ocupação -`p()`. A ausência de variáveis (modelo nulo) será descrita
como `psi(.)p(.)`

Primeira etapa: Detecção
------------------------


### 1. Avaliando 3 modelos: nulo p(.), viés de tempo de detecção p(t), viés pelas variáveis p(var)

Primeiro vamos estabelecer 3 modelos de detecção fixando a ocupação como
nula. Chamaremos de dec1 o modelo nulo p(.), dec2 o modelo influenciado
pelo tempo de detecção p(t), e dec3 o modelo determinado pela variáveis
p(var). Escreva o nome das variáveis selecionadas para o modelo de
detecção de acordo com o nome nos inputs.

É importante testar o viés do tempo de detecção em caso de diferentes
períodos de detecção, que acontece comumente em levantamentos por
armadilha fotográfica onde as câmeras possuem períodos de atividades
distintos (porque acabou a pilha, ou por diferentes dias de instalação e
remoção, etc.)

    # Hipóteses de modelo de detecção
    dec1.cfm <- occu( ~ 1 ~ 1, cfm.umf) # modelo nulo
    dec2.cfm <- occu( ~ obsNum ~ 1, cfm.umf) # viés de tempo de detecção
    dec3.cfm <- occu( ~ ele + DistBorda_PLAN + RAI_Hum ~ 1, cfm.umf) # viés de detecção pelas covariáveis

A função `occu( ~ detecção ~ ocupação)` faz o encaixe no modelo de
ocupação de estação única (*fits single-season models*) como proposto
por MacKenzie et al. (2002), com base em uma fórmula dupla que descreve
covariáveis de detecção e ocupação nessa ordem.

Após o encaixe dos modelos vem a seleção dos modelos com base no
Critério de Informação de Akaike. A função `modSel` rankeia uma lista de
modelos (nesse caso gerados pela função `fitList`) baseado no AIC.

    # Criando a lista de modelos
    dec.list.cfm <-
      fitList(
        "psi(.)p(.)" = dec1.cfm,
        "psi(.)p(t)" = dec2.cfm,
        "psi(.)p(var)" = dec3.cfm
      )
    ms.dec.cfm <- modSel(dec.list.cfm)
    ms.dec.cfm   # Ordenado pelo AIC

------------------------------------------------------------------------

Esta etapa possui um output, que é a tabela com os 3 modelos rankeados.

### 1.2.A Caso o seja selecionado o modelo psi(.)p(t), ou psi(.)p(.)

Exemplo de uma espécie em que o modelo de detecção é nulo. Neste caso
basta seguir para a etapa final da modelagem de detecção, a **etapa
1.3**.

    ##              nPars    AIC delta AICwt cumltvWt
    ## psi(.)p(.)       2 112.20  0.00 0.916     0.92
    ## psi(.)p(var)     5 117.04  4.83 0.082     1.00
    ## psi(.)p(t)      10 124.47 12.27 0.002     1.00

### 1.2.B1 Caso seja selecionado o modelo psi(.)p(var)

Exemplo de uma espécie em que o modelo de detecção é é determinado pelas
variáveis. Neste caso deve-se avaliar a melhor combinação das
covariáveis, por isso siga para a **etapa 1.2.B2**.

    ##              nPars    AIC delta   AICwt cumltvWt
    ## psi(.)p(var)     5 384.75  0.00 1.0e+00     1.00
    ## psi(.)p(.)       2 416.97 32.22 1.0e-07     1.00
    ## psi(.)p(t)      10 429.77 45.03 1.7e-10     1.00

### 1.2.B2 Gerando modelos de detecção com base nas covariáveis

A seleção da melhor combinação de variáveis que descreverá o modelo de
detecção final envolve o rankeamento dos modelos de detecção com todas
as combinações de covariáveis do modelo global. Para isso usaremos a
função `dredge(global.model)`, que gera uma tabela de seleção de modelos
com combinações (subconjuntos) de termos de efeito fixo no modelo
global. Sendo que o modelo global é o modelo gerado pela função
`occu()`.

    # 3.2. Etapa Intermediária -----
    # Caso o modelo selecionado seja determinado pelas covariáveis psi(.)p(var), então é necessário desagregar a função dec3
    # 
    dd.cfm <- dredge(dec3.cfm)
    dd.cfm   # Ordenado pelo AICc

O resultado é uma tabela como abaixo:

    ## Global model call: occu(formula = ~ele + DistBorda_PLAN + RAI_Hum ~ 1, data = cfm.umf)
    ## ---
    ## Model selection table 
    ##      p(Int) psi(Int) p(DsB_PLA)   p(ele) p(RAI_Hum) df   logLik  AICc delta
    ## 5  0.459000   0.7426                          3.242  3 -188.510 383.6  0.00
    ## 6  0.400100   0.7414     0.1844               3.036  4 -187.419 383.8  0.23
    ## 7  0.414700   0.7423            -0.08051      3.047  4 -188.337 385.7  2.07
    ## 8  0.381500   0.7414     0.1770 -0.04177      2.953  5 -187.374 386.3  2.68
    ## 3 -0.024890   0.7190            -0.29350             3 -203.643 413.9 30.27
    ## 4 -0.027680   0.7183     0.1032 -0.27200             4 -203.287 415.6 31.97
    ## 1  0.014300   0.7197                                 2 -206.483 417.3 33.64
    ## 2  0.006469   0.7162     0.1552                      3 -205.624 417.8 34.23
    ##   weight
    ## 5  0.399
    ## 6  0.355
    ## 7  0.142
    ## 8  0.104
    ## 3  0.000
    ## 4  0.000
    ## 1  0.000
    ## 2  0.000
    ## Models ranked by AICc(x)

Como mais de um modelo se mostrou igualmente parcimonioso então é
necessário selecionar as variáveis com base na influência das mesmas nos
modelos. Para isso iremos olhar a média e desvio padrão dos modelos que
contém cada covariável e com base no efeito da covariável nos modelos
iremos selecionar as covariáveis que farão parte do modelo de detecção
final.

    ##             coef.mean  coef.sd     w.mean       w.sd delta.mean   delta.sd
    ## p(Int)      0.2029278 16.88615 0.12500000 0.22690922  16.784396 0.16521751
    ## psi(Int)    0.7300940 16.88615 0.12500000 0.01267481  16.784396 0.16521751
    ## p(DsB_PLA)  0.1549708 17.27768 0.11485504 0.03665620  18.319232 0.16752413
    ## p(ele)     -0.1719570 16.74593 0.06154655 0.12923023  16.611456 0.07269543
    ## p(RAI_Hum)  3.0693640  1.24514 0.24999995 0.12249276   1.330988 0.14841145

Para melhor visualização ao final do script, na sessão de outputs, é
possível plotar os resultado graficamente.

![](README_files/figure-markdown_strict/unnamed-chunk-4-1.png)

------------------------------------------------------------------------

Esta etapa gera 3 outputs, a tabela com os modelos das combinações de
variáveis, a tabela com a média e desvio padrão das variáveis
separadamente e o gráfico com as mesmas médias e desvios padrões.

### 1.3 Fazendo a predição do modelo

Uma vez que uma função final de modelo de detecção é determinado
segue-se então a última etapa da modelagem de detecção, que é avaliar a
predição do modelo selecionado e a predição da detecção por site.

    dec.sel.cfm <-
      occu( ~ ele + DistBorda_PLAN + RAI_Hum ~ 1, cfm.umf) # Modelo de detecção final
    det.cfm.pred <-
      predict(dec.sel.cfm, type = "det", appendData = TRUE)
    colMeans(det.cfm.pred[, 1:4])

    ##  Predicted         SE      lower      upper 
    ## 0.48348864 0.05283129 0.37494400 0.58288965

------------------------------------------------------------------------

Esta etapa gera 2 outputs, a predição do modelo geral, e a predição da
detcção por site.

Segunda etapa: Ocupação
-----------------------


### 2. Modelo global de ocupação

Mais uma vez será usada a função `occu()`, porém desta vez a ocupação
não será fixa.

Escreva um modelo global para a ocupação com base nas variáveis que
poderiam estar influenciando a ocupação da espécie. Repita a função do
modelo de detecção utilizado na etapa 1.2, de agora em diante a detecção
será fixa, e será avaliada apenas a ocupação.

    # 4.1. Avalie os modelos de ocupação ####
    # Use o modelo de detecção selecionado na etapa anterior 
    # ( ~ detecção ~ ocupação)

    ocu.cfm <-
      occu(~ ele + DistBorda_PLAN + RAI_Hum ~ RS1 + RS2 + RS3 + RAI_Hum, cfm.umf)
    dd.ocu.cfm <- dredge(ocu.cfm)
    View(dd.ocu.cfm) # Ordenado pelo AICc

------------------------------------------------------------------------

Esta etapa gera um output dos modelos de ocupação com diferentes
combinações de covariáveis.

### 2.1.B1 Caso mais de um modelo tenha sido selecionado

Exemplo de uma espécie em que mais de um modelo de ocupação foi
explicativo/parcimonioso. Neste caso deve-se avaliar a selecionar as
covariáveis com base na influência das mesma nos modelos, para isso siga
para a **etapa 2.1.B2**.

    ## Global model call: occu(formula = ~ele + DistBorda_PLAN + RAI_Hum ~ RS1 + RS2 + 
    ##     RS3 + RAI_Hum, data = cfm.umf)
    ## ---
    ## Model selection table 
    ##    p(Int) psi(Int) p(DsB_PLA)   p(ele) p(RAI_Hum) psi(RAI_Hum) psi(RS1)
    ## 5  0.4590   0.7426                          3.242                      
    ## 6  0.4001   0.7414     0.1844               3.036                      
    ## 69 0.4575   0.7865                          3.244                      
    ## 70 0.3990   0.7832     0.1823               3.038                      
    ## 13 0.4600   0.8537                          3.236       0.9408         
    ## 14 0.4009   0.8525     0.1843               3.028       0.9424         
    ## 77 0.4586   0.9001                          3.238       0.9618         
    ## 7  0.4147   0.7423            -0.08051      3.047                      
    ## 21 0.4579   0.7543                          3.242                0.1857
    ## 78 0.4005   0.8984     0.1824               3.033       0.9701         
    ## 37 0.4589   0.7429                          3.242                      
    ##    psi(RS2) psi(RS3) df   logLik  AICc delta weight
    ## 5                     3 -188.510 383.6  0.00  0.162
    ## 6                     4 -187.419 383.8  0.23  0.144
    ## 69            0.4840  4 -187.476 384.0  0.35  0.136
    ## 70            0.4796  5 -186.409 384.4  0.75  0.111
    ## 13                    4 -187.866 384.7  1.13  0.092
    ## 14                    5 -186.774 385.1  1.48  0.077
    ## 77            0.4579  5 -186.934 385.4  1.80  0.066
    ## 7                     4 -188.337 385.7  2.07  0.057
    ## 21                    4 -188.359 385.7  2.11  0.056
    ## 78            0.4535  6 -185.865 385.9  2.33  0.050
    ## 37  0.02186           4 -188.508 386.0  2.41  0.048
    ## Models ranked by AICc(x)

### 2.1.B2 Avaliar as covariáveis isoladamente

Assim como na detecção, qaundo mais de um modelo se mostra igualmente
parcimonioso é necessário selecionar as variáveis com base na influência
das mesmas nos modelos. Para isso olharemos a média e desvio padrão dos
modelos que contém cada covariável e com base no efeito da covariável
nos modelos iremos selecionar quais farão parte do modelo de ocupação
final.

    ##                     coef.mean   coef.sd delta.mean   delta.sd      w.mean
    ## p(Int)             0.20283208 0.4599437        6.0 0.21441217 0.015609627
    ## psi(Int)           0.83361066 0.4599437        6.0 0.06142788 0.015609627
    ## p(DistBorda_PLAN)  0.15281400 0.4581395        6.5 0.03169414 0.015457804
    ## p(ele)            -0.16800833 0.4573566        6.5 0.11459837 0.016176339
    ## p(RAI_Hum)         3.07472804 0.4677997        6.5 0.10443152 0.013639200
    ## psi(RAI_Hum)       0.98103541 0.4471582        6.5 0.02522761 0.008417363
    ## psi(RS1)           0.14024913 0.4592931        6.5 0.01721392 0.015328948
    ## psi(RS2)          -0.01143109 0.4604559        6.5 0.01810391 0.016337504
    ## psi(RS3)           0.45994368 0.4599437        6.0 0.01560963 0.015609627
    ##                       w.sd
    ## p(Int)            1.234427
    ## psi(Int)          1.234427
    ## p(DistBorda_PLAN) 1.135924
    ## p(ele)            1.135924
    ## p(RAI_Hum)        1.135924
    ## psi(RAI_Hum)      1.135924
    ## psi(RS1)          1.135924
    ## psi(RS2)          1.135924
    ## psi(RS3)          1.234427

Para visualizar graficamente basta plotar o gráfico no final do script,
na sessão de exportar outputs. Esta etapa gera 2 outputs, a
tabela com a média e desvio padrão das variáveis separadamente e o
gráfico com as mesmas médias e desvios padrões.

### 2.1.A Caso apenas um modelo seja explicativo

Neste caso apenas prossiga para a etapa final de predição, **etapa
2.2**.

### 2.2. Predição com base modelo de ocupação

Escreva a função contendo as covariáveis selecionada para a detecção e
para a oucpação. Mais uma vez será usada a função `predict()`, o qual
resultará na predição do modelo de ocupação geral e na predição da
ocupação por site.

    ocu.sel.cfm <-
      occu( ~ ele + DistBorda_PLAN + RAI_Hum ~ RS1 + RS3 + RAI_Hum , cfm.umf)
    ocu.pred.cfm <- predict(ocu.sel.cfm, type = "state")
    colMeans(ocu.pred.cfm)

    ## Predicted        SE     lower     upper 
    ## 0.6782023 0.1258738 0.3797087 0.8615871

------------------------------------------------------------------------

Esta etapa gera 2 outputs, a predição do modelo geral, e a predição da
detecção por site.

Outputs
=======

O número de outputs vai depender da espécie. O máximo são de 11 outputs,
todos em .csv. Para visualizar os resultados use o script
2-manipulating-output.R

1.  Predição do modelo de detecção final   
        | ./output/detection-final-7x1-sp1-p().csv   
        
2.  Predição do modelo de detecção final por site    
        | ./output/detection-persite-7x1-sp1-p().csv 
        
3.  Predição do modelo de ocupação final     
        | ./output/occupancy-final-7x1-sp1-p()psi().csv 
        
4.  Predição do modelo de ocupação final por site         
        | ./output/occupancy-persite-7x1-sp1-p()psi().csv    
        
5.  Modelos de detecção p(.), p(t), p(var)        
        | ./output/detection-models-7x1-sp1.csv        
        
6.  Modelos de detecção com base nas variáveis p(var)     
        | ./output/detection-pVar-7x1-sp1.csv      
        
7.  Influência das covariáveis na detecção          
        | ./output/detection-covariates-7x1-sp1.csv     
        
8.  Modelos de ocupação      
        | ./output/occupancy-psiVar-7x1-sp1.csv    
        
9.  Influência das covariáveis na ocupação          
        | ./output/occupancy-covariates-7x1-sp1.csv      
        
10. Gráfico da influência das covariáveis na detecção    
        | ./figs/detection-covariates-7x1-sp1.png     
        
11. Gráfico da influência das covariáveis na ocupação    
        | ./figs/occupancy-covariates-7x1-sp1.png     

------------------------------------------------------------------------

-   Os outputs 1-5 e 8 são comuns a todas as espécies

-   Os outputs 6, 7 e 10 serão gerados apenas para espécies em que a
    detecção foi influenciada pelas variáveis.

-   Os outputs 9 e 11 serão gerados apenas para as espécies em que mais
    de um modelo for explicativo para a ocupação.

Escrevendo resultado final
==========================

Use o script **2-manipulating-output.R** para juntar os outputs num
único arquivo.

É necessário modificar os diretórios e nomes dos arquivos, usando a
estrutura de pastas sugerida basta apenas modificar o nome do arquivo.

Referências
===========

Burnham, Kenneth P., and David R. Anderson. 2001. *Model Selection and
Inference: A Practical Information-Theoretic Approach*. Vol. 65. 3.
<https://doi.org/10.2307/3803117>.

MacKenzie, Darryl I., James D. Nichols, Gideon B. Lachman, Sam Droege,
Andrew A. Royle, and Catherine A. Langtimm. 2002. “Estimating site
occupancy rates when detection probabilities are less than one.”
*Ecology* 83 (8): 2248–55.
<https://doi.org/10.1890/0012-9658(2002)083%5B2248:ESORWD%5D2.0.CO;2>.
