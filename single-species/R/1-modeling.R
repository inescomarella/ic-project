##### Script baseado no TutorialOcupacaoInes.R enviado pela Marina Zanin em 11 de November 2019.

#### Modelo de ocupação single-season single-species para espécies não-marcadas
#### A análise é dividida em duas etapas, na primeira é estimada a probabilidade de detectar a espécie e essa probabilidade é usada em uma segunda etapa para estimar o parâmetro da ocupação.

# 0. Carregando pacotes  -----
x <- c("readxl", "vegan", "unmarked", "MuMIn", "plotrix")
lapply(x, library, character.only = TRUE)
source("R/plot-function.R")

# 1. IMPORTANDO OS DADOS =====

# 1.1. Importando as variáveis explanatórias =====
Var <- read.csv("./data/covariates.csv")
View(Var)

# Padronizando os dados
Var <- decostand(Var, method = "standardize", MARGIN = 2)


# 1.2. Importando a tabela com histórico de detecção =====
cfm <- read.csv("./data/sp.csv", header = F)
View(cfm)

# Matriz para ser lida pelo unmarked
cfm_umf <- unmarkedFrameOccu(y = cfm, siteCovs = Var)
summary(cfm_umf)


# 2. MODELANDO DA DETECÇÃO =====

# 2.1. Avaliando os modelos de detecção -----
# Modelo nulo p(.), viés de tempo de detecção p(t), viés pelas variáveis p(var)

dec1_cfm <- occu( ~ 1 ~ 1, cfm_umf)
dec2_cfm <- occu( ~ obsNum ~ 1, cfm_umf)
dec3_cfm <-
  occu( ~ ele + DistBorda_PLAN + RAI_Hum ~ 1, cfm_umf) # Escreva as variáveis do modelo de detecção

# Rankeando os modelos
dec_list_cfm <-
  fitList(
    "psi(.)p(.)" = dec1_cfm,
    "psi(.)p(t)" = dec2_cfm,
    "psi(.)p(var)" = dec3_cfm
  )
ms_dec_cfm <- modSel(dec_list_cfm)
ms_dec_cfm # Ordenado pelo AIC

# 2.2. Etapa intermediária -----
# Caso seja selecionado o psi(.)p(var) gere os modelos de detecção a partir da combinação das variáveis do modelo global
dd_cfm <- dredge(dec3_cfm)
View(dd_cfm)   # Ordenado pelo AIC

table <- as.matrix(dd_cfm)
importancia_var_cfm <- matrix(NA, nrow = 5, ncol = 6)
rownames(importancia_var_cfm) <-
  c("p(Int)", "psi(Int)", "p(DsB_PLA)", "p(ele)",
    "p(RAI_Hum)")
colnames(importancia_var_cfm) <-
  c("coef.mean",
    "coef.sd",
    "w.mean",
    "w.sd",
    "delta.mean",
    "delta.sd")

for (i in 1:5) {
  temp <- na.omit(table[, c(i, 9, 10)])
  sd.t <- apply(temp, 2, sd)
  mean.t <- apply(temp, 2, mean)
  importancia_var_cfm[i, ] <-
    c(mean.t, sd.t)
}
View(importancia_var_cfm)

# Plotando num gráfico
plot.angle.label(
  importancia_var_cfm,
  importancia_var_cfm[, 1],
  importancia_var_cfm[, 2],
  "Coeficiente de regressão"
)
plot.angle.label(importancia_var_cfm,
                 importancia_var_cfm[, 3],
                 importancia_var_cfm[, 4],
                 "Peso")
plot.angle.label(importancia_var_cfm,
                 importancia_var_cfm[, 5],
                 importancia_var_cfm[, 6],
                 "Delta AICc")

# 2.3. Modelo de detecção final -----
# ~ detection ~ occupancy, fixe a ocupação como nula
#Apenas para lembrar as covariáveis: ~ ele + DistBorda_PLAN + RAI_Hum ~ 1

dec_sel_cfm <-
  occu( ~ RAI_Hum ~ 1, cfm_umf) # escreva aqui a função de detecção final
det_cfm_pred <-
  predict(dec_sel_cfm, type = "det", appendData = TRUE)
colMeans(det_cfm_pred[, 1:4])



# 3. MODELANDO A OCUPAÇÃO =====

# 3.1. Avaliando os modelos de ocupação ####
# Use o modelo de detecção selecionado na etapa anterior
# occu( ~ detecção ~ ocupação)
# Apenas para lembrar as coraviáveis: ~ ele + DistBorda_PLAN + RAI_Hum ~ RS1 + RS2 + RS3 + RAI_Hum

ocu_cfm <-
  occu(~ RAI_Hum ~ RS1 + RS2 + RS3 + RAI_Hum, cfm_umf)
dd_ocu_cfm <- dredge(ocu_cfm)
View(dd_ocu_cfm) # Ordenado pelo AIC


# 3.2. Etapa intermediária -----
# Caso mais de um modelo tenha sido selecionado, avalie e selecione as covariáveis com base na influência das mesma nos modelos
table_ocu <- as.matrix(dd_ocu_cfm)
OCU_importancia_var_cfm <-
  matrix(NA, nrow = ncol(table_ocu) - 5, ncol = 6)
rownames(OCU_importancia_var_cfm) <-
  colnames(table_ocu)[1:(ncol(table_ocu) - 5)]

colnames(OCU_importancia_var_cfm) <-
  c("coef.mean",
    "coef.sd",
    "w.mean",
    "w.sd",
    "delta.mean",
    "delta.sd")

for (i in 1:(ncol(table_ocu) - 5)) {
  temp <- na.omit(table[, c(i, 9, 10)])
  sd.t <- apply(temp, 2, sd)
  mean.t <- apply(temp, 2, mean)
  OCU_importancia_var_cfm[i, ] <- c(mean.t, sd.t)
}

View(OCU_importancia_var_cfm)

# Plotando num gráfico
plot.angle.label(
  OCU_importancia_var_cfm,
  OCU_importancia_var_cfm[, 1],
  OCU_importancia_var_cfm[, 2],
  "Coeficiente de regressão"
)
plot.angle.label(
  OCU_importancia_var_cfm,
  OCU_importancia_var_cfm[, 3],
  OCU_importancia_var_cfm[, 4],
  "Peso"
)
plot.angle.label(
  OCU_importancia_var_cfm,
  OCU_importancia_var_cfm[, 5],
  OCU_importancia_var_cfm[, 6],
  "Delta AICc"
)

# 3.3. Modelo de ocupação final -----
# occu( ~ detecção ~ ocupação)
# Apenas para lembrar as covariáveis: ~ ele + DistBorda_PLAN + RAI_Hum ~ RS1 + RS2 + RS3 + RAI_Hum

ocu_sel_cfm <-
  occu( ~ RAI_Hum ~ 1 , cfm_umf)
ocu_pred_cfm <- predict(ocu_sel_cfm, type = "state")
colMeans(ocu_pred_cfm)