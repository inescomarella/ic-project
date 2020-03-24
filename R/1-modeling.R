##### Script baseado no TutorialOcupacaoInes.R enviado pela Marina Zanin em 11 de November 2019.

#### Modelo de ocupação single-season single-species para espécies não-marcadas
#### A análise é dividida em duas etapas, na primeira é estimada a probabilidade de detectar a espécie e essa probabilidade é usada em uma segunda etapa para estimar o parâmetro da ocupação.

# 0. Carregando pacotes  -----
x <- c("readxl", "vegan", "unmarked", "MuMIn", "plotrix")
lapply(x, library, character.only = TRUE)
source("R/plot-function.R")

# 1. IMPORTANDO OS DADOS =====

# 1.1. Importando as variáveis explanatórias =====
# Esta primeira etapa é igual para todas as espécies

VariaveisExp <-
  read_excel("./data/VariaveisExp.xlsx", sheet = "VarExp")
View(VariaveisExp)
Var <- VariaveisExp[, c(5:8, 10)]

# Juntando a presença humana e a passagem de carros em uma única variável
Var <- cbind(Var, VariaveisExp[, 14] + VariaveisExp[, 15])
View(Var)
# Padronizando os dados
Var <-
  decostand(Var, method = "standardize", MARGIN = 2)


# 1.2. Importando a tabela com histórico de detecção =====
cfm <- read_excel("./data/occu-7x1.xlsx",
                  sheet = "sp1")
cfm <- cfm[, -1]
View(cfm)

# Matriz para ser lida pelo unmarked
cfm.umf <- unmarkedFrameOccu(y = cfm, siteCovs = Var)
summary(cfm.umf)


# 2. MODELANDO DA DETECÇÃO =====

# 2.1. Avaliando os modelos de detecção -----
# Modelo nulo p(.), viés de tempo de detecção p(t), viés pelas variáveis p(var)

dec1.cfm <- occu(~ 1 ~ 1, cfm.umf)
dec2.cfm <- occu(~ obsNum ~ 1, cfm.umf)
dec3.cfm <-
  occu(~ ele + DistBorda_PLAN + RAI_Hum ~ 1, cfm.umf) # Escreva as variáveis do modelo de detecção

# Rankeando os modelos
dec.list.cfm <-
  fitList(
    "psi(.)p(.)" = dec1.cfm,
    "psi(.)p(t)" = dec2.cfm,
    "psi(.)p(var)" = dec3.cfm
  )
ms.dec.cfm <- modSel(dec.list.cfm)
ms.dec.cfm # Ordenado pelo AIC

# 2.2. Etapa intermediária -----
# Caso seja selecionado o psi(.)p(var) gere os modelos de detecção a partir da combinação das variáveis do modelo global

dd.cfm <- dredge(dec3.cfm)
View(dd.cfm)   # Ordenado pelo AIC

table <- as.matrix(dd.cfm)
importancia.var.cfm <- matrix(NA, nrow = 5, ncol = 6)
rownames(importancia.var.cfm) <-
  c("p(Int)", "psi(Int)", "p(DsB_PLA)", "p(ele)",
    "p(RAI_Hum)")
colnames(importancia.var.cfm) <-
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
  importancia.var.cfm[i,] <-
    c(mean.t, sd.t)
}
View(importancia.var.cfm)

# Plotando num gráfico
plot.angle.label(
  importancia.var.cfm,
  importancia.var.cfm[, 1],
  importancia.var.cfm[, 2],
  "Coeficiente de regressão"
)
plot.angle.label(importancia.var.cfm,
                 importancia.var.cfm[, 3],
                 importancia.var.cfm[, 4],
                 "Peso")
plot.angle.label(importancia.var.cfm,
                 importancia.var.cfm[, 5],
                 importancia.var.cfm[, 6],
                 "Delta AICc")

# 2.3. Modelo de detecção final -----
# ~ detection ~ occupancy, fize a ocupação como nula

#Apenas para lembrar as covariáveis: ~ ele + DistBorda_PLAN + RAI_Hum ~ 1

dec.sel.cfm <-
  occu(~ RAI_Hum ~ 1, cfm.umf) # escreva aqui a função de detecção final
det.cfm.pred <-
  predict(dec.sel.cfm, type = "det", appendData = TRUE)
colMeans(det.cfm.pred[, 1:4])



# 3. MODELANDO A OCUPAÇÃO =====

# 3.1. Avaliando os modelos de ocupação ####
# Use o modelo de detecção selecionado na etapa anterior
# occu( ~ detecção ~ ocupação)
# Paenas para lembrar as coraviáveis: ~ ele + DistBorda_PLAN + RAI_Hum ~ RS1 + RS2 + RS3 + RAI_Hum

ocu.cfm <-
  occu( ~ RAI_Hum ~ RS1 + RS2 + RS3 + RAI_Hum, cfm.umf)
dd.ocu.cfm <- dredge(ocu.cfm)
View(dd.ocu.cfm) # Ordenado pelo AIC


# 3.2. Etapa intermediária -----
# Caso mais de um modelo tenha sido selecionado, avalie e selecione as covariáveis com base na influência das mesma nos modelos
table.ocu <- as.matrix(dd.ocu.cfm)
OCU.importancia.var.cfm <-
  matrix(NA, nrow = ncol(table.ocu) - 5, ncol = 6)
rownames(OCU.importancia.var.cfm) <-
  colnames(table.ocu)[1:(ncol(table.ocu) - 5)]

colnames(OCU.importancia.var.cfm) <-
  c("coef.mean",
    "coef.sd",
    "w.mean",
    "w.sd",
    "delta.mean",
    "delta.sd")

for (i in 1:(ncol(table.ocu) - 5)) {
  temp <- na.omit(table[, c(i, 9, 10)])
  sd.t <- apply(temp, 2, sd)
  mean.t <- apply(temp, 2, mean)
  OCU.importancia.var.cfm[i,] <- c(mean.t, sd.t)
}

View(OCU.importancia.var.cfm)

# Plotando num gráfico
plot.angle.label(
  OCU.importancia.var.cfm,
  OCU.importancia.var.cfm[, 1],
  OCU.importancia.var.cfm[, 2],
  "Coeficiente de regressão"
)
plot.angle.label(
  OCU.importancia.var.cfm,
  OCU.importancia.var.cfm[, 3],
  OCU.importancia.var.cfm[, 4],
  "Peso"
)
plot.angle.label(
  OCU.importancia.var.cfm,
  OCU.importancia.var.cfm[, 5],
  OCU.importancia.var.cfm[, 6],
  "Delta AICc"
)

# 3.3. Modelo de ocupação final -----
# occu( ~ detecção ~ ocupação)
# Apenas para lembrar as covariáveis: ~ ele + DistBorda_PLAN + RAI_Hum ~ RS1 + RS2 + RS3 + RAI_Hum

ocu.sel.cfm <-
  occu(~ RAI_Hum ~ 1 , cfm.umf)
ocu.pred.cfm <- predict(ocu.sel.cfm, type = "state")
colMeans(ocu.pred.cfm)


# 4. EXPORTANDO OS OUTPUTS =====

Species <- "sp1"

# Identificando as espécies
sp_name <- read_excel("./data/species-names.xlsx")
sp_name

# 4.1. Exportando a predição do modelo de detecção ----
# Especificando a espécie na tabela
dec.sel.cfm # confere o modelo

sp_det_model <- matrix(NA, nrow = 1, ncol = 2)
colnames(sp_det_model) <- c("Species", "Model")
sp_det_model[, 1] <- "sp1" # especifica a espécie
sp_det_model[, 2] <- "p(RAI_Hum)" # especifica o modelo

det_final <- t(colMeans(det.cfm.pred[, 1:4])) # prepara a tabela
det_final_sp <-
  cbind(det_final, sp_det_model) # identifica a espécie e o modelo da tabela
det_final_sp # confere

write.table(
  det_final_sp,
  file =
    "./output/detection-final-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)


# 4.2. Exportando a predição da detecção por site ----
det_persite_sp <- cbind(det.cfm.pred, sp_det_model)

write.table(
  det_persite_sp,
  file =
    "./output/detection-persite-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 4.3. Exportando a predição do modelo de ocupação final ----
ocu.sel.cfm # confere o modelo

# Especificando a espécie e o modelo na tabela
sp_occu_model <- matrix(NA, nrow = 1, ncol = 2)
colnames(sp_occu_model) <- c("Species", "Model")
sp_occu_model[, 1] <- "sp1" # especifica a espécie
sp_occu_model[, 2] <- "p(RAI_Hum)psi(.)" # especifica o modelo

occu_final <- t(colMeans(ocu.pred.cfm)) # prepara a tabela
occu_final_sp <-
  cbind(occu_final, sp_occu_model) # identifica a espécie o modelo na tabela

write.table(
  occu_final_sp,
  file =
    "./output/occupancy-final-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)


# 4.4. Exportando a predição da ocupação por site ----
# Especificando a espécie e o modelo na tabela
occu_persite_sp <- cbind(ocu.pred.cfm, sp_occu_model)

write.table(
  occu_persite_sp,
  file =
    "./output/occupancy-persite-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)
#++++++++++++++++++++++++++

# 4.5. Exportando modelos de detecção p(.), p(t), p(var) ----
ms.dec.cfm

# Como o output gerado pela função modSel é um objeto de classe S4 não é possível simplesmente exportar os dados, então será necessário escrever o dataframe com os dados para serem exportados

det_list_df <- data.frame(
  nPars = c(5, 2, 7),
  AIC = c(384.75, 416.97, 429.77),
  delta = c(0.00, 32.22, 45.03),
  AICwt = c(1.0e+00, 1.0e-07, 1.7e-10),
  cumltvWt = c(1.00, 1.00, 1.00),
  Species = "sp1",
  row.names = c("psi(.)p(var)", "psi(.)p(.)", "psi(.)p(t)")
)
det_list_df
write.table(
  det_list_df,
  file = "./output/detection-models-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)


# 4.6. Exportando modelos de detecção com base nas variáveis p(var) ----
dd.cfm_sp <- cbind(dd.cfm, Species)
write.table(
  dd.cfm_sp,
  file = "./output/detection-pVar-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 4.7. Exportando influência das covariáveis na detecção ----
importancia.var.cfm_sp <- cbind(importancia.var.cfm, Species)
write.table(
  importancia.var.cfm_sp,
  file = "./output/detection-covariates-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 4.8. Exportando modelos de ocupação ----
dd.ocu.cfm_sp <- cbind(dd.ocu.cfm, Species)
write.table(
  dd.ocu.cfm_sp,
  file = "./output/occupancy-psiVar-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)
# 4.9. Exportando influência das covariáveis na ocupação ----
OCU.importancia.var.cfm_sp <-
  cbind(OCU.importancia.var.cfm, Species)
write.table(
  OCU.importancia.var.cfm_sp,
  file = "./output/occupancy-covariates-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 4.10. Gráfico da influência das covariáveis na detecção ----
png(
  "figs/detection-covariates-sp1.png",
  res = 300,
  width = 2400,
  height = 2200
)
op <-
  par(
    mfrow = c(2, 2),
    mar = c(4.1, 3.1, 1, 1.1),
    oma = c(0.5, 0.5, 4, 0.5)
  )
plot.angle.label(
  importancia.var.cfm,
  importancia.var.cfm[, 1],
  importancia.var.cfm[, 2],
  "Coeficiente de regressão"
)
plot.angle.label(importancia.var.cfm,
                 importancia.var.cfm[, 3],
                 importancia.var.cfm[, 4],
                 "Peso")
plot.angle.label(importancia.var.cfm,
                 importancia.var.cfm[, 5],
                 importancia.var.cfm[, 6],
                 "Delta AICc")

binomnames.det <-
  expression(bold(paste(
    "Variáveis de detecção - ", italic("sp1"), ""
  )))
title(binomnames.det, line = 1, outer = TRUE)
dev.off()



# 4.11. Gráfico da influência das covariáveis na ocupação ----
png(
  "figs/occupancy-covariates-sp1.png",
  res = 300,
  width = 2400,
  height = 2200
)
op <-
  par(
    mfrow = c(2, 2),
    mar = c(4.1, 3.1, 1, 1.1),
    oma = c(0.5, 0.5, 4, 0.5)
  )
plot.angle.label(
  OCU.importancia.var.cfm,
  OCU.importancia.var.cfm[, 1],
  OCU.importancia.var.cfm[, 2],
  "Coeficiente de regressão"
)
plot.angle.label(
  OCU.importancia.var.cfm,
  OCU.importancia.var.cfm[, 3],
  OCU.importancia.var.cfm[, 4],
  "Peso"
)
plot.angle.label(
  OCU.importancia.var.cfm,
  OCU.importancia.var.cfm[, 5],
  OCU.importancia.var.cfm[, 6],
  "Delta AICc"
)
binomnames.ocu <-
  expression(bold(paste(
    "Variáveis de ocupação - ", italic("sp1"), ""
  )))
title(binomnames.ocu, line = 1, outer = TRUE)
dev.off()
