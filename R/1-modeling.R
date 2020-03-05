##### Script modified based on TutorialOcupacaoInes.R sent by Marina Zanin in November 11th, 2019.

#### Single-season single-species occupancy model to unmarked species
#### It follows the stepwise variable selection method
#### The analysis has two parts, the first one is to model the detection, the second one is to model the occupancy based on the detection model selected in the previous step.


# 0. Loading packages  -----
library(readxl)
library(vegan)
library(unmarked)
library(MuMIn)
library(plotrix)


# 1. Importing explanatory variables =====
# This first step is equal to all species

VariaveisExp <-
  read_excel("./data/VariaveisExp.xlsx", sheet = "VarExp")
View(VariaveisExp)
Var <- VariaveisExp[, c(5:8, 10)]
# Binding person and car presence in one single variable
Var <- cbind(Var, VariaveisExp[, 14] + VariaveisExp[, 15])
View(Var)
# Standardizing data
Var <-
  decostand(Var, method = "standardize", MARGIN = 2)


# 2. Importing and preparing species data =====
# For now on the analyses is DIFFERENT to each species

# 2.1. Importing species data -----
cfm <- read_excel("./data/occu-10x1.xlsx",
                  sheet = "sp3")
cfm <- cfm[, -1]
View(cfm)

# 2.2. Matrix to be read by unmarked -----
cfm.umf <- unmarkedFrameOccu(y = cfm, siteCovs = Var)
summary(cfm.umf)


# 3. DETECTION MODELING =====

# 3.1. Evaluate the detection models -----
# if it is null (dec1), if it is influenced by time (dec2), or by the co-variates (dec3)

# Detection bias hipothesis
dec1.cfm <- occu( ~ 1 ~ 1, cfm.umf)
dec2.cfm <- occu( ~ obsNum ~ 1, cfm.umf)
dec3.cfm <- occu( ~ ele + DistBorda_PLAN + RAI_Hum ~ 1, cfm.umf)

# Creating a list of models
dec.list.cfm <-
  fitList(
    "psi(.)p(.)" = dec1.cfm,
    "psi(.)p(t)" = dec2.cfm,
    "psi(.)p(var)" = dec3.cfm
  )
ms.dec.cfm <- modSel(dec.list.cfm)
ms.dec.cfm   # Ordered by AIC


# 3.1.1. Exporting list of detection models ----
# As the modSel output is S4 class method it is not possible to coerse it to dataframe and export
# Fill the dataframe according to the modSel
det_list_df <- data.frame(
  nPars = c(2, 7, 5),
  AIC = c(126.65, 128.49, 133.92),
  delta = c(0.00, 1.84, 7.27),
  AICwt = c(0.701, 0.280, 0.019),
  cumltvWt = c(0.70, 0.98, 1.00),
  Species = "sp3",
  row.names = c("psi(.)p(.)", "psi(.)p(var)", "psi(.)p(t)")
)
det_list_df
write.table(
  det_list_df,
  file = "./output/detection-models-10x1-sp4.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)
# 3.2. Intermediate step -----
# If the detection model selected is influenced by the covariates "psi(.)p(var)", then it is necessary to disintegrate the function dec3 according to Akaike criterion
dd.cfm <- dredge(dec3.cfm)
View(dd.cfm)   # Ordered by AIC

table <- as.matrix(dd.cfm)
importancia.var.cfm <- matrix(NA, nrow = 5, ncol = 6)
rownames(importancia.var.cfm) <-
  c("p(Int)", "psi(Int)", "p(DsB_PLA)", "p(ele)",
    "p(RAI_Hum)")
colnames(importancia.var.cfm) <-
  c("coef.mean",
    "coef.sd",
    "delta.mean",
    "delta.sd",
    "w.mean",
    "w.sd")

for (i in 1:5) {
  temp <- na.omit(table[, c(i, 9, 10)])
  sd.t <- apply(temp, 2, sd)
  mean.t <- apply(temp, 2, mean)
  importancia.var.cfm[i, ] <-
    c(mean.t, sd.t)
}
View(importancia.var.cfm)

# 3.3. Final detection model function -----
# write here the final function ( ~ detection ~ occupancy), consider the occupancy null

#Just to remember the covariates: ~ ele + DistBorda_PLAN + RAI_Hum ~ 1

dec.sel.cfm <-
  occu( ~ 1 ~ 1, cfm.umf)
det.cfm.pred <-
  predict(dec.sel.cfm, type = "det", appendData = TRUE)
colMeans(det.cfm.pred[, 1:4])

# Specifying species and model in the table
sp_det_model <- matrix(NA, nrow = 1, ncol = 2)
colnames(sp_det_model) <- c("Species", "Model")
sp_det_model[,1] <- "sp10"
sp_det_model[,2] <- "p(.)"

# 3.3.1. Exporting final detection model ----
det_final <- t(colMeans(det.cfm.pred[, 1:4]))
det_final_sp <- cbind(det_final, sp_det_model)
det_final_sp

write.table(
  det_final_sp,
  file =
    "./output/detection-final-10x1-sp10-p(.).csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 3.3.2. Exporting detection bias per site ----
det_persite_sp <- cbind(det.cfm.pred, sp_det_model)

write.table(
  det_persite_sp,
  file =
    "./output/detection-persite-10x1-sp10-p(.).csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 5. Exporting data ----

Species <- "sp10"

# Metadata with species names
sp_name <- read_excel("./data/species-names.xlsx")
sp_name


# 5.1. Exporting detection models p(var) ----
dd.cfm_sp <- cbind(dd.cfm, Species)
write.table(
  dd.cfm_sp,
  file = "./output/detection-pVar-10x1-sp10.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 5.2. Exporting detection covariates mean ----
importancia.var.cfm_sp <- cbind(importancia.var.cfm, Species)
write.table(
  importancia.var.cfm_sp,
  file = "./output/detection-covariates-10x1-sp10.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 5.5. Exporting graph 1 ----
png(
  "figs/detection-covariates-10x1-sp10.png",
  res = 300,
  width = 2400,
  height = 2200
)
par(mfrow = c(2, 2))
op <-
  par(
    mfrow = c(2, 2),
    mar = c(4.1, 3.1, 1, 1.1),
    oma = c(0.5, 0.5, 4, 0.5),
    xpd = NA
  )

{
  plotCI(
    x = 1:5,
    y = importancia.var.cfm[, 1],
    uiw = importancia.var.cfm[, 2],
    yaxt = "n",
    xaxt = "n",
    ylab = "Coeficiente de regressão",
    xlab = NA,
    mgp = c(2, 1, 0)
  )
  axis(side = 1,
       at = seq(1, 5),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7,
  )
  text(
    x = seq(1, 5, by = 1),
    par("usr")[3] - 0.25,
    labels = rownames(importancia.var.cfm),
    cex = 0.73,
    srt = 25,
    adj = c(0.8, 1.7)
  )
  }

{
  plotCI(
    x = 1:5,
    y = importancia.var.cfm[, 3],
    uiw = importancia.var.cfm[, 4],
    yaxt = "n",
    xaxt = "n",
    ylab = "delta AIC",
    xlab = NA,
    mgp = c(2, 1, 0)
  )
  axis(side = 1,
       at = seq(1, 5),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, 5, by = 1),
    par("usr")[3],
    labels = rownames(importancia.var.cfm),
    cex = 0.73,
    srt = 25,
    adj = c(0.8, 1.9)
  )
}

{
  plotCI(
    x = 1:5,
    y = importancia.var.cfm[, 5],
    uiw = importancia.var.cfm[, 6],
    yaxt = "n",
    xaxt = "n",
    ylab = "weight",
    xlab = NA,
    mgp = c(2, 1, 0)
  )
  axis(side = 1,
       at = seq(1, 5),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, 5, by = 1),
    par("usr")[3] - 0,
    labels = rownames(importancia.var.cfm),
    cex = 0.73,
    srt = 25,
    adj = c(0.8, 1.9)
  )
}

binomnames.det <-
  expression(bold(paste(
    "Variáveis de detecção - ", italic("Lontra longicaudis"), ""
  )))
title(binomnames.det, line = 1, outer = TRUE)
dev.off()



