##### Script modified based on TutorialOcupacaoInes.R sent by Marina Zanin in November 11th, 2019.
##### This is script aims to model occupancy based heterogeneous sites, it was developed originally to run camera-tra data of carnivores in a fragmented landscape
##### The analysis are made individually to each species. The script has two parts, the first part is to develop the model detections and to analyse the influence of covariates in the species detection per site, the second part is to develop the occupancy models itself based on the detection model selected in the previous step.


# 0. Loading packages -----
library(readxl)
library(vegan)
library(unmarked)
library(MuMIn)
library(plotrix)


# 1. Importing explanatory variables and covariates =====
# This first step is equal to all species

VariaveisExp <-
  read_excel("./data/VariaveisExp.xlsx", sheet = "VarExp")
View(VariaveisExp)
Var <- VariaveisExp[, c(5:8, 10)]
Var <- cbind(Var, VariaveisExp[, 14] + VariaveisExp[, 15])
View(Var)
Var <-
  decostand(Var, method = "standardize", MARGIN = 2) # standardizing data


# 2. Importing and preparing species data =====
# From now on the analyses is DIFFERENT to each species

# 2.1. Importing species data -----
cfm <- read_excel("./data/occu-10x1.xlsx",
                  sheet = "sp1")
cfm <- cfm[, -1]
View(cfm)

# Metadata with species names
sp_name <- read_excel("./data/species-names.xlsx")
sp_name


# 2.2. Final matrix to be read by unmarked - data table -----
cfm.umf <- unmarkedFrameOccu(y = cfm, siteCovs = Var)
summary(cfm.umf)


# 3. DETECTION MODELING =====

# 3.1. Evaluate the detection bias -----
# if it is null (dec1), if it is influenced by time (dec2), or by the co-variates (dec3)

dec1.cfm <- occu(~ 1 ~ 1, cfm.umf)
dec2.cfm <- occu(~ obsNum ~ 1, cfm.umf)
dec3.cfm <- occu(~ ele + DistBorda_PLAN + RAI_Hum ~ 1, cfm.umf)

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
# Fill the dataframe ocording to the previous table
det_list_df <- data.frame(nPars = c(5, 2, 7),
                 AIC = c(85.26, 92.54, 100.95),
                 delta = c(0.00, 7.28, 15.69),
                 AICwt = c(0.97406, 0.02556, 0.00038),
                 cumltvWt = c(0.97, 1.00, 1.00),
                 Species = c("sp2", NA, NA),
                 row.names = c("psi(.)p(var)", "psi(.)p(.)", "psi(.)p(t)"))
det_list_df
write.table(
  det_list_df,
  file = "./output/detection-models-10x1-sp2.csv",
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
  importancia.var.cfm[i,] <-
    c(mean.t, sd.t)
}
View(importancia.var.cfm)






#Just to remember the covariates: ~ ele + DistBorda_PLAN + RAI_Hum ~ 1

# 3.3. Final detection model function -----
# write here the final function ( ~ detection ~ occupancy), consider the occupancy null
dec.sel.cfm <-
  occu(~ ele + RAI_Hum ~ 1, cfm.umf)
det.cfm.pred <-
  predict(dec.sel.cfm, type = "det", appendData = TRUE)
colMeans(det.cfm.pred[, 1:4])


# 3.3.1. Exporting final model detection ----
det_final <- t(colMeans(det.cfm.pred[, 1:4]))

# Specifying the model in the table
rownames(det_final) <- "p(ele + RAI_Hum)"

# Specifying the species in the table
det_final_sp <- cbind(det_final, c("sp2"))
View(det_final_sp)

write.table(
  det_final_sp,
  file =
    "./output/detection-final-10x1-sp2-p(ele-RAI_Hum).csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)
# 3.3.2. Exporting detection bias per site ----
# Specifiying the species and the model in the table
sp_model <- matrix(NA, nrow = 1, ncol = 2)
colnames(sp_model) <- c("sp2", "p(ele + RAI_Hum)")
det_persite_sp <- cbind(det.cfm.pred, sp_model)
View(det_persite_sp)

write.table(
  det_persite_sp,
  file =
    "./output/detection-persite-10x1-sp2-p(ele-RAI_Hum).csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)






# 4. OCCUPANCY MODELING =====

# 4.1. Evaluate the occupancy ####
# Use the detection model function selected in the previous step ( ~ detection ~ occupancy)

ocu.cfm <-
  occu( ~ ele + DistBorda_PLAN + RAI_Hum ~ RS1 + RS2 + RS3 + RAI_Hum, cfm.umf)
dd.ocu.cfm <- dredge(ocu.cfm)
View(dd.ocu.cfm) # Ordered by AIC


# 4.2. Intermediate step -----
# If more than one model is an explanatory function (according to AIC), then evaluate the models based on the mean and standard deviation

table.ocu <- as.matrix(dd.ocu.cfm)
OCU.importancia.var.cfm <-
  matrix(NA, nrow = ncol(table.ocu) - 5, ncol =
           6)
rownames(OCU.importancia.var.cfm) <-
  colnames(table.ocu)[1:(ncol(table.ocu) - 5)]

colnames(OCU.importancia.var.cfm) <-
  c("coef.mean",
    "coef.sd",
    "delta.mean",
    "delta.sd",
    "w.mean",
    "w.sd")

for (i in 1:(ncol(table.ocu) - 5)) {
  temp <- na.omit(table.ocu[, c(i, 9, 10)])
  sd.t <- apply(temp, 2, sd)
  mean.t <- apply(temp, 2, mean)
  OCU.importancia.var.cfm[i,] <-
    c(mean.t, sd.t)
}
View(OCU.importancia.var.cfm)




# Just to remember the covariates: ~ ele + DistBorda_PLAN + RAI_Hum ~ RS1 + RS2 + RS3 + RAI_Hum

# 4.3. Final occupancy model function -----
# Write here the final function ( ~ detection ~ occupancy), based on previous  step
ocu.sel.cfm <-
  occu(~ 1 ~ RS1 + RS2, cfm.umf)
ocu.pred.cfm <- predict(ocu.sel.cfm, type = "state")
colMeans(ocu.pred.cfm)
View(ocu.pred.cfm)


# 4.3.1. Exporting final occupancy model ----

occu_final <- t(colMeans(ocu.pred.cfm))

# Specifying the model in the table
rownames(occu_final) <- "p(.)psi(RS1 + RS2)"

# Specifying the species in the table
occu_final_sp <- cbind(occu_final, c("sp2"))
View(occu_final_sp)

write.table(
  occu_final_sp,
  file =
    "./output/occupancy-final-10x1-sp2-p(.)psi(RS1-RS2).csv",
  ## CHANGE SPECIES AND P( )
  sep = ",",
  row.names = TRUE,
  col.names = NA
)
# 4.3.2. Exporting occupancy per site ----
# Specifiying the species and the model in the table
sp_model_occu <- matrix(NA, nrow = 1, ncol = 2)
colnames(sp_model_occu) <- c("sp2", "p(.)psi(RS1 + RS2)")
occu_persite_sp <- cbind(ocu.pred.cfm, sp_model_occu)
View(occu_persite_sp)

write.table(
  occu_persite_sp,
  file =
    "./output/occupancy-persite-10x1-sp2-p(.)psi(RS1-RS2).csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. Exporting data ----
# CHANGE SPECIES AND P( )PSI( )

# 5.1. Exporting covariate detection model  ----
View(dd.cfm)   # Ordered by AIC
dd.cfm_sp <- cbind(dd.cfm, "sp1")
View(dd.cfm_sp)
write.table(
  dd.cfm_sp,
  file = "./output/detection-pVar-10x1-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 5.2. Exporting covariates bias on detection ----
View(importancia.var.cfm)
importancia.var.cfm_sp <- cbind(importancia.var.cfm, "sp1")
write.table(
  importancia.var.cfm_sp,
  file = "./output/detection-covariates-10x1-sp1.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 5.3. Exporting graph 1 ----
png(
  "figs/detection-covariates-10x1-sp2.png",
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
       cex.axis = 0.7,)
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
    "Variáveis de detecção - ", italic("Cerdocyon thous"), ""  ## CHANGE THE SPECIES
  )))
title(binomnames.det, line = 1, outer = TRUE)
dev.off()

# 5.4. Exporting occupancy models ----
View(dd.ocu.cfm)
dd.ocu.cfm_sp <- cbind(dd.ocu.cfm, "sp2")
write.table(
  dd.ocu.cfm_sp,
  file = "./output/occupancy-psiVar-10x1-sp2.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)



# 5.5. Exporting covariate influence in occupancy ----
OCU.importancia.var.cfm_sp <- cbind(OCU.importancia.var.cfm, "sp2")
write.table(
  OCU.importancia.var.cfm_sp,
  file = "./output/occupancy-covariates-10x1-sp2.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)
# 5.6. Exporting graph 2 ----
png(
  "figs/occupancy-covariates-10x1-sp2.png",
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
    x = 1:(ncol(table.ocu) - 5),
    y = OCU.importancia.var.cfm[, 1],
    uiw = OCU.importancia.var.cfm[, 2],
    yaxt = "n",
    xaxt = "n",
    ylab = "Coeficiente de regressão",
    xlab = NA,
    mgp = c(1.75, 0.2, 0)
  )
  axis(side = 1,
       at = seq(1, (ncol(table.ocu) - 5)),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, (ncol(table.ocu) - 5), by = 1),
    par("usr")[3] - 0.05,
    labels = rownames(OCU.importancia.var.cfm),
    cex = 0.73,
    srt = 45,
    adj = c(0.95, 1.5)
  )
  }

{
  plotCI(
    x = 1:(ncol(table.ocu) - 5),
    y = OCU.importancia.var.cfm[, 3],
    uiw = OCU.importancia.var.cfm[, 4],
    yaxt = "n",
    xaxt = "n",
    ylab = "Delta AIC",
    xlab = NA,
    mgp = c(1.75, 1, 0)
  )
  axis(side = 1,
       at = seq(1, (ncol(table.ocu) - 5)),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, (ncol(table.ocu) - 5), by = 1),
    par("usr")[3] - 0.01,
    labels = rownames(OCU.importancia.var.cfm),
    cex = 0.73,
    srt = 45,
    adj = c(0.95, 1.5)
  )
}

{
  plotCI(
    x = 1:(ncol(table.ocu) - 5),
    y = OCU.importancia.var.cfm[, 5],
    uiw = OCU.importancia.var.cfm[, 6],
    yaxt = "n",
    xaxt = "n",
    ylab = "Weight",
    xlab = NA,
    mgp = c(1.75, 1, 0)
  )
  axis(side = 1,
       at = seq(1, (ncol(table.ocu) - 5)),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, (ncol(table.ocu) - 5), by = 1),
    par("usr")[3] - 0.02,
    labels = rownames(OCU.importancia.var.cfm),
    cex = 0.73,
    srt = 45,
    adj = c(0.9, 1.5)
  )
}

binomnames.ocu <-
  expression(bold(paste(
    "Variáveis de ocupação - ", italic("Cerdocyon thous"), ""
  )))
title(binomnames.ocu, line = 1, outer = TRUE)
dev.off()
