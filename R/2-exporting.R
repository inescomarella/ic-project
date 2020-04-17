source("./R/1-modeling.R")

Species <- "sp"

# Identificando as espécies
sp_name <- read_excel("./data/species-names.xlsx")
sp_name

# 1. Exportando a predição do modelo de detecção ----
# Especificando a espécie na tabela
dec_sel_cfm # confere o modelo

sp_det_model <- matrix(NA, nrow = 1, ncol = 2)
colnames(sp_det_model) <- c("Species", "Model")
sp_det_model[, 1] <- "sp" # especifica a espécie
sp_det_model[, 2] <- "p(RAI_Hum)" # especifica o modelo

det_final <- t(colMeans(det.cfm.pred[, 1:4])) # prepara a tabela
det_final_sp <-
  cbind(det_final, sp_det_model) # identifica a espécie e o modelo da tabela
det_final_sp # confere

write.table(
  det_final_sp,
  file =
    "./output/detection-final-sp.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)


# 2. Exportando a predição da detecção por site ----
det_persite_sp <- cbind(det_cfm_pred, sp_det_model)

write.table(
  det_persite_sp,
  file =
    "./output/detection-persite-sp.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 3. Exportando a predição do modelo de ocupação final ----
ocu_sel_cfm # confere o modelo

# Especificando a espécie e o modelo na tabela
sp_occu_model <- matrix(NA, nrow = 1, ncol = 2)
colnames(sp_occu_model) <- c("Species", "Model")
sp_occu_model[, 1] <- "sp" # especifica a espécie
sp_occu_model[, 2] <- "p(RAI_Hum)psi(.)" # especifica o modelo

occu_final <- t(colMeans(ocu.pred.cfm)) # prepara a tabela
occu_final_sp <-
  cbind(occu_final, sp_occu_model) # identifica a espécie o modelo na tabela

write.table(
  occu_final_sp,
  file =
    "./output/occupancy-final-sp.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)


# 4. Exportando a predição da ocupação por site ----
# Especificando a espécie e o modelo na tabela
occu_persite_sp <- cbind(ocu.pred.cfm, sp_occu_model)

write.table(
  occu_persite_sp,
  file =
    "./output/occupancy-persite-sp.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)
#++++++++++++++++++++++++++

# 5. Exportando modelos de detecção p(.), p(t), p(var) ----
ms_dec_cfm

# Como o output gerado pela função modSel é um objeto de classe S4 não é possível simplesmente exportar os dados, então será necessário escrever o dataframe com os dados para serem exportados

det_list_df <- data.frame(
  nPars = c(5, 2, 7),
  AIC = c(384.75, 416.97, 429.77),
  delta = c(0.00, 32.22, 45.03),
  AICwt = c(1.0e+00, 1.0e-07, 1.7e-10),
  cumltvWt = c(1.00, 1.00, 1.00),
  Species = "sp",
  row.names = c("psi(.)p(var)", "psi(.)p(.)", "psi(.)p(t)")
)
det_list_df
write.table(
  det_list_df,
  file = "./output/detection-models-sp.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)


# 6. Exportando modelos de detecção com base nas variáveis p(var) ----
dd_cfm_sp <- cbind(dd_cfm, Species)
write.table(
  dd_cfm_sp,
  file = "./output/detection-pVar-sp.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 7. Exportando influência das covariáveis na detecção ----
importancia_var_cfm_sp <- cbind(importancia_var_cfm, Species)
write.table(
  importancia_var_cfm_sp,
  file = "./output/detection-covariates-sp.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 8. Exportando modelos de ocupação ----
dd_ocu_cfm_sp <- cbind(dd_ocu_cfm, Species)
write.table(
  dd_ocu_cfm_sp,
  file = "./output/occupancy-psiVar-sp.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)
# 9. Exportando influência das covariáveis na ocupação ----
OCU_importancia_var_cfm_sp <-
  cbind(OCU_importancia_var_cfm, Species)
write.table(
  OCU_importancia_var_cfm_sp,
  file = "./output/occupancy-covariates-sp.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 10. Gráfico da influência das covariáveis na detecção ----
png(
  "figs/detection-covariates-sp.png",
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

binomnames_det <-
  expression(bold(paste(
    "Variáveis de detecção - ", italic("sp"), ""
  )))
title(binomnames_det, line = 1, outer = TRUE)
dev.off()



# 11. Gráfico da influência das covariáveis na ocupação ----
png(
  "figs/occupancy-covariates-sp.png",
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
binomnames_ocu <-
  expression(bold(paste(
    "Variáveis de ocupação - ", italic("sp"), ""
  )))
title(binomnames_ocu, line = 1, outer = TRUE)
dev.off()
