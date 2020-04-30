source("./R/1-modeling.R")

# 1. Exportando a predição do modelo de detecção ----
det_final <- t(colMeans(det_cfm_pred[, 1:4])) # prepara a tabela

write.table(
  det_final,
  file =
    "./output/detection-predict.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)


# 2. Exportando a predição da detecção por site ----
write.table(
  det_persite,
  file =
    "./output/detection-predict-persite.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 3. Exportando a predição do modelo de ocupação final ----
occu_final <- t(colMeans(ocu_pred_cfm)) # prepara a tabela

write.table(
  occu_final,
  file =
    "./output/occupancy-predict.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 4. Exportando a predição da ocupação por site ----
write.table(
  ocu_pred_cfm,
  file =
    "./output/occupancy-predict-persite.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)
#++++++++++++++++++++++++++

# 5. Exportando modelos de detecção p(.), p(t), p(var) ----
ms_dec_cfm

# Como o output gerado pela função modSel é um objeto de classe S4 não é possível simplesmente exportar o objeto, então será necessário escrever o dataframe com os dados para serem exportados

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
  file = "./output/detection-models.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)


# 6. Exportando modelos de detecção com base nas variáveis p(var) ----
write.table(
  dd_cfm,
  file = "./output/detection-models-pVar.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 7. Exportando modelos de ocupação ----
write.table(
  dd_ocu_cfm,
  file = "./output/occupancy-models.csv",
  sep = ",",
  row.names = TRUE,
  col.names = NA
)

# 8. Gráfico da influência das covariáveis na detecção ----
png(
  "figs/detection-covariates.png",
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



# 9. Gráfico da influência das covariáveis na ocupação ----
png(
  "figs/occupancy-covariates.png",
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
