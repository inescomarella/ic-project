# 2020-02-25 Inês Motta Comarella
# http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r


# Unindo os outputs ====

# Carregando os pacotes ----
x <- c("readxl", "openxlsx", "data.table")
lapply(x, library, character.only = TRUE)

# Lendo os outputs ----
## files.path é um vetor de diretórios, não leu os arquivos ainda, tem apenas os diretórios
occu_predict_path <-
list.files(path = "./output",
           pattern = "occupancy-predict",
           full.names = TRUE)
occu_predict_data <- lapply(occu_predict_path, read.csv)
occu_predict <- rbindlist(occu_predict_data, fill = TRUE)

occu_persite_path <-
list.files(path = "./output",
           pattern = "occupancy-predict-persite",
           full.names = TRUE)
occu_persite_data <- lapply(occu_persite_path, read.csv)
occu_persite <- rbindlist(occu_persite_data, fill = TRUE)

occu_models_path <-
list.files(path = "./output",
           pattern = "occupancy-models",
           full.names = TRUE)
occu_models_data <- lapply(occu_models_path, read.csv)
occu_models <- rbindlist(occu_models_data, fill =  TRUE)

det_predict_path <-
list.files(path = "./output",
           pattern = "detection-predict",
           full.names = TRUE)
det_predict_data <- lapply(det_predict_path, read.csv)
det_predict <- rbindlist(det_predict_data, fill = TRUE)

det_persite_path <-
list.files(path = "./output",
           pattern = "detection-predict-persite",
           full.names = TRUE)
det_persite_data <- lapply(det_persite_path, read.csv)
det_persite <- rbindlist(det_persite_data, fill = TRUE)

det_models_path <-
list.files(path = "./output",
           pattern = "detection-models",
           full.names = TRUE)
det_models_data <- lapply(det_models_path, read.csv)
det_models <- rbindlist(det_models_data, fill = TRUE)

det_pVar_path <-
list.files(path = "./output",
           pattern = "detection-models-pVar",
           full.names = TRUE)
det_pVar_data <- lapply(det_pVar_path, read.csv)
det_pVar <- rbindlist(det_pVar_data, fill = TRUE)


# Criando o arquivo dos resultado e as abas ----
wb <- createWorkbook()
addWorksheet(wb, "det_models", gridLines = FALSE)
addWorksheet(wb, "det_models_pVar", gridLines = FALSE)
addWorksheet(wb, "det_pred", gridLines = FALSE)
addWorksheet(wb, "det_persite", gridLines = FALSE)
addWorksheet(wb, "occu_models", gridLines = FALSE)
addWorksheet(wb, "occu_pred", gridLines = FALSE)
addWorksheet(wb, "occu_persite", gridLines = FALSE)

# Escrevendo os dados em cada aba ----
writeData(wb, "det_models", det_models)
writeData(wb, "det_models_pVar", det_pVar)
writeData(wb, "det_pred", det_predict)
writeData(wb, "det_persite", det_persite)
writeData(wb, "occu_models", occu_models)
writeData(wb, "occu_pred", occu_predict)
writeData(wb, "occu_persite", occu_persite)

# Estabelecendo o estilo -----
headerStyle <-
  createStyle(
    textDecoration = "bold",
    border = "TopBottom",
    bgFill = "white",
    fgFill = "white",
    fontName = "TIMES",
    halign = "center",
    fontColour = "black",
    borderStyle = "THIN"
  )

bodyStyle <- createStyle(
  numFmt = "0.000",
  fontName = "TIMES",
  halign = "center",
  fontColour = "black",
  borderStyle = "none"
)

bottom <- createStyle(
  numFmt = "0.000",
  fontName = "TIMES",
  halign = "center",
  fontColour = "black",
  border = "bottom",
  borderStyle = "thin"
)

# Aplicando o estilo ----
for (curr_sheet in sheets(wb)) {
  x <- readWorkbook(wb, curr_sheet)
  addStyle(
    wb,
    curr_sheet,
    bodyStyle,
    rows = 2:(nrow(x) + 1),
    cols = 1:(ncol(x)),
    gridExpand = T
  )
  addStyle(
    wb,
    curr_sheet,
    headerStyle,
    rows = 1,
    cols = 1:(ncol(x)),
    gridExpand = T
  )
  addStyle(
    wb,
    curr_sheet,
    bottom,
    rows = (nrow(x) + 1),
    cols = 1:(ncol(x)),
    gridExpand = T
  )

}

# Salvando o arquivo ----
saveWorkbook(wb, "./results/results.xlsx", overwrite = TRUE)

# Abrindo o arquivo com os resultados ----
openXL("./results/results.xlsx")
