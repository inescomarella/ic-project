# 2020-02-25 Inês Motta Comarella
# http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r


# Unindo os outputs ====

# Carregando os pacotes ----
x <- c("readxl", "openxlsx", "data.table")
lapply(x, library, character.only = TRUE)


# Lendo os outputs ----
## files.path é um vetor de diretórios, não leu os arquivos ainda, tem apenas os diretórios
occu_final_path <-
  list.files(path = "./output",
             pattern = "occupancy-final-7x1",
             full.names = TRUE)
occu_final_data <- lapply(occu_final_path, read.csv)
occu_final <- rbindlist(occu_final_data)

occu_persite_path <-
  list.files(path = "./output",
             pattern = "occupancy-persite-7x1",
             full.names = TRUE)
occu_persite_data <- lapply(occu_persite_path, read.csv)
occu_persite <- rbindlist(occu_persite_data, fill = TRUE)

occu_cov_path <-
  list.files(path = "./output",
             pattern = "occupancy-covariates-7x1",
             full.names = TRUE)
occu_cov_data <- lapply(occu_cov_path, read.csv)
occu_cov <- rbindlist(occu_cov_data)

occu_psiVar_path <-
  list.files(path = "./output",
             pattern = "occupancy-psiVar-7x1",
             full.names = TRUE)
occu_psiVar_data <- lapply(occu_psiVar_path, read.csv)
occu_psiVar <- rbindlist(occu_psiVar_data, fill =  TRUE)

det_final_path <-
  list.files(path = "./output",
             pattern = "detection-final-7x1",
             full.names = TRUE)
det_final_data <- lapply(det_final_path, read.csv)
det_final <- rbindlist(det_final_data)

det_persite_path <-
  list.files(path = "./output",
             pattern = "detection-persite-7x1",
             full.names = TRUE)
det_persite_data <- lapply(det_persite_path, read.csv)
det_persite <- rbindlist(det_persite_data, fill = TRUE)

det_models_path <-
  list.files(path = "./output",
             pattern = "detection-models-7x1",
             full.names = TRUE)
det_models_data <- lapply(det_models_path, read.csv)
det_models <- rbindlist(det_models_data, fill = TRUE)

det_cov_path <-
  list.files(path = "./output",
             pattern = "detection-covariates-7x1",
             full.names = TRUE)
det_cov_data <- lapply(det_cov_path, read.csv)
det_cov <- rbindlist(det_cov_data)

det_pVar_path <-
  list.files(path = "./output",
             pattern = "detection-pVar-7x1",
             full.names = TRUE)
det_pVar_data <- lapply(det_pVar_path, read.csv)
det_pVar <- rbindlist(det_pVar_data, fill = TRUE)


# Criando o arquivo dos resultado e as abas ----
wb <- createWorkbook("./results/result-7x1.xlsx")
addWorksheet(wb, "occu_final")
addWorksheet(wb, "occu_persite")
addWorksheet(wb, "occu_cov")
addWorksheet(wb, "occu_psiVar")
addWorksheet(wb, "det_final")
addWorksheet(wb, "det_persite")
addWorksheet(wb, "det_models")
addWorksheet(wb, "det_cov")
addWorksheet(wb, "det_pVar")

# Escrevendo os dados em cada aba ----
writeData(wb, "occu_final", occu_final)
writeData(wb, "occu_persite", occu_persite)
writeData(wb, "occu_cov", occu_cov)
writeData(wb, "occu_psiVar", occu_psiVar)
writeData(wb, "det_final", det_final)
writeData(wb, "det_persite", det_persite)
writeData(wb, "det_models", det_models)
writeData(wb, "det_cov", det_cov)
writeData(wb, "det_pVar", det_pVar)
saveWorkbook(wb, "./results/result-detection-7x1.xlsx")

# Abrindo o arquivo com os resultados ----
openXL("./results/result-7x1.xlsx")

