# 2020-02-25 Inês Motta Comarella
# http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r


# Manipulating the outputs ====

# I have 10 species, each generates 8 outputs in each occasion, and there are 4 occasion. So there is a total of 320 outputs.
# I can analyse each occasion separately what will reduce my output to 40 (different species per occasion), but I was also curious about comparing among different occasion, in this case I'll add 10 outputs (different occasions per species)
# First I'll consider one occasion, so I'll compare the species, then I'll compare the different occasions response in each species.

# Loading packages ----
library("readxl")
library("openxlsx")
library("data.table")


# Reading outputs ----
## files.path é um vetor de diretórios, não leu os arquivos ainda, tem apenas os diretórios
occu_final_path <-
  list.files(path = "./output",
             pattern = "occupancy-final-10x1",
             full.names = TRUE)
occu_final_data <- lapply(occu_final_path, read.csv)
occu_final <- rbindlist(occu_final_data)

occu_persite_path <-
  list.files(path = "./output",
             pattern = "occupancy-persite-10x1",
             full.names = TRUE)
occu_persite_data <- lapply(occu_persite_path, read.csv)
occu_persite <- rbindlist(occu_persite_data, fill = TRUE)

occu_cov_path <-
  list.files(path = "./output",
             pattern = "occupancy-covariates-10x1",
             full.names = TRUE)
occu_cov_data <- lapply(occu_cov_path, read.csv)
occu_cov <- rbindlist(occu_cov_data)

occu_psiVar_path <-
  list.files(path = "./output",
             pattern = "occupancy-psiVar-10x1",
             full.names = TRUE)
occu_psiVar_data <- lapply(occu_psiVar_path, read.csv)
occu_psiVar <- rbindlist(occu_psiVar_data, fill =  TRUE)

det_final_path <-
  list.files(path = "./output",
             pattern = "detection-final-10x1",
             full.names = TRUE)
det_final_data <- lapply(det_final_path, read.csv)
det_final <- rbindlist(det_final_data)

det_persite_path <-
  list.files(path = "./output",
             pattern = "detection-persite-10x1",
             full.names = TRUE)
det_persite_data <- lapply(det_persite_path, read.csv)
det_persite <- rbindlist(det_persite_data, fill = TRUE)

det_models_path <-
  list.files(path = "./output",
             pattern = "detection-models-10x1",
             full.names = TRUE)
det_models_data <- lapply(det_models_path, read.csv)
det_models <- rbindlist(det_models_data, fill = TRUE)

det_cov_path <-
  list.files(path = "./output",
             pattern = "detection-covariates-10x1",
             full.names = TRUE)
det_cov_data <- lapply(det_cov_path, read.csv)
det_cov <- rbindlist(det_cov_data)

det_pVar_path <-
  list.files(path = "./output",
             pattern = "detection-pVar-10x1",
             full.names = TRUE)
det_pVar_data <- lapply(det_pVar_path, read.csv)
det_pVar <- rbindlist(det_pVar_data, fill = TRUE)


# Creating the result file and sheets ----
wb <- createWorkbook("./results/result-10x1.xlsx")
addWorksheet(wb, "occu_final")
addWorksheet(wb, "occu_persite")
addWorksheet(wb, "occu_cov")
addWorksheet(wb, "occu_psiVar")
addWorksheet(wb, "det_final")
addWorksheet(wb, "det_persite")
addWorksheet(wb, "det_models")
addWorksheet(wb, "det_cov")
addWorksheet(wb, "det_pVar")

# Writing the final results ----
writeData(wb, "occu_final", occu_final)
writeData(wb, "occu_persite", occu_persite)
writeData(wb, "occu_cov", occu_cov)
writeData(wb, "occu_psiVar", occu_psiVar)
writeData(wb, "det_final", det_final)
writeData(wb, "det_persite", det_persite)
writeData(wb, "det_models", det_models)
writeData(wb, "det_cov", det_cov)
writeData(wb, "det_pVar", det_pVar)
saveWorkbook(wb, "./results/result-detection-10x1.xlsx")

# Opening the results file ----
openXL("./results/result-detection-10x1.xlsx")

