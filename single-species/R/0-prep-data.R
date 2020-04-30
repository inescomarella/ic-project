# 2020-02-26 Inês Motta Comarella
# Mudando o nome das espécies
# Esta etapa não faz parte das análises, é apenas para estabelecer um código das espécies e para viabilizar o compartilhamento dos dados

# Importando os dados ----
library(openxlsx)

# Lista com o código dos nomes ----
# Ela será usada para substituir o nome das espécies nas abas da planilha de dados
sheet_names <-
  c("sp1",
    "sp2",
    "sp3",
    "sp4",
    "sp5",
    "sp6",
    "sp7",
    "sp8",
    "sp9",
    "sp10")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ocasião 10x1 ----

# Renomeando as abas
data <- loadWorkbook("./data/occu-10x1.xlsx")
for (i in 1:10) {
  renameWorksheet(data, i, sheet_names[[i]])

}

# Salvando as modificações
saveWorkbook(data, "./data/occu-10x1.xlsx", overwrite = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ocasião 7x1 ----

# Renomeando as abas
data <- loadWorkbook("./data/occu-7x1.xlsx")
for (i in 1:10) {
  renameWorksheet(data, i, sheet_names[[i]])

}

# Renomeando as abas
saveWorkbook(data, "./data/occu-7x1.xlsx", overwrite = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ocasião 5x1 ----

# Renomeando as abas
data <- loadWorkbook("./data/occu-5x1.xlsx")
for (i in 1:10) {
  renameWorksheet(data, i, sheet_names[[i]])

}

# Salvando as modificações
saveWorkbook(data, "./data/occu-5x1.xlsx", overwrite = TRUE)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ocasião 1x1 ----

# Renomeando as abas
data <- loadWorkbook("./data/occu-1x1.xlsx")
for (i in 1:10) {
  renameWorksheet(data, i, sheet_names[[i]])

}

# Salvando as modificações
saveWorkbook(data, "./data/occu-1x1.xlsx", overwrite = TRUE)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# METADADOS ----
# Arquivo com o nome original das espécies

original_names <- getSheetNames("./data/TABELA DE OCASIAO_1x1.xlsx")
metadata_matrix <- cbind(original_names, sheet_names)
metadata_df <- as.data.frame(metadata_matrix)
metadata_df

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
writeDataTable(wb, "Sheet 1", x = metadata_df)
saveWorkbook(wb, "./data/species-names.xlsx", overwrite = TRUE)
openXL("./data/species-names.xlsx") # abrir arquivo



