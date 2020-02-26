# 2020-02-26 Inês Motta Comarella
# changing the species names

library(openxlsx)

# Species code list, used to name the sheets
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
# Occasion 10x1 ----

# Renaming the sheets
data <- loadWorkbook("./data/occu-10x1.xlsx")
for (i in 1:10) {
  renameWorksheet(data, i, sheet_names[[i]])

}

# Saving the modifications
saveWorkbook(data, "./data/occu-10x1.xlsx", overwrite = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Occasion 7x1 ----

# Renaming the sheets
data <- loadWorkbook("./data/occu-7x1.xlsx")
for (i in 1:10) {
  renameWorksheet(data, i, sheet_names[[i]])

}

# Saving the modifications
saveWorkbook(data, "./data/occu-7x1.xlsx", overwrite = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Occasion 5x1 ----

# Renaming the sheets
data <- loadWorkbook("./data/occu-5x1.xlsx")
for (i in 1:10) {
  renameWorksheet(data, i, sheet_names[[i]])

}

# Saving the modifications
saveWorkbook(data, "./data/occu-5x1.xlsx", overwrite = TRUE)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Occasion 1x1 ----

# Renaming the sheets
data <- loadWorkbook("./data/occu-1x1.xlsx")
for (i in 1:10) {
  renameWorksheet(data, i, sheet_names[[i]])

}

# Saving the modifications
saveWorkbook(data, "./data/occu-1x1.xlsx", overwrite = TRUE)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Generating a species list name -> METADATA

original_names <- getSheetNames("./data/TABELA DE OCASIAO_1x1.xlsx")
metadata_matrix <- cbind(original_names, sheet_names)
metadata_df <- as.data.frame(metadata_matrix)
metadata_df

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
writeDataTable(wb, "Sheet 1", x = metadata_df)
saveWorkbook(wb, "./data/species-names.xlsx", overwrite = TRUE)
openXL("./data/species-names.xlsx")



