# 2020-02-25 Inês Motta Comarella
# http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r


# Manipulating the outputs ====

# I have 10 species, each generates 8 outputs in each occasion, and there are 4 occasion. So there is a total of 320 outputs.
# I can analyse each occasion separately what will reduce my output to 40 (different species per occasion), but I was also curious about comparing among different occasion, in this case I'll add 10 outputs (different occasions per species)
# First I'll consider one occasion, so I'll compare the species, then I'll compare the different occasions response in each species.

# packages necessary to run "xlsx" package
install.packages(c("rJava", "xlsxjars", "xlsx"))



setx PATH "C:/Program Files (x86)/Java/jre1.8.0_241/bin/server;%PATH%"
Sys.setenv(JAVA_HOME = 'C:\\Program Files (x86)\\Java\\jre1.8.0_241')

library(rJava)
library(xlsxjars)
library(xlsx)


det_models <-
  read.csv(file = "./output/detection-models-10x1-llongicaudis.csv")
det_pVar <-
  read.csv(file = "./output/detection-pVar-10x1-llongicaudis.csv")
det_covar <-
  read.csv(file = "./output/detection-covariates-10x1-llongicaudis.csv")
det_persite <-
  read.csv(file = ".output/detection-persite-10x1-llongicaudis-p(.).csv")
occu_models <-
  read.csv(file = "./output/occupancy-models-10x1-llongicaudis.csv")
occu_pVar <-
  read.csv(file = "./output/occupancy-pVar-10x1-ebarbara.cfm.csv")
occu_persite <-
  read.csv(file = "./output/occupancy-persite-10x1-llongicaudis-p(.)psi(RS3).csv")


write.xlsx2(
  x,
  file,
  sheetName = "Sheet1",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE
)
