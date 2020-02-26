# 2020-02-25 Inês Motta Comarella
# http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r


# Manipulating the outputs ====

# I have 10 species, each generates 8 outputs in each occasion, and there are 4 occasion. So there is a total of 320 outputs.
# I can analyse each occasion separately what will reduce my output to 40 (different species per occasion), but I was also curious about comparing among different occasion, in this case I'll add 10 outputs (different occasions per species)
# First I'll consider one occasion, so I'll compare the species, then I'll compare the different occasions response in each species.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# IMPORTANTE: versão atualizada do Java, com mesma arquitetura do R (no meu caso, x64 bit)
# Após baixado rode o comando no terminal:
setx PATH "C:/Program Files/Java/jre1.8.0_241/bin/server;%PATH%"
# Em seguida no R, rode:
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_241')
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# packages necessary to run "xlsx" package
install.packages(c("rJava", "xlsxjars", "xlsx"))

library(rJava)
library(xlsxjars)
library(xlsx)

# Lendo os dados da primeira espécie para criar o arquivo
det_models <-
  read.csv(file = "./output/detection-models-10x1-sp1.csv")
det_pVar <-
  read.csv(file = "./output/detection-pVar-10x1-sp1.csv")
det_covar <-
  read.csv(file = "./output/detection-covariates-10x1-sp1.csv")
det_persite <-
  read.csv(file = ".output/detection-persite-10x1-sp1-p(.).csv")
occu_models <-
  read.csv(file = "./output/occupancy-models-10x1-sp1.csv")
occu_pVar <-
  read.csv(file = "./output/occupancy-pVar-10x1-sp1.cfm.csv")
occu_persite <-
  read.csv(file = "./output/occupancy-persite-10x1-sp1-p(.)psi(RS3).csv")

# Criando o arquivo e as páginas de acordo com os dados da primeira espécie
xlsx.writeMultipleData <-
  function ("./results/occupancy-model-OCCASION",
            c(det_models,
              det_pVar,
              detcovar,
              det_persite,
              occu_models,
              occu_pVar,
              occu_persite))
  {
    require(xlsx, quietly = TRUE)
    objects <-
      list(det_models,
           det_pVar,
           detcovar,
           det_persite,
           occu_models,
           occu_pVar,
           occu_persite)
    fargs <- as.list(match.call(expand.dots = TRUE))
    objnames <- as.character(fargs)[-c(1, 2)]
    nobjects <- length(objects)
    for (i in 1:nobjects) {
      if (i == 1)
        write.xlsx(objects[[i]], file, sheetName = objnames[i])
      else
        write.xlsx(objects[[i]], file, sheetName = objnames[i],
                   append = TRUE)
    }
  }


# Lendos os dados das outras espécies e adicionando às sheets
# Para reconhecer o nome parcial dos arquivos
det_models <- apropos("detection-models")
n_det_models <- length(det_models)
for (i in 1:n_det_models) {
  if (i == 1)
    write.x
  lsx(det_models[[i]], file = "./results/occupancy-model-OCCASION", sheetName = objnames[i])
  else
    addDataFrame(det_model[[1]],
                 det_models,
                 startRow  =
                 startColumn = 1)
}


####
