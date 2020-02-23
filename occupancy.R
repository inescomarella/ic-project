###Tutorial ocupacaoo Ines

###Instale os pacotes abaixo antes de carregar.
###Todas as linhas que tem um sinalzinho vermelho, possuem resultados que vc precisar? para preencher a planilha
###Nem todos os dados que vc precisa para a planilha est?o sinalizados, pq simplesmente n?o s?o t?o expl?citos (n?o ? sacanagem minha, rsrsrs)

library(readxl)
library(vegan)
library(unmarked)
library(MuMIn)
library(plotrix)

#########################################################
#1. Importando as vari?veis explanat?rias e co-vari?veis
# Essa etapa ? igual para todas as esp?cies
#########################################################
library(readxl)
VariaveisExp <- read_excel("C:/PROJETO IC/AnalisesR/VariaveisExp.xlsx", sheet = "VarExp")
View(VariaveisExp)
Var<-VariaveisExp[,c(5:8, 10)]
Var<-cbind (Var, VariaveisExp[,14]+VariaveisExp[,15])  ### =(RAI_Vei+RAI_Hum) --> RAI_Hum passa a ser uma vari?vel que leva em considera??o a presen?a de carros
View(Var)
Var<-decostand(Var, method="standardize", MARGIN=2)##padronizando os dados

#########################################################
#2. Importando dados das esp?cies e criando os arquivos para ser rodados
# Essa etapa DIFERE ENTRE ESP?CIES
#########################################################

#2.1. Importando arquivo da esp?cie
library(readxl)

cfm <- read_excel("C:/PROJETO IC/AnalisesR/TABELA DE OCASIAO_7x1.xlsx",
                      sheet = "Canis lupus familiaris")
cfm <-cfm [,-1]
View(cfm)

#2.2. criando uma matriz final para ser lida no unmarked - planilha de dados
cfm.umf <- unmarkedFrameOccu(y=cfm,siteCovs= Var)  	###Organizes detection, non-detection data along with the covariates
summary (cfm.umf) 	###summary() is a generic function used to produce result summaries of various model fitting functions.

#########################################################
#3. Gerando os modelos de detec??o (primeira fun??o que iremos avaliar)
# Essa etapa DIFERE ENTRE ESP?CIES - como exemplo rodei para canis familiaris. Mude o nome dos arquivos
#########################################################

#3.1. Etapa um da detec??o: Avalia??o se a detec??o n?o tem vi?s nulo (dec1), ? influenciada pelo tempo
#de atividade das AFs (dec2), ou pelas co-vari?veis (dec3)

dec1.cfm<-occu(~1~1,cfm.umf)    ###This function fits the single season occupancy model of MacKenzie et al (2002).
dec2.cfm<-occu(~obsNum ~1,cfm.umf)
dec3.cfm<-occu(~ele+DistBorda_PLAN+RAI_Hum ~1, cfm.umf)

        #criar uma lista de modelos
        dec.list.cfm<-fitList("psi(.)p(.)" = dec1.cfm,"psi(.)p(t)" = dec2.cfm,"psi(.)p(var)" = dec3.cfm)
        # Ordenar pelo AIC
        ms.dec.cfm <- modSel(dec.list.cfm)
        ms.dec.cfm

#3.2. Caso o modelo selecionado (delta<2) seja "psi(.)p(var)", desmembrar a fun??o dec3 segundo crit?rio de AIC
dd.cfm<-dredge(dec3.cfm)
dd.cfm ### analizar segundo os crit?rios de akaike
write.table(dd.cfm, file =
              "C:/PROJETO IC/AnalisesR/Resultados 10x1/Deteccao2/llongicaudis-10x1-dd.cfm.csv", sep = ",", row.names = TRUE, col.names = NA)

table<-as.matrix(dd.cfm)

importancia.var.cfm<-matrix(NA, nrow=5, ncol=6)
        rownames(importancia.var.cfm)<-c("p(Int)", "psi(Int)", "p(DsB_PLA)", "p(ele)",
                                         "p(RAI_Hum)")
        colnames(importancia.var.cfm)<-c("coef.mean", "coef.sd", "delta.mean", "delta.sd",
                                         "w.mean", "w.sd")

                        for(i in 1:5){
                                temp<-na.omit(table [,c(i,9, 10)])
                                sd.t<-apply(temp,2,sd)
                                mean.t<-apply(temp,2,mean)
                                importancia.var.cfm[i,]<-c(mean.t, sd.t)
                        }
importancia.var.cfm
write.table(importancia.var.cfm, file =
              "C:/PROJETO IC/AnalisesR/Resultados 10x1/p_var/llongicaudis-10x1-importancia.var.cfm.csv", sep = ",", row.names = TRUE, col.names = NA)

{
par(mfrow=c(2,2))
op <- par(mfrow = c(2,2), mar=c(4.1,3.1,1,1.1), oma = c(0.5,0.5,4,0.5), xpd=NA)

{plotCI(x=1:5, y=importancia.var.cfm[,1], uiw=importancia.var.cfm[,2], yaxt="n", xaxt="n",
      ylab="Coeficiente de regress?o", xlab=NA, mgp=c(2,1,0))
  axis(side=1, at=seq(1,5), labels=FALSE)
  axis(side=2, labels=TRUE, cex.axis=0.7, )
  text(x=seq(1,5, by=1), par("usr")[3]-0.25,
       labels=rownames(importancia.var.cfm),
       cex=0.73, srt=25, adj=c(0.8,1.7))}

{plotCI(x=1:5, y=importancia.var.cfm[,3], uiw=importancia.var.cfm[,4], yaxt="n", xaxt="n",
       ylab="delta AIC", xlab=NA, mgp=c(2,1,0))
  axis(side=1, at=seq(1,5), labels=FALSE)
  axis(side=2, labels=TRUE, cex.axis=0.7)
  text(x=seq(1,5, by=1), par("usr")[3],
       labels=rownames(importancia.var.cfm),
       cex=0.73, srt=25, adj=c(0.8,1.9))}

{plotCI(x=1:5, y=importancia.var.cfm[,5], uiw=importancia.var.cfm[,6], yaxt="n", xaxt="n",
       ylab="weight", xlab=NA, mgp=c(2,1,0))
  axis(side=1, at=seq(1,5), labels=FALSE)
  axis(side=2, labels=TRUE, cex.axis=0.7)
  text(x=seq(1,5, by=1), par("usr")[3]-0,
       labels=rownames(importancia.var.cfm),
       cex=0.73, srt=25, adj=c(0.8,1.9))}

binomnames.det<-expression(bold(paste("Vari?veis de detec??o - ",italic("Eira barbara"),"")))
title(binomnames.det, line = 1, outer = TRUE)
}

~ele+DistBorda_PLAN+RAI_Hum~RS1+RS2+RS3+RAI_Hum

#3.3. Escrevendo fun??o final e avaliando os resultados de vi?s de detec??o
dec.sel.cfm<-occu(~1~1, cfm.umf)##escrever modelo de detec??o final aqui

det.cfm.pred<-predict(dec.sel.cfm, type="det", appendData=TRUE)###valores de detec??o por local, se eu quisezze extrapolar, bastava inserir outra base de dados
colMeans(det.cfm.pred[,1:4])
write.table(det.cfm.pred, file =
              "C:/PROJETO IC/AnalisesR/Resultados 10x1/DeteccaoUA/llongicaudis-10x1-p(.)-det.cfm.pred.csv", sep = ",", row.names = TRUE, col.names = NA)


#########################################
#4.       MODELO DE OCUPA??O
##########################################

#4.1. avaliando vari?veis
#OBS: Primeira fun??o ? a fun??o de detec??o que foi selecionada na etapa anterior. Nesse caso, a fun??o ? a nula
ocu.cfm<-occu(~1~RS1+RS2+RS3+RAI_Hum, cfm.umf)####COLOCAR VARIAVEIS DO modelo de detec??o
dd.ocu.cfm<-dredge(ocu.cfm)
dd.ocu.cfm ### analizar segundo os crit?rios de akaike
write.table(dd.ocu.cfm, file =
              "C:/PROJETO IC/AnalisesR/Resultados 10x1/Ocupacao1/llongicaudis-10x1-dd.ocu.cfm.csv", sep = ",", row.names = TRUE, col.names = NA)


#4.2. Caso haja mais de um modelo como fun??o explanat?ria, fazer a avalia??o baseada na m?dia e intervalo de confian?a
table.ocu<-as.matrix(dd.ocu.cfm)
OCU.importancia.var.cfm<-matrix(NA, nrow=ncol(table.ocu)-5, ncol=6)
rownames(OCU.importancia.var.cfm)<-colnames(table.ocu)[1:(ncol(table.ocu)-5)]

colnames(OCU.importancia.var.cfm)<-c("coef.mean", "coef.sd", "delta.mean", "delta.sd",
                                 "w.mean", "w.sd")

                for(i in 1:(ncol(table.ocu)-5)){
                        temp<-na.omit(table.ocu[, c(i,9, 10)])
                        sd.t<-apply(temp,2,sd)
                        mean.t<-apply(temp,2,mean)
                        OCU.importancia.var.cfm[i,]<-c(mean.t, sd.t)
                }

OCU.importancia.var.cfm
write.table(OCU.importancia.var.cfm, file =
              "C:/PROJETO IC/AnalisesR/Resultados 7x1/Ocupacao2/ebarbara-7x1-OCU.importancia.var.cfm.csv", sep = ",", row.names = TRUE, col.names = NA)

{
par(mfrow=c(2,2))
op <- par(mfrow = c(2,2), mar=c(4.1,3.1,1,1.1), oma = c(0,0,4,0), mgp=c(3,1,0), xpd=NA)

{plotCI(x=1:(ncol(table.ocu)-5), y=OCU.importancia.var.cfm[,1], uiw=OCU.importancia.var.cfm[,2],
         yaxt="n", xaxt="n", ylab="Coeficiente de regress?o", xlab=NA, mgp=c(1.75,0.2,0))
  axis(side=1, at=seq(1,(ncol(table.ocu)-5)), labels=FALSE)
  axis(side=2, labels=TRUE, cex.axis=0.7)
  text(x=seq(1,(ncol(table.ocu)-5), by=1), par("usr")[3]-0.05,
       labels=rownames(OCU.importancia.var.cfm),
       cex=0.73, srt=45, adj=c(0.95,1.5))}

{plotCI(x=1:(ncol(table.ocu)-5), y=OCU.importancia.var.cfm[,3], uiw=OCU.importancia.var.cfm[,4],
        yaxt="n", xaxt="n", ylab="Delta AIC", xlab=NA, mgp=c(1.75,1,0))
  axis(side=1, at=seq(1,(ncol(table.ocu)-5)), labels=FALSE)
  axis(side=2, labels=TRUE, cex.axis=0.7)
  text(x=seq(1,(ncol(table.ocu)-5), by=1), par("usr")[3]-0.01,
       labels=rownames(OCU.importancia.var.cfm),
       cex=0.73, srt=45, adj=c(0.95,1.5))}


{plotCI(x=1:(ncol(table.ocu)-5), y=OCU.importancia.var.cfm[,5], uiw=OCU.importancia.var.cfm[,6],
        yaxt="n", xaxt="n", ylab="Weight", xlab=NA, mgp=c(1.75,1,0))
  axis(side=1, at=seq(1,(ncol(table.ocu)-5)), labels=FALSE)
  axis(side=2, labels=TRUE, cex.axis=0.7)
  text(x=seq(1,(ncol(table.ocu)-5), by=1), par("usr")[3]-0.02,
       labels=rownames(OCU.importancia.var.cfm),
       cex=0.73, srt=45, adj=c(0.9,1.5))}

binomnames.ocu<-expression(bold(paste("Vari?veis de ocupa??o - ",italic("Eira barbara"),"")))
title(binomnames.ocu, line = 1, outer = TRUE)
}
~ele+DistBorda_PLAN+RAI_Hum~RS1+RS2+RS3+RAI_Hum

#4.3. Escrevendo fun??o final e avaliando os resultados da ocupa??o
#OBS: ESCREVER A FUN??O TANTO DE OCUPA??O QUANTO A DE DETEC??O ABAIXO
ocu.sel.cfm<-occu(~1~RS3, cfm.umf)##escrever modelo de ocupacao final aqui
ocu.pred.cfm<-predict(ocu.sel.cfm, type="state")
colMeans(ocu.pred.cfm)
write.table(ocu.pred.cfm, file =
"C:/PROJETO IC/AnalisesR/Resultados 10x1/OcupacaoUA/llongicaudis-10x1-p(.)psi(RS3)-ocu.pred.cfm.csv", sep = ",", row.names = TRUE, col.names = NA)
