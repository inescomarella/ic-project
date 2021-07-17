library(tidyverse)
library(lubridate)

detection.history <- function(species_name, data, window) {
  # Inputs:
  ### species_name: character obj
  ### data: dataframe with the columns "SamplingUnitName", "PhotoDate", "Species"
  ### window: days window for detection
  
  species_name <- as.character(species_name)
  
  re.order <- function(vector) {
    vector <- vector[, colSums(is.na(vector)) != nrow(vector)]
  }
  
  sp <- data %>%
    select(SamplingUnitName, PhotoDate, Species) %>%
    filter(str_detect(Species, species_name))
  
  # Tempo de coleta
  int <- interval(min(as.Date(data$PhotoDate)), max(as.Date(data$PhotoDate)))
  ndays <- time_length(int, "day") + 1 
  days <- seq(min(as.Date(data$PhotoDate)), max(as.Date(data$PhotoDate)), by = "days")
  
  # Número de câmeras
  ncams <- length(unique(data$SamplingUnitName))
  
  # Matriz base para o histórico de coleta
  det_his <- matrix(nrow = ncams, ncol = ndays)
  det_his <- as.data.frame(det_his, row.names = unique(data$SamplingUnitName))
  colnames(det_his) <- days
  
  for (i in 1:ncams) {
    X <- data %>%
      select(SamplingUnitName, PhotoDate) %>%
      filter(SamplingUnitName == unique(data$SamplingUnitName)[i])
    X_days <- seq(min(as.Date(X$PhotoDate)), max(as.Date(X$PhotoDate)), by = "days")
    for (j in 1:ndays) {
      if (as.Date(colnames(det_his)[j]) %in% as.Date(X_days))
        det_his[i,j] <- '0'
    }
  }
  
  # Matriz base da specie
  sp_det_his <- det_his
  
  for (i in 1:ncams) {
    sp_days <- sp %>%
      filter(SamplingUnitName == unique(data$SamplingUnitName)[i])
    sp_days <- sp_days$PhotoDate
    for (j in 1:ndays) {
      if (as.Date(days[j]) %in% as.Date(sp_days))
        sp_det_his[i,j] <- '1'
    }
  }
  colnames(sp_det_his) <- seq(1, ndays, 1) # tirando as datas das colunas
  
  # Reorganizando a planilha para começar com o primeiro dia de monitoriamento
  sp_det_his_reord <- re.order(sp_det_his[1,]) 
  for (j in ncol(sp_det_his_reord):ndays) {
    sp_det_his_reord[,j] <- NA
  }
  colnames(sp_det_his_reord) <- as.character(seq(1, ndays, 1))
  
  for (i in 2:ncams) {
    cam <- re.order(sp_det_his[i,])
    for (j in ncol(cam):ndays) {
      cam[,j] <- NA
    }
    colnames(cam) <- as.character(seq(1, ndays, 1))
    sp_det_his_reord <- bind_rows(sp_det_his_reord, cam)
  }
  
  # Histórico de detecção com janela de detecção de 'window' dias
  det_his_window <- matrix(nrow = ncams, ncol = length(seq(1, ndays, by = window)) - 1) # criando a matriz base
  det_his_window <- as.data.frame(det_his_window, row.names = unique(data$SamplingUnitName)) # Identificando as armadilhas (nomeando as linhas)
  colnames(det_his_window) <- seq(1, length(det_his_window), 1) # Renomeando as colunas 
  
  sp_det_his_window <- det_his_window
  j <- 1
  for (J in seq(1, length(det_his_window)*window, by = window)) {
    for (i in 1:ncams) {
      if (1 %in% sp_det_his_reord[i,J:(J + window - 1)]) {
        sp_det_his_window[i,j] <- '1'
      }
      else {
        if (all(is.na(sp_det_his_reord[i,J:(J + window - 1)]))) {
          sp_det_his_window[i,j] <- NA
        }
        else {
          sp_det_his_window[i,j] <- '0'
        }
      }
    }
    j <- j + 1
  }
  sp_det_his_window
}
