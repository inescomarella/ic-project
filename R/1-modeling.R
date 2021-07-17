#### Modelo de ocupação single-season single-species para espécies não-marcadas

# 0. Carregando pacotes  -----
xfun::pkg_attach(c("readxl", "vegan", "unmarked", "MuMIn", "plotrix", "Rcpp"))

source("./R/detection.history.R")

# 1. BASE DE DADOS ---------------------------

# Importando variáveis explanatórias
Var <- read.csv("./data/covariates.csv")

# Importando histórico de detecção
rec_data <- read.csv("./data/camtrap_records_example.csv", header = T)

# Padronizando variáveis explanatórias
Var <- decostand(Var, method = "standardize", MARGIN = 2)

# Padronizando variáveis explanatórias
cfm <- detection.history(species_name = "Didelphis aurita", data =  rec_data, window = 7)

# Convert columns to numeric
cfm <- sapply(cfm, as.numeric)

# Matriz para ser lida pelo unmarked
cfm_umf <- unmarkedFrameOccu(y = cfm, siteCovs = Var)
summary(cfm_umf)

# 2. MODELANDO DA DETECÇÃO ---------------------------

# Avaliando os modelos de detecção
# Modelo nulo p(.), viés de tempo de detecção p(t), viés pelas variáveis p(var)
dec1_cfm <- occu( ~ 1 ~ 1, data = cfm_umf)
dec2_cfm <- occu( ~ obsNum ~ 1, cfm_umf)
dec3_cfm <-
  occu( ~ ele + DistBorda_PLAN + RAI_Hum ~ 1, cfm_umf) # Escreva as variáveis do modelo de detecção

# Rankeando os modelos
dec_list_cfm <-
  fitList(
    "psi(.)p(.)" = dec1_cfm,
    "psi(.)p(t)" = dec2_cfm,
    "psi(.)p(var)" = dec3_cfm
  )

ms_dec_cfm <- modSel(dec_list_cfm)

ms_dec_cfm # Ordenado pelo AIC

# 3. MODELANDO A OCUPAÇÃO ---------------------------

# Avaliando os modelos de ocupação ####
# Use o modelo de detecção selecionado na etapa anterior

ocu_cfm <-
  occu(~ 1 ~ RS1 + RS2 + RS3 + RAI_Hum, cfm_umf)

# Recombinação das covariáveis do modelo global
dd_ocu_cfm <- dredge(ocu_cfm)

# View selected models
dd_ocu_cfm %>% 
  filter(delta < 2)

# Modelo de ocupação final
ocu_sel_cfm <-
  occu( ~ 1 ~ 1 , cfm_umf)

# 4. Predict ---------------------------

# Detection
det_pred_cfm <-predict(dec1_cfm, type = "det", appendData = TRUE)
colMeans(det_pred_cfm[, 1:4])

# Occupancy
ocu_pred_cfm <- predict(ocu_sel_cfm, type = "state")
colMeans(ocu_pred_cfm)