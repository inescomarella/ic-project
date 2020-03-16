# 2020-03-15 Inês Comarella

# Carregando os pacotes ----
x <- c("data.table", "dplyr", "formattable", "tidyr", "webshot", "htmltools")
lapply(x, library, character.only = TRUE)


# Função para exportar tabela do formattable ----
#' Export a Formattable as PNG, PDF, or JPEG
#'
#' @param f A formattable.
#' @param file Export path with extension .png, .pdf, or .jpeg.
#' @param width Width specification of the html widget being exported.
#' @param height Height specification of the html widget being exported.
#' @param background Background color specification.
#' @param delay Time to wait before taking webshot, in seconds.
#'
#' @importFrom formattable as.htmlwidget
#' @importFrom htmltools html_print
#' @importFrom webshot webshot
#'
#' @export
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}



# Gráfico do histórico de detecção ----
png(
  "figs/observations-sp7.png",
  res = 150,
  width = 1000,
  height = 712.5
)
plot(cfm.umf, xlab = "Ocasiões")

dev.off()

# Tabela modelos de detecção p(.) p(t) p(var) -----
det_list_df <- data.frame(
  nPar = c(5, 2, 7),
  AIC = c(195.42, 205.94, 218.21),
  "delta AIC" = c(0.00, 10.52, 22.79),
  Peso = c(0.99, 0.0052, 0.000011),
  row.names = c("psi(.)p(var)", "psi(.)p(.)", "psi(.)p(t)")
)

FT.sp7 <- formattable(
  det_list_df,
  align = c("l", "c", "c", "c", "c", "c", "c", "c", "r"),
  list(`Indicator Name` = formatter(
    "span", style = ~ style(color = "grey", font.weight = "bold")
  ))
)

export_formattable(FT.sp7,"./figs/det-models-sp7.png", width = "50%")




# Tabela modelos de detecção parametrizado ---



# Tabela modelos de detecção covariáveis -----
colnames(dd.cfm) <- c("p(Int)", "psi(Int)", "p(DistBorda_PLAN)", "p(ele)", "p(RAI_Hum)", "nPar", "logLik", "AICc", "delta AICc", "Peso")
det.par.model <- formattable(
  dd.cfm,
  align = c("l", "c", "c", "c", "c", "c", "c", "c", "r"),
  list(`Indicator Name` = formatter(
    "span", style = ~ style(color = "grey", font.weight = "bold")
  ))
)
export_formattable(det.par.model,"./doc/simbioma/fig-felinos/det-par-model-sp7.png", width = "100%")


# Gráfico variáveis da detecção ----
png(
  "doc/simbioma/fig-felinos/detection-covariates-sp7.png",
  res = 200,
  width = 1500,
  height = 600
)
par(mfrow = c(1, 3))
op <-
  par(
    mfrow = c(1, 3),
    mar = c(4.1, 3.1, 1, 1.1),
    oma = c(0.5, 0.5, 4, 0.5),
    xpd = NA
  )

{
  plotCI(
    x = 1:5,
    y = importancia.var.cfm[, 1],
    uiw = importancia.var.cfm[, 2],
    yaxt = "n",
    xaxt = "n",
    ylab = "Coeficiente de regressão",
    xlab = NA,
    mgp = c(2, 1, 0)
  )
  axis(side = 1,
       at = seq(1, 5),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7,
  )
  text(
    x = seq(1, 5, by = 1),
    par("usr")[3] - 0.25,
    labels = rownames(importancia.var.cfm),
    cex = 0.73,
    srt = 25,
    adj = c(0.8, 1.7)
  )
  }

{
  plotCI(
    x = 1:5,
    y = importancia.var.cfm[, 3],
    uiw = importancia.var.cfm[, 4],
    yaxt = "n",
    xaxt = "n",
    ylab = "Peso",
    xlab = NA,
    mgp = c(2, 1, 0)
  )
  axis(side = 1,
       at = seq(1, 5),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, 5, by = 1),
    par("usr")[3],
    labels = rownames(importancia.var.cfm),
    cex = 0.73,
    srt = 25,
    adj = c(0.8, 1.9)
  )
}

{
  plotCI(
    x = 1:5,
    y = importancia.var.cfm[, 5],
    uiw = importancia.var.cfm[, 6],
    yaxt = "n",
    xaxt = "n",
    ylab = "delta AICc",
    xlab = NA,
    mgp = c(2, 1, 0)
  )
  axis(side = 1,
       at = seq(1, 5),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, 5, by = 1),
    par("usr")[3] - 0,
    labels = rownames(importancia.var.cfm),
    cex = 0.73,
    srt = 25,
    adj = c(0.8, 1.9)
  )
}

dev.off()


# Tabela modelos de ocupação ----


# Gráfico variáveis da ocupação ----
png(
  "figs/occupancy-covariates-7x1-sp7.png",
  res = 200,
  width = 1500,
  height = 600
)
par(mfrow = c(1,3))
op <-
  par(
    mfrow = c(1, 3),
    mar = c(4.1, 3.1, 1, 1.1),
    oma = c(0.5, 0.5, 4, 0.5),
    xpd = NA
  )
{
  plotCI(
    x = 1:(ncol(table.ocu) - 5),
    y = OCU.importancia.var.cfm[, 1],
    uiw = OCU.importancia.var.cfm[, 2],
    yaxt = "n",
    xaxt = "n",
    ylab = "Coeficiente de regressão",
    xlab = NA,
    mgp = c(1.75, 0.2, 0)
  )
  axis(side = 1,
       at = seq(1, (ncol(table.ocu) - 5)),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, (ncol(table.ocu) - 5), by = 1),
    par("usr")[3] - 0.05,
    labels = rownames(OCU.importancia.var.cfm),
    cex = 0.73,
    srt = 45,
    adj = c(0.95, 1.5)
  )
  }

{
  plotCI(
    x = 1:(ncol(table.ocu) - 5),
    y = OCU.importancia.var.cfm[, 3],
    uiw = OCU.importancia.var.cfm[, 4],
    yaxt = "n",
    xaxt = "n",
    ylab = "weight",
    xlab = NA,
    mgp = c(1.75, 1, 0)
  )
  axis(side = 1,
       at = seq(1, (ncol(table.ocu) - 5)),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, (ncol(table.ocu) - 5), by = 1),
    par("usr")[3] - 0.01,
    labels = rownames(OCU.importancia.var.cfm),
    cex = 0.73,
    srt = 45,
    adj = c(0.95, 1.5)
  )
}

{
  plotCI(
    x = 1:(ncol(table.ocu) - 5),
    y = OCU.importancia.var.cfm[, 5],
    uiw = OCU.importancia.var.cfm[, 6],
    yaxt = "n",
    xaxt = "n",
    ylab = "delta AIC",
    xlab = NA,
    mgp = c(1.75, 1, 0)
  )
  axis(side = 1,
       at = seq(1, (ncol(table.ocu) - 5)),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, (ncol(table.ocu) - 5), by = 1),
    par("usr")[3] - 0.02,
    labels = rownames(OCU.importancia.var.cfm),
    cex = 0.73,
    srt = 45,
    adj = c(0.9, 1.5)
  )
}

dev.off()


# Tabela predição do modelo de detecção e do modelo de ocupação ----
