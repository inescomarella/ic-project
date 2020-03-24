plot.angle.label <- function(table, mean, sd, ylab){
  par(xpd = NA)
  plotCI(
    x = 1:nrow(table),
    y = mean,
    uiw = sd,
    yaxt = "n",
    xaxt = "n",
    ylab = as.character(ylab),
    xlab = NA,
    mgp = c(1.75, 0.2, 0)
  )
  axis(side = 1,
       at = seq(1, nrow(table)),
       labels = FALSE)
  axis(side = 2,
       labels = TRUE,
       cex.axis = 0.7)
  text(
    x = seq(1, nrow(table), by = 1),
    par("usr")[3] - 0.05,
    labels = rownames(table),
    cex = 0.73,
    srt = 45,
    adj = c(0.95, 1.5)
  )
}

