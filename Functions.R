# This function outputs labels with arrows on a scatterplot
textplot = function (x, y, words, cex = 1, new = TRUE, show.lines = TRUE, dir=-1, ...) 
{
  require('wordcloud')
  if (new) 
    plot(x, y, type = "n", ...)
  lay <- wordlayout(x, y, words, cex, ...)
  lay[,3:4] = lay[,3:4] * 2
  lay[,4] = lay[,4] * dir
  if (show.lines) {
    for (i in 1:length(x)) {
      xl <- lay[i, 1]
      yl <- lay[i, 2]
      w <- lay[i, 3]
      h <- lay[i, 4]
      points(x[i], y[i], pch = 16, col = "red", cex = 1)
      nx <- xl + 0.5 * w
      ny <- yl + 0.5 * h
      lines(c(x[i], nx), c(y[i], ny), col = adjustcolor("grey", alpha.f = 0.5))
      # }
    }
  }
  text(lay[, 1] + 0.5 * lay[, 3], lay[, 2] + 0.5 * lay[, 4], 
       words, cex = cex, ...)
}

