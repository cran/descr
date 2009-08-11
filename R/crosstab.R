
crosstab <- function (x, y, weight = NULL, digits = 3, max.width = 5,
  expected = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE,
  prop.chisq = FALSE, chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
  resid = FALSE, sresid = FALSE, asresid = FALSE, missing.include = FALSE,
  format = "SPSS", dnn = NULL, plot = getOption("descr.plot"), main = "",
  xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
  col = gray.colors(length(levels(y))), ...) 
{
  if (length(weight) == 0) 
    tab <- table(x, y)
  else
    tab <- round(xtabs(weight ~ x + y))
  if (plot) 
    plot(tab, main = main, xlab = xlab, ylab = ylab, col = col, ...)

  tab <- CrossTable(tab, digits = digits, max.width = max.width,
    expected = expected, prop.r = prop.r, prop.c = prop.c, prop.t = prop.t,
    prop.chisq = prop.chisq, chisq = chisq, fisher = fisher, mcnemar = mcnemar,
    resid = resid, sresid = sresid, asresid = asresid,
    missing.include = missing.include, format = format, dnn = dnn)
  tab
}


