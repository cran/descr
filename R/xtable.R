
xtable.CrossTable <- function(x, caption=NULL, label=NULL, align=NULL,
  digits=0, display=NULL, ...)
{
  nt <- cbind(x$t, x$rs)
  nt <- rbind(nt, c(x$cs, NA))
  nt[dim(nt)[1], dim(nt)[2]] <- x$gt
  colnames(nt) <- c(colnames(x$t), gettext("Row Total", domain = "R-descr"))
  rownames(nt) <- c(rownames(x$t), gettext("Column Total", domain = "R-descr"))

  xtable(nt, caption=caption, label=label, align=align, digits=digits,
    display=display, ...)
}

xtable.meanscomp <- function(x, caption=NULL, label=NULL, align=NULL,
  digits=0, display=NULL, ...)
{
  xtable(x$table, caption=caption, label=label, align=align, digits=digits,
    display=display, ...)
}

xtable.freqtable <- function(x,  caption=NULL, label=NULL, align=NULL,
  digits=0, display=NULL, ...)
{
  xtable(x$freqtable, caption=caption, label=label, align=align, digits=digits,
    display=display, ...)
}

