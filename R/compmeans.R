# From Hmisc::wtd.var
wtd.sd <- function(x, weights)
{
    xbar <- sum(weights * x)/sum(weights)
    sqrt(sum(weights * ((x - xbar)^2))/(sum(weights) - 1))
}

compmeans <- function(x, f, w, sort = FALSE, maxlevels = 60,
  plot = getOption("descr.plot"), xlab = deparse(substitute(f)),
  ylab = deparse(substitute(x)), ...)
{
  row.label <- attr(f, "label")
  column.label <- attr(x, "label")
  row.name <- deparse(substitute(f))
  column.name <- deparse(substitute(x))

  if(plot){
    if(is.factor(f) && length(levels(f)) < maxlevels){
      boxplot(x ~ f, ylab=ylab, xlab=xlab, ...)
      if(missing(w) == FALSE){
        cat(gettext("Warning:"),
          gettext("boxplot of weighted values not implemented yet."),
          "\n\n")
      }
    }
  }
  f.name <- deparse(substitute(f))
  n.name <- deparse(substitute(x))
  lf <- length(f)
  lx <- length(x)
  if (lf != lx) {
    msg <- paste(f.name, gettext("and"), n.name, gettext("have different lengths"))
    stop(msg)
  }
  if (is.factor(f) == FALSE) {
    f <- factor(f)
    nl <- length(levels(f))
    if (nl > maxlevels) {
      msg <- paste(f.name, gettext("was converted into a factor, but the new variable had too many levels"))
      stop(msg)
    }
    msg <- paste(gettext("Warning:"), " \"", f.name, "\" ", 
      gettext("was converted into factor!"), sep = "")
    cat(msg, "\n")
  } else{
    class(f) <- "factor"
  }
  if(is.numeric(x)){
    class(x) <- "numeric"
  }
  if (missing(w)) {
    w <- rep(1, lf)
  } else {
    lw <- length(w)
    if (lw != lf) {
      msg <- paste(f.name, gettext("and"), "weight", gettext("have different lengths."))
      stop(msg)
    }
  }
  if(is.numeric(w)){
    class(w) <- "numeric"
  }

  if (is.factor(x) == TRUE) {
    x <- as.numeric(x)
    msg <- paste(gettext("Warning:"), " \"", n.name, "\" ", 
      gettext("was converted from factor into numeric!"))
    cat(msg, "\n")
  }
  has.w <- FALSE
  k <- grep(FALSE, (is.na(f) | is.na(x) | is.na(w)))
  f <- f[k]
  x <- x[k]
  w <- w[k]
  lf2 <- length(f)
  if (lf > lf2) {
    cat("\n")
    msg <- gettext("rows with missing values dropped")
    cat((lf - lf2), msg, "\n\n")
  }

  xwsum <- tapply(x*w, f, sum)
  wsum <- tapply(w, f, sum)
  xmean <- xwsum / wsum
  wsum <- round(wsum)
  wsd <- xmean

  flevs <- levels(f)
  nflevs <- length(flevs)
  b <- data.frame(x,f,w)
  for(i in 1:nflevs){
    b2 <- subset(b, f == flevs[i])
    wsd[i] <- wtd.sd(b2$x, b2$w)
  }

  l <- length(xmean)
  xmean[l+1] <- weighted.mean(x, w)
  wsum[l+1] <- round(sum(w))
  wsd[l+1] <- wtd.sd(x, w)
  tab <- cbind(xmean, wsum, wsd)
  tabrn <- rownames(tab)
  tabrn[l+1] <- gettext("Total")
  rownames(tab) <- tabrn
  colnames(tab) <- c(gettext("Mean"), gettext("N"), gettext("Std. Dev."))
  if (sort == TRUE) {
    ordl <- order(xmean)
    tab <- tab[ordl,]
  }
  tab <- list(row = row.name, column = column.name,
    row.label = row.label, column.label = column.label, table = tab)
  class(tab) <- "meanscomp"
  tab
}

print.meanscomp <- function(x, ...)
{
  rlab <- ifelse(is.null(x$row.label), x$row, x$row.label)
  clab <- ifelse(is.null(x$column.label), x$column, x$column.label)

  # 'domain' is necessary because this function is not exported to
  # 'descr' namespace.
  msg1 <- gettext("Mean value of", domain = "R-descr")
  msg2 <- gettext("according to", domain = "R-descr")
  cat(msg1, ' "', clab, '" ', msg2, ' "', rlab, '"\n', sep="")
  print(x$table, ...)
}


