
freq <- function (x, w, plot = getOption("descr.plot"), y.axis = "count", ...)
{
  xlab <- attr(x, "label", TRUE)
  if (is.factor(x) == FALSE) {
    x <- as.factor(x)
  }
  if (missing(w)) 
    w <- rep(1, length(x))
  nmiss <- sum(is.na(x))
  misssum <- nmiss
  xlevels <- levels(x)
  l <- length(xlevels)
  if (nmiss) {
    w2 <- w
    w2[is.na(x) == FALSE] <- 0
    misssum <- sum(w2)
    l <- l + 1
    xlevels[l] <- "NA's"
    x <- as.numeric(x)
    x[is.na(x)] <- l
    x <- factor(x, labels = xlevels)
  }
  xnames <- levels(x)
  xnames[l + 1] <- gettext("Total")
  xfreq <- tapply(w, x, sum)
  xtotal <- sum(xfreq)
  xpercent <- 100 * xfreq/xtotal
  xfreq[l + 1] <- sum(xfreq)
  xpercent[l + 1] <- sum(xpercent)
  if (nmiss) {
    xtotal <- xtotal - misssum
    xvalper <- 100 * xfreq/xtotal
    xcumsum <- cumsum(xvalper)
    xcumsum[l] <- NA
    xcumsum[l+1] <- NA
    xvalper[l] <- 0
    xvalper[l + 1] <- 0
    xvalper[l + 1] <- sum(xvalper)
    xvalper[l] <- NA
    xfreq <- round(xfreq)
    ftab <- cbind(xfreq, xpercent, xvalper, xcumsum)
    colnames(ftab) <- c(gettext("Frequency"), gettext("Percent"), 
      gettext("Valid Percent"), gettext("Cum Percent"))
  } else {
    xcumsum <- cumsum(xpercent)
    xcumsum[l+1] <- NA
    xfreq <- round(xfreq)
    ftab <- cbind(xfreq, xpercent, xcumsum)
    colnames(ftab) <- c(gettext("Frequency"), gettext("Percent"),
      gettext("Cum Percent"))
  }
  rownames(ftab) <- xnames
  if(length(xlab) > 0){
    ftab <- list(label = xlab, freqtable = ftab)
    class(ftab) <- "freqtable"
  }
  if(plot){
      if(y.axis == "count"){
          xdata <- xfreq
      } else {
          if(nmiss){
              xdata <- xvalper
          } else {
              xdata <- xpercent
          }
      }
      if(nmiss)
          xflen <- length(xdata) - 2
      else
          xflen <- length(xdata) - 1
      xplot <- vector("numeric", xflen)
      xnames <- vector("character", xflen)
      xallnames <- names(xdata)
      for(i in 1:xflen){
          xplot[i] <- xdata[[i]]
          xnames[i] <- xallnames[i]
      }
      names(xplot) <- xnames
      barplot(xplot, ...)
  }
  ftab
}

print.freqtable <- function(x, digits = 4, na.print="", ...){
  cat(x$label, "\n")
  x2 <- x$freqtable
  print(x2, digits = digits, na.print = na.print, ...)
  return(invisible(x))
}

