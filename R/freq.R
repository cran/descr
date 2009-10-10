
freq <- function (x, w, plot = getOption("descr.plot"),
    y.axis = c("count", "percent"), ...)
{
  xlab <- attr(x, "label", TRUE)
  if(length(xlab[1]) == 0)
    xlab <- deparse(substitute(x))
  if (is.factor(x) == FALSE) {
    x <- as.factor(x)
  }
  xclass <- class(x)
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
    x <- factor(x, levels=1:l, labels = xlevels)
  }
  xnames <- levels(x)
  xnames[l + 1] <- gettext("Total", domain = "R-descr")
  xfreq <- tapply(w, x, sum)
  xfreq[is.na(xfreq)] <- 0 # Some levels may be empty, and tapply will insert NAs
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
    if(xclass[1] == "ordered"){
      ftab <- cbind(xfreq, xpercent, xvalper, xcumsum)
      colnames(ftab) <- c(gettext("Frequency", domain = "R-descr"),
          gettext("Percent", domain = "R-descr"), 
          gettext("Valid Percent", domain = "R-descr"), gettext("Cum Percent",
              domain = "R-descr"))
    } else {
      ftab <- cbind(xfreq, xpercent, xvalper)
      colnames(ftab) <- c(gettext("Frequency", domain = "R-descr"),
          gettext("Percent", domain = "R-descr"), 
          gettext("Valid Percent", domain = "R-descr"))
    }
  } else {
    xcumsum <- cumsum(xpercent)
    xcumsum[l+1] <- NA
    xfreq <- round(xfreq)
    if(xclass[1] == "ordered"){
      ftab <- cbind(xfreq, xpercent, xcumsum)
      colnames(ftab) <- c(gettext("Frequency", domain = "R-descr"),
          gettext("Percent", domain = "R-descr"), gettext("Cum Percent", domain
              = "R-descr"))
    } else {
      ftab <- cbind(xfreq, xpercent)
      colnames(ftab) <- c(gettext("Frequency", domain = "R-descr"),
          gettext("Percent", domain = "R-descr"))
    }
  }

  rownames(ftab) <- xnames
  ftab <- list(label = xlab, freqtable = ftab)
  class(ftab) <- "freqtable"

  if(plot){
      if(y.axis[1] == "count"){
          xdata <- xfreq
      } else {
        if(y.axis[1] == "percent"){
          if(nmiss){
              xdata <- xvalper
          } else {
              xdata <- xpercent
          }
        } else {
          msg <- paste(gettext("Invalid y.axis: '", domain = "R-descr"), y.axis[1], "'", sep = "")
          stop(msg)
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
      #sumx <- sum(xplot)
      #xplot <- 100 * xplot / sumx
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

