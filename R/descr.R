
.onLoad <- function(dir, package) {
  library.dynam("descr", package, dir, local=FALSE);

  if(is.null(getOption("descr.plot"))){
      options(descr.plot=TRUE)
  }
}

.onUnload <- function(libpath) {
  library.dynam.unload("descr", libpath)
}


# Converts a variable from UTF-8 into something else
fromUTF8 <- function (x, to = "WINDOWS-1252")
{
    if (is.list(x)) {
        l <- length(x)
	for(i in 1:l) x[[i]] <- fromUTF8(x[[i]], to)
    } else {
        if (is.factor(x)) {
            levels(x) <- iconv(levels(x), "UTF-8", to, sub = "byte")
        } else {
	    if (is.character(x)) {
	        x <- iconv(x, "UTF-8", to, sub = "byte")
	    }
	}
        lb <- attr(x, "label")
        if (length(lb) > 0) {
            attr(x, "label") <- iconv(attr(x, "label"), "UTF-8",
                to, sub = "byte")
        }
    }
    x
}

# Converts a variable from something into UTF-8
toUTF8 <- function (x, from = "WINDOWS-1252")
{
    if (is.data.frame(x)) {
        l <- length(x)
	for(i in 1:l) x[[i]] <- toUTF8(x[[i]], from)
    } else {
        if (is.factor(x)) {
            levels(x) <- iconv(levels(x), from, "UTF-8", sub = "byte")
        } else {
	    if (is.character(x)) {
	        x <- iconv(x, from, "UTF-8", sub = "byte")
	    }
	}
        lb <- attr(x, "label")
        if (length(lb) > 0) {
            attr(x, "label") <- iconv(attr(x, "label"), from,
                "UTF-8", sub = "byte")
        }
    }
    x
}

# R do not have variable labels.
# See Hmisc package.
descr <- function (x)
{
    if (class(x)[1] == "data.frame") {
        l <- length(x)
        bnames <- names(x)
        for (i in 1:l) {
            lb <- attr(x[[i]], "label")
            if (length(lb) > 0) {
                cat("\n", bnames[i], " - ", lb, "\n", sep = "")
            }
	    else {
	      cat("\n", bnames[i], "\n", sep = "")
	    }
            print(summary(x[[i]]))
        }
	return(invisible(NULL))
    }
    else {
        lb <- attr(x, "label")
        if (length(lb) > 0) {
            cat(deparse(substitute(x)), " - ", lb, "\n", sep = "")
        }
        print(summary(x))
	return(invisible(NULL))
    }
}

# From Hmisc::wtd.var
wtd.sd <- function(x, weights)
{
    xbar <- sum(weights * x)/sum(weights)
    sqrt(sum(weights * ((x - xbar)^2))/(sum(weights) - 1))
}


compmeans <- function(x, f, w, digits = 6, sort = FALSE, maxlevels = 60,
    plot = getOption("descr.plot"), xlab = deparse(substitute(f)),
    ylab = deparse(substitute(x)), ...)
{
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
    print(tab, digits = digits)
    return(invisible(tab))
}


# Wrapper to CrossTable (gmodels)
crosstab <- function (x, y, weight = NULL, digits = 3, max.width = 5,
    expected = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE,
    prop.chisq = FALSE, chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
    resid = FALSE, sresid = FALSE, asresid = FALSE, missing.include = FALSE,
    format = "SPSS", dnn = NULL, plot = getOption("descr.plot"), main = "",
    xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
    col = gray.colors(length(levels(y))), ...)
{
    require(gmodels)

    if(length(weight) == 0)
        tab <- table(x, y)
    else
        tab <- round(xtabs(weight ~ x + y))
    if(plot)
        plot(tab, main = main, xlab = xlab, ylab = ylab, col = col, ...)
    CrossTable(tab, digits=digits, max.width=max.width, expected=expected,
        prop.r=prop.r, prop.c=prop.c, prop.t=prop.t, prop.chisq=prop.chisq,
        chisq=chisq, fisher=fisher, mcnemar=mcnemar, resid=resid, sresid=sresid,
        asresid=asresid, missing.include=missing.include, format=format,
        dnn=dnn)
    return(invisible(tab))
}


# The original versions of the functions freq, hist.kdnc, and LogRegR2 were
# written by Dirk Enzmann <dirk.enzmann@jura.uni-hamburg.de> who has given me
# permission to include them in this package. The original code can be found at
# http://www2.jura.uni-hamburg.de/instkrim/kriminologie/Mitarbeiter/Enzmann/Software/Enzmann_Software.html


# Plot histogram of variable with kernel density estimates and normal curve:
# I had to change the name because the "." was causing R to think that the
# function was a method of hist.
histkdnc <- function (v, breaks = 0, include.lowest = T, right = T,
  main = "Histogram with kernel density and normal curve",
  col = grey(0.90), xlab = deparse(substitute(v)), ...) 
{
  v2 <- na.omit(v)
  x <- v2
  h <- hist.default(v2, plot = F)
  if (length(breaks) == 1) 
    breaks <- h$breaks
  dens <- density(v2)
  ylim <- range(0, h$density, dnorm(x = v2, mean = mean(v2), sd = sd(v2)), 
    dens$y)
  xlim <- range(v2, dens$x)
  hist(v2, freq = F, breaks = breaks, include.lowest = include.lowest, 
    right = right, xlim = xlim, ylim = ylim, col = col, 
    xlab = xlab, main = main, ...)
  lines(density(v2), col = "red")
  curve(dnorm(x, mean = mean(v2), sd = sd(v2)), col = "blue", 
    add = T)
}


# Modified version of freq:
freq <- function (x, w, returnMatrix = FALSE, digits = 6, na.print = "",
    plot = getOption("descr.plot"), barchart = "count", ...)
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
  if(returnMatrix == FALSE && length(xlab) > 0){
    attr(ftab, "label") <- xlab
    class(ftab) <- "freqtable"
  }
  if(plot){
      if(barchart == "count"){
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
  print(ftab, digits=digits, na.print=na.print, ...)
  return(invisible(ftab))
}

print.freqtable <- function(x, ...){
  cat(attr(x, "label"), "\n")
  attr(x, "label") <- NULL
  class(x) <- "matrix"
  print(x, ...)
}


# Calculates multiple R2 analogs (pseudo R2) of logistic regression:
LogRegR2 <- function(model)
{
  if (!(model$family$family == "binomial" && (model$family$link == "logit" ||
      model$family$link == "probit"))) {
      stop("No logistic regression model, no pseudo R^2 computed.")
  }

   n    <- dim(model$model)[1]
   Chi2 <- model$null - model$dev
   Df   <- model$df.null - model$df.res
   p    <- 1-pchisq(Chi2,Df)

   Cox  <- 1-exp(-Chi2/n)             # Cox & Snell Index
   Nag  <- Cox/(1-exp(-model$null/n)) # Nagelkerke Index
   RL2  <- Chi2/model$null            # also called McFaddens R2

   cat(formatC(gettext("Chi2"), flag = "-", width = 20), Chi2, "\n")
   cat(formatC(gettext("Df"), flag = "-", width = 20), Df, "\n")
   cat(formatC(gettext("Sig."), flag = "-", width = 20), p, "\n")
   cat(formatC(gettext("Cox & Snell Index"), flag = "-", width = 20), Cox, "\n")
   cat(formatC(gettext("Nagelkerke Index"), flag = "-", width = 20), Nag, "\n")
   cat(formatC(gettext("McFadden's R2"), flag = "-", width = 20), RL2, "\n")

   x <- list('Chi2'=Chi2,'df'=Df,'p'=p,'RL2'=RL2,'CoxR2'=Cox,'NagelkerkeR2'=Nag)
   return(invisible(x))
}


fwf2csv <- function(fwffile, csvfile, names, begin, end)
{
    # Check for errors
    ncols = length(names)
    if(length(begin) != ncols || length(end) != ncols){
	stop("The vectors \"names\", \"begin\" and \"end\" must have the same length.")
    }
    if(file.exists(fwffile) == FALSE){
        msg <- paste(gettext("File not found:"), fwffile)
        stop(msg)
    }

    .C("realfwf2csv",
	as.character(fwffile),
	as.character(csvfile),
	as.character(names),
	as.integer(begin),
	as.integer(end),
	ncols, PACKAGE="descr")

    return (invisible(NULL))
}

# vim:shiftwidth=4
