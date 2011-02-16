
freq <- function (x, w, plot = getOption("descr.plot"),
                  y.axis = c("count", "percent"), user.missing, ...)
{
    xlab <- attr(x, "label", TRUE)
    if(length(xlab[1]) == 0)
        xlab <- deparse(substitute(x))
    if (is.factor(x) == FALSE)
        x <- as.factor(x)
    xclass <- class(x)
    if (missing(w)) 
        w <- rep(1, length(x))
    nmiss <- sum(is.na(x))
    xlevels <- levels(x)
    l <- length(xlevels)
    hasna <- FALSE
    if (nmiss) {
        hasna <- TRUE
        l <- l + 1
        xlevels[l] <- "NA's"
        x <- as.numeric(x)
        x[is.na(x)] <- l
        x <- factor(x, levels=1:l, labels = xlevels)
    }
    xnames <- levels(x)
    xnames[l + 1] <- gettext("Total", domain = "R-descr")
    xfreq <- tapply(w, x, sum)
    xfreq[is.na(xfreq)] <- 0 # If there are empty levels, tapply will insert NAs
    xtotal <- sum(xfreq)
    xpercent <- 100 * xfreq/xtotal
    xfreq[l + 1] <- sum(xfreq)
    xpercent[l + 1] <- sum(xpercent)
    if(!missing(user.missing)){
        user.missing <- paste("^", user.missing, "$", sep = "")
        for(lev in user.missing){
            if(length(grep(lev, xlevels))) x[x == lev] <- NA
        }
    }
    x[x == "NA's"] <- NA
    nmiss <- sum(is.na(x))
    if (nmiss) {
        xtotal <- xtotal - nmiss
        xfreq.valid <- xfreq
        if(!missing(user.missing))
            for(lev in user.missing)
                if(length(grep(lev, xlevels))) xfreq.valid[lev] <- NA
        if(hasna) xfreq.valid["NA's"] <- NA
        xvalper <- 100 * xfreq.valid/xtotal
        xcumsum <- cumsum(xvalper)
        xcumsum[l] <- NA
        xcumsum[l+1] <- NA
        xvalper[l] <- 0
        xvalper[l + 1] <- 0
        xvalper[l + 1] <- sum(xvalper, na.rm = TRUE)
        xvalper[l] <- NA
        xfreq <- round(xfreq)
        if(xclass[1] == "ordered"){
            ftab <- cbind(xfreq, xpercent, xvalper, xcumsum)
            colnames(ftab) <- c(gettext("Frequency", domain = "R-descr"),
                                gettext("Percent", domain = "R-descr"), 
                                gettext("Valid Percent", domain = "R-descr"),
                                gettext("Cum Percent", domain = "R-descr"))
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
                                gettext("Percent", domain = "R-descr"),
                                gettext("Cum Percent", domain = "R-descr"))
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
            if(!missing(user.missing))
                xdata[is.na(xvalper)] <- NA
        } else {
            if(y.axis[1] == "percent"){
                if(nmiss)
                    xdata <- xvalper
                else
                    xdata <- xpercent
            } else {
                msg <- paste(gettext("Invalid y.axis: '", domain = "R-descr"),
                             y.axis[1], "'", sep = "")
                stop(msg)
            }
        }
        if(length(grep("^NA's$", names(xdata))) > 0)
            xdata["NA's"] <- NA
        xdata <- xdata[!is.na(xdata)]
        xflen <- length(xdata) - 1
        xplot <- xdata[1:xflen]
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

