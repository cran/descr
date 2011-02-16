# From Hmisc::wtd.var
wtd.sd <- function(x, weights)
{
    xbar <- sum(weights * x)/sum(weights)
    sqrt(sum(weights * ((x - xbar)^2))/(sum(weights) - 1))
}


compmeans <- function(x, f, w, sort = FALSE, maxlevels = 60,
                      plot = getOption("descr.plot"),
                      xlab = deparse(substitute(f)),
                      ylab = deparse(substitute(x)), user.missing, ...)
{
    xlab <- xlab
    ylab <- ylab
    row.label <- attr(f, "label")
    column.label <- attr(x, "label")
    row.name <- deparse(substitute(f))
    column.name <- deparse(substitute(x))

    f.name <- deparse(substitute(f))
    n.name <- deparse(substitute(x))
    lf <- length(f)
    lx <- length(x)
    if (lf != lx) {
        msg <- paste(f.name, gettext("and", domain = "R-descr"), n.name,
                     gettext("have different lengths", domain = "R-descr"))
        stop(msg)
    }

    if (is.factor(f) == FALSE) {
        f <- factor(f)
        nl <- length(levels(f))
        if (nl > maxlevels) {
            msg <- paste(f.name,
                         gettext("was converted into a factor, but the new variable had too many levels",
                                 domain = "R-descr"))
            stop(msg)
        }
        wmsg <- paste(gettext("Warning:", domain = "R-descr"), " \"", f.name,
                      "\" ", gettext("was converted into factor!",
                                     domain = "R-descr"), sep = "")
        warning(wmsg)
    } else{
        class(f) <- "factor"
    }
    if(!missing(user.missing)){
        user.missing <- paste("^", user.missing, "$", sep = "")
        flevels <- levels(f)
        for(lev in user.missing){
            if(length(grep(lev, flevels))){
                idx <- grep(lev, as.character(f)) 
                if(length(idx))
                    f[idx] <- NA
            }
        }
        f <- factor(f)
    }

    if(is.numeric(x)){
        class(x) <- "numeric"
    }
    if (missing(w)) {
        w <- rep(1, lf)
    } else {
        lw <- length(w)
        if (lw != lf) {
            msg <- paste(f.name, gettext("and", domain = "R-descr"), "weight",
                         gettext("have different lengths.", domain = "R-descr"))
            stop(msg)
        }
    }
    if(is.numeric(w)){
        class(w) <- "numeric"
    }

    if (is.factor(x) == TRUE) {
        x <- as.numeric(x)
        wmsg <- paste(gettext("Warning:", domain = "R-descr"), " \"", n.name, "\" ", 
                      gettext("was converted from factor into numeric!", domain = "R-descr"))
        warning(wmsg)
    }
    has.w <- FALSE
    k <- grep(FALSE, (is.na(f) | is.na(x) | is.na(w)))
    f <- f[k]
    x <- x[k]
    w <- w[k]
    lf2 <- length(f)
    if (lf > lf2) {
        cat("\n")
        msg <- gettext("rows with missing values dropped", domain = "R-descr")
        wmsg <- paste(lf - lf2, msg)
        warning(wmsg)
    }

    xwsum <- tapply(x*w, f, sum)
    wsum <- tapply(w, f, sum)
    xmean <- xwsum / wsum
    wsum <- round(wsum)
    wsd <- xmean

    nflevs <- length(levels(f))
    b <- split(data.frame(x, w), f)
    wsd <- sapply(b, function(.df) wtd.sd(.df$x, .df$w))

    l <- length(xmean)
    xmean[l+1] <- weighted.mean(x, w)
    wsum[l+1] <- round(sum(w))
    wsd[l+1] <- wtd.sd(x, w)
    tab <- cbind(xmean, wsum, wsd)
    tabrn <- rownames(tab)
    tabrn[l+1] <- gettext("Total", domain = "R-descr")
    rownames(tab) <- tabrn
    colnames(tab) <- c(gettext("Mean", domain = "R-descr"),
                       gettext("N", domain = "R-descr"),
                       gettext("Std. Dev.", domain = "R-descr"))
    if (sort == TRUE) {
        len <- length(xmean)
        len1 <- len - 1
        ordl <- order(xmean[1:len1]) # Do not sort the "Total"
        tab <- tab[c(ordl, len),]
        f <- factor(as.numeric(f), levels = ordl, labels = levels(f)[ordl])
    }
    attr(tab, "row.name") <- row.name
    attr(tab, "column.name") <- column.name
    attr(tab, "row.label") <- row.label
    attr(tab, "column.label") <- column.label
    class(tab) <- c("matrix", "meanscomp")
    if(plot){
        if(is.factor(f) && length(levels(f)) < maxlevels){
            if(!missing(w))
                ENmisc::wtd.boxplot(x ~ f, weights = w, ylab = ylab, xlab = xlab, ...)
            else
                boxplot(x ~ f, ylab = ylab, xlab = xlab, ...)
        }
    }
    tab
}

print.meanscomp <- function(x, ...)
{
    rlab <- attr(x, "row.label")
    clab <- attr(x, "column.label")

    if(is.null(rlab))
        rlab <- attr(x, "row.name")
    if(is.null(clab))
        clab <- attr(x, "column.name")

    # 'domain' is necessary because this function is not exported to
    # 'descr' namespace.
    msg1 <- gettext("Mean value of", domain = "R-descr")
    msg2 <- gettext("according to", domain = "R-descr")
    lwd <- getOption("width")
    msg <- paste(msg1, ' "', clab, '" ', msg2, ' "', rlab, '"', sep = "")

    # Break the label string if it is too large:
    if(nchar(msg) < lwd){
        cat(msg, "\n", sep = "")
    } else {
        if((nchar(msg1) + nchar(clab)) < lwd) {
            msg <- paste(msg1, ' "', clab, '" ', sep = "")
            if((nchar(msg) + nchar(msg2)) < lwd) {
                cat(msg, msg2, '\n', '"', rlab, '"', '\n', sep = "")
            } else {
                cat(msg, "\n", sep = "")
                if((nchar(msg2) + nchar(rlab)) < (lwd - 1)){
                    cat(msg2, ' "', rlab, '"\n', sep = "")
                } else {
                    cat(msg2, '\n"', rlab, '"\n', sep = "")
                }
            }
        } else {
            cat(msg1, '\n"', clab, '"\n', sep = "")
            if((nchar(msg2) + nchar(rlab)) < (lwd - 1)){
                cat(msg2, ' "', rlab, '"\n', sep = "")
            } else {
                cat(msg2, '\n"', rlab, '"\n', sep = "")
            }
        }
    }
    attr(x, "row.name") <- NULL
    attr(x, "column.name") <- NULL
    attr(x, "row.label") <- NULL
    attr(x, "column.label") <- NULL
    class(x) <- "matrix"
    print(x, ...)
    return(invisible(NULL))
}

