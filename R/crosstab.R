
crosstab <- function(x, y, weight = NULL, digits = 3, max.width = 5,
                     expected = FALSE, prop.r = FALSE, prop.c = FALSE,
                     prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE,
                     fisher = FALSE, mcnemar = FALSE, resid = FALSE,
                     sresid = FALSE, asresid = FALSE, missing.include = FALSE,
                     format = "SPSS", dnn = NULL, user.missing.x,
                     user.missing.y, plot = getOption("descr.plot"), 
                     xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
                     main = "", col = gray.colors(length(levels(y)), 0.9, 0.3), ...) 
{
    if(!missing(user.missing.x)){
        user.missing.x <- paste("^", user.missing.x, "$", sep = "")
        xlevels <- levels(x)
        for(lev in user.missing.x){
            if(length(grep(lev, xlevels))){
                idx <- grep(lev, as.character(x)) 
                if(length(idx))
                    x[idx] <- NA
            }
        }
        x <- factor(x)
    }
    if(!missing(user.missing.y)){
        user.missing.y <- paste("^", user.missing.y, "$", sep = "")
        ylevels <- levels(y)
        for(lev in user.missing.y){
            if(length(grep(lev, ylevels))){
                idx <- grep(lev, as.character(y)) 
                if(length(idx))
                    y[idx] <- NA
            }
        }
        y <- factor(y)
    }
    if (is.null(weight)) 
        tab <- table(x, y)
    else
        tab <- round(xtabs(weight ~ x + y))
    if (plot) 
        plot(tab, main = main, xlab = xlab, ylab = ylab, col = col, ...)

    tab <- CrossTable(tab, digits = digits, max.width = max.width,
                      expected = expected, prop.r = prop.r, prop.c = prop.c,
                      prop.t = prop.t, prop.chisq = prop.chisq, chisq = chisq,
                      fisher = fisher, mcnemar = mcnemar, resid = resid,
                      sresid = sresid, asresid = asresid,
                      missing.include = missing.include, format = format,
                      dnn = dnn)
    tab
}


