# These functions were developed from the function CrossTable of the package
# gmodels.  The original function had the following comments:
#
# Revision 2.2 2006/05/02
# Fix a bug when a matrix is passed as the 'x' argument
# Reported by Prof. Albert Sorribas same day
# Fix involved creating default values for RowData and ColData
# when there are no dimnames for the matrix

# Revision 2.1 2005/06/26
# Added 'dnn' argument to enable specification of dimnames
# as per table()
# Correct bug in SPSS output for 1d table, where proportions
# were being printed and not percentages ('%' output)

# Revision 2.0 2005/04/27
# Added 'format = "d"' to all table count output
# so that large integers do not print in
# scientific notation

CrossTable <- function (x, y, digits = 3, max.width = 5, expected = FALSE,
  prop.r = TRUE, prop.c = TRUE, prop.t = TRUE, prop.chisq = TRUE,
  chisq = FALSE, fisher = FALSE, mcnemar = FALSE, resid = FALSE,
  sresid = FALSE, asresid = FALSE, missing.include = FALSE,
  format = c("SAS", "SPSS"), dnn = NULL, ...)
{

  format = match.arg(format)

  RowData <- deparse(substitute(x))
  if (missing(y))
    ColData <- NA
  else
    ColData <- deparse(substitute(y))

  ## Ensure that max.width >= 1
  if (max.width < 1)
    stop("max.width must be >= 1")
  ## Set 'x' vector flag
  vector.x <- FALSE
  ## Ensure that if (expected), a chisq is done and vice-versa
  if (prop.chisq)
    chisq <- TRUE
  if (expected)
    chisq <- TRUE
  if (chisq)
    expected <- TRUE

  if (missing(y))
  {
    ## is x a vector?
    if (is.null(dim(x)))
    {
      if (missing.include)
        x <- factor(x, exclude=NULL)
      else
        ## Remove any unused factor levels
        x <- factor(x)

      t <- t(as.matrix(table(x)))
      vector.x <- TRUE
    }
    ## is x a matrix?
    else if (length(dim(x) == 2))
    {
      if(any(x < 0) || any(is.na(x)))
        stop("all entries of x must be nonnegative and finite")

      ## Check to see if x has names(dimnames) defined. If yes, use these for
      ## 'RowData' and 'ColData' labels, else create blank ones
      ## This can be overridden by setting 'dnn' values
      if (is.null(names(dimnames(x))))
      {
        RowData <- ""
        ColData <- ""
      } else {
        RowData <- names(dimnames(x))[1]
        ColData <- names(dimnames(x))[2]
      }

      ## Add generic column and rownames if required
      ## check each separately, in case user has defined one or the other
      if (is.null(rownames(x)))
        rownames(x) <- paste("[", 1:nrow(x), ",]", sep = "")
      if (is.null(colnames(x)))
        colnames(x) <- paste("[,", 1:ncol(x), "]", sep = "")

      t <- x
    }
    else
      stop("x must be either a vector or a 2 dimensional matrix, if y is not given")
  }
  else
  {
    if(length(x) != length(y))
      stop("x and y must have the same length")

    if (missing.include)
    {
      x <- factor(x, exclude=c())
      y <- factor(y, exclude=c())
    }
    else
    {
      ## Remove unused factor levels from vectors
      x <- factor(x)
      y <- factor(y)
    }
    ## Generate table
    t <- table(x, y)
  }

  ## Create Titles for Table From Vector Names
  ## At least 2 x 2 table only (for now)
  if (all(dim(t) >= 2))
  {
    if (!is.null(dnn))
    {
      if (length(dnn) != 2)
        stop("dnn must have length of 2, one element for each table dimension")
      else
      {
        RowData <- dnn[1]
        ColData <- dnn[2]
      }
    }  
  }

  ## if t is not at least a 2 x 2, do not do stats
  ## even if any set to TRUE. Do not do col/table props
  if (any(dim(t) < 2))
    prop.c <- prop.chisq <- chisq <- expected <- fisher <- mcnemar <- FALSE

  if (vector.x && dim(t)[2] < 2)
    prop.r <- FALSE
  if (!vector.x && dim(t)[1] < 2)
    prop.r <- FALSE

  CPR <- CPC <- CPT <- GT <- RS <- TotalN <- CSTc <- CST <- ASR <- FTt <-
  FTl <- FTg <- McN <- McNc <- NA

  ## Generate cell proportion of row
  if(prop.r)
    CPR <- prop.table(t, 1)

  ## Generate cell proportion of col
  if(prop.c)
    CPC <- prop.table(t, 2)

  ## Generate cell proportion of total
  if(prop.t)
    CPT <- prop.table(t)

  ## Generate summary counts
  GT <- sum(t)
  RS <- rowSums(t)
  CS <- colSums(t)

  if (length(dim(x) == 2))
    TotalN <- GT
  else
    TotalN <- length(x)

  ## Perform Chi-Square Tests
  if (chisq) {
    CST <- chisq.test(t, correct = FALSE, ...)
    if (all(dim(t) == 2))
      CSTc <- chisq.test(t, correct = TRUE, ...)
  }

  if (asresid & !vector.x)
    ASR <- (CST$observed-CST$expected)/sqrt(CST$expected*((1-RS/GT) %*% t(1-CS/GT)))

  if (fisher)
  {
    try(FTt <- fisher.test(t, alternative = "two.sided"))
    if (all(dim(t) == 2))
    {
      FTl <- fisher.test(t, alternative = "less")
      FTg <- fisher.test(t, alternative = "greater")
    }
  }

  if (mcnemar)
  {
    if(dim(t)[1] == dim(t)[2])
      McN <- mcnemar.test(t, correct = FALSE)
    if (all(dim(t) == 2))
      McNc <- mcnemar.test(t, correct = TRUE)
  }

  res <- list(t = t, prop.row = CPR, prop.col = CPC, prop.tbl = CPT, gt = GT,
    rs = RS, cs = CS, total.n = TotalN, chisq = CST, chisq.corr = CSTc,
    fisher.ts = FTt, fisher.lt = FTl, fisher.gt = FTg, print.mcnemar = mcnemar,
    mcnemar = McN, mcnemar.corr = McNc, asr = ASR, RowData = RowData,
    ColData = ColData, digits = digits, max.width = max.width,
    vector.x = vector.x, expected = expected, prop.chisq = prop.chisq,
    resid = resid, sresid = sresid, asresid = asresid,
    missing.include = missing.include, format = format)
  class(res) <- "CrossTable"
  res
}

print.CrossTable <- function(x, ...)
{
  t <- x$t
  CPR <- x$prop.row
  CPC <- x$prop.col
  CPT <- x$prop.tbl
  GT <- x$gt
  RS <- x$rs
  CS <- x$cs
  TotalN <- x$total.n
  CST <- x$chisq
  CSTc <- x$chisq.corr
  FTt <- x$fisher.ts
  FTl <- x$fisher.lt
  FTg <- x$fisher.gt
  McN <- x$mcnemar
  McNc <- x$mcnemar.corr
  ASR <- x$asr
  RowData <- x$RowData
  ColData <- x$ColData
  digits <- x$digits
  max.width <- x$max.width
  vector.x <- x$vector.x
  expected <- x$expected
  prop.r <- (is.na(CPR[1]) == FALSE)
  prop.c <- (is.na(CPC[1]) == FALSE)
  prop.t <- (is.na(CPT[1]) == FALSE)
  chisq <- (is.na(CST[1]) == FALSE)
  prop.chisq <- x$prop.chisq
  fisher <- (class(FTt) == "htest")
  resid <- x$resid
  sresid <- x$sresid
  asresid <- x$asresid
  mcnemar <- x$print.mcnemar
  missing.include <- x$missing.include
  format <- x$format

  nsep <- "  | " # normal separator
  if(format == "SAS") {
    resid <- sresid <- asresid <- FALSE
    hdd <- 1
    psep <- "  | " # percent separator
  } else {
    if (format == "SPSS") {
      hdd <- 100
      psep <- "% | "
    } else {
      stop("unknown format")
    }
  }

  if(vector.x)
    expected <- prop.chisq <- prop.c <- prop.t <- resid <- sresid <- asresid <- FALSE

  ## Column and Row Total Headings
  ColTotal <- gettext("Column Total", domain = "R-descr")
  RowTotal <- gettext("Row Total", domain = "R-descr")

  ## Set consistent column widths based upon dimnames and table values
  CWidth <- max(digits + 2, c(nchar(t), nchar(dimnames(t)[[2]]), nchar(RS), nchar(CS), nchar(RowTotal)))
  RWidth <- max(c(nchar(dimnames(t)[[1]]), nchar(ColTotal)))

  ## Adjust first column width if Data Titles present
  if (is.na(RowData) == FALSE)
    RWidth <- max(RWidth, nchar(RowData))

  ## Create row separators
  RowSep <- paste(rep("-", CWidth + 2), collapse = "")
  RowSep1 <- paste(rep("-", RWidth + 1), collapse = "")
  SpaceSep1 <- paste(rep(" ", RWidth), collapse = "")
  SpaceSep2 <- paste(rep(" ", CWidth), collapse = "")

  ## Create formatted Names
  FirstCol <- formatC(dimnames(t)[[1]], width = RWidth, format = "s")
  ColTotal <- formatC(ColTotal, width = RWidth, format = "s")
  RowTotal <- formatC(RowTotal, width = CWidth, format = "s")


  #### Printing the tables

  ## Print Cell Layout
  cat(rep("\n", 2))
  cat("  ", gettext("Cell Contents", domain = "R-descr"), "\n")
  if (format=="SAS")
  {
    cat("|-------------------------|\n")
    cat(gettext("|                       N |", domain = "R-descr"), "\n")
    if (expected)
      cat(gettext("|              Expected N |", domain = "R-descr"), "\n")
    if (prop.chisq)                                                     
      cat(gettext("| Chi-square contribution |", domain = "R-descr"), "\n")
    if (prop.r)                                                         
      cat(gettext("|           N / Row Total |", domain = "R-descr"), "\n")
    if (prop.c)                                                         
      cat(gettext("|           N / Col Total |", domain = "R-descr"), "\n")
    if (prop.t)                                                         
      cat(gettext("|         N / Table Total |", domain = "R-descr"), "\n")
    cat("|-------------------------|\n")
  }
  else if (format == "SPSS")
  {
    cat("|-------------------------|\n")
    cat(gettext("|                   Count |", domain = "R-descr"), "\n")
    if (expected)
      cat(gettext("|         Expected Values |", domain = "R-descr"), "\n")
    if (prop.chisq)                                                       
      cat(gettext("| Chi-square contribution |", domain = "R-descr"), "\n")
    if (prop.r)                                                           
      cat(gettext("|             Row Percent |", domain = "R-descr"), "\n")
    if (prop.c)                                                           
      cat(gettext("|          Column Percent |", domain = "R-descr"), "\n")
    if (prop.t)                                                           
      cat(gettext("|           Total Percent |", domain = "R-descr"), "\n")
    if (resid)                                                            
      cat(gettext("|                Residual |", domain = "R-descr"), "\n")
    if (sresid)                                                           
      cat(gettext("|            Std Residual |", domain = "R-descr"), "\n")
    if (asresid)                                                          
      cat(gettext("|           Adj Std Resid |", domain = "R-descr"), "\n")
    cat("|-------------------------|\n")
  } ## End of if(format=="SPSS")

  cat("\n")
  cat(gettext("Total Observations in Table:", domain = "R-descr"), GT, "\n\n")

  ## Print 1 X N vector
  if (vector.x) {
    if (length(t) > max.width)
    {
      ## set breakpoints for output based upon max.width
      final.row <- length(t) %% max.width
      max <- length(t) - final.row
      ## Define breakpoint indices for each row
      start <- seq(1, max, max.width)
      end <- start + (max.width - 1)
      ## Add final.row if required
      if (final.row > 0)
      {
        start <- c(start, end[length(end)] + 1)
        end <- c(end, end[length(end)] + final.row)
      }
    }
    else
    {
      ## Each value printed horizontally in a single row
      start <- 1
      end <- length(t)
    }

    SpaceSep3 <- paste(SpaceSep2, " ", sep = "")

    for (i in 1:length(start))
    {
      cat(cat(SpaceSep2, sep = " | ", collapse=""),
        cat(formatC(dimnames(t)[[2]][start[i]:end[i]],
            width = CWidth-1, format = "s"), sep = nsep, collapse = "\n"),
        sep = "", collapse="")
      cat(SpaceSep3, rep(RowSep, (end[i] - start[i]) +
          1), sep = "|", collapse = "\n")
      cat(cat(SpaceSep2, sep = " | ", collapse=""),
        cat(formatC(t[, start[i]:end[i]], width = CWidth-1, format = "d"),
          sep = nsep, collapse = "\n"),
        sep = "", collapse="")
      if(prop.r)
        cat(cat(SpaceSep2, sep = " | ", collapse=""),
        cat(formatC(CPT[, start[i]:end[i]] * hdd, width = CWidth-1,
            digits = digits, format = "f"), sep = psep,
          collapse = ""), sep = "", collapse="\n")
      cat(SpaceSep3, rep(RowSep, (end[i] - start[i]) +
          1), sep = "|", collapse = "\n")

    }  ## End of for (i in 1:length(start))

    if(format == "SPSS" && GT < TotalN)
      cat("\n", gettext("Number of Missing Observations:", domain = "R-descr"),
        " ", TotalN-GT, " (", 100*(TotalN-GT)/TotalN, "%)\n", sep = "")
    return(invisible(x))
  } ## End of if (vector.x)

  ## Print table header
  if (is.na(RowData) == FALSE)
  {
    cat(SpaceSep1, "|", ColData, "\n")
    cat(cat(formatC(RowData, width = RWidth, format = "s"), sep = " | ",
        collapse=""),
      cat(formatC(dimnames(t)[[2]], width = CWidth-1, format = "s"),
        sep = nsep, collapse=""),
      cat(RowTotal, sep = " | ", collapse = "\n"), sep = "", collapse="")
  }
  else
    cat(SpaceSep1, formatC(dimnames(t)[[2]], width = CWidth, format = "s"),
    RowTotal, sep = " | ", collapse = "\n")

  cat(RowSep1, rep(RowSep, ncol(t) + 1), sep = "|", collapse = "\n")

  ## Print table cells
  for (i in 1:nrow(t))
  {
    cat(cat(FirstCol[i], sep = " | ", collapse=""),
      cat(formatC(c(t[i, ], RS[i]), width = CWidth-1, format = "d"),
        sep = nsep, collapse = "\n"), sep = "", collapse="")

    if (expected)
      cat(cat(SpaceSep1, sep = " | ", collapse=""),
      cat(formatC(CST$expected[i, ], digits = digits, format = "f",
          width = CWidth-1), sep = nsep, collapse=""),
      cat(SpaceSep2, sep = " | ", collapse = "\n"), sep = "", collapse="")

    if (prop.chisq)
      cat(cat(SpaceSep1, sep = " | ", collapse=""),
      cat(formatC((((CST$expected[i, ]-t[i, ])^2)/CST$expected[i, ]),
          digits = digits, format = "f",
          width = CWidth-1), sep = nsep, collapse=""),
      cat(SpaceSep2, sep = " | ", collapse = "\n"), sep = "", collapse="")
    if (prop.r)
      cat(cat(SpaceSep1, sep = " | ", collapse=""),
      cat(formatC(c(CPR[i, ]*hdd, hdd*RS[i] / GT),
          width = CWidth-1, digits = digits, format = "f"),
        sep = psep, collapse = "\n"), sep = "", collapse="")

    if (prop.c)
      cat(cat(SpaceSep1, sep = " | ", collapse=""),
      cat(formatC(CPC[i, ]*hdd, width = CWidth-1,
          digits = digits, format = "f"), sep = psep, collapse=""),
      cat(SpaceSep2, sep = " | ", collapse = "\n"), sep = "", collapse="")

    if (prop.t)
      cat(cat(SpaceSep1, sep = " | ", collapse=""),
      cat(formatC(CPT[i, ]*hdd, width = CWidth-1, digits = digits,
          format = "f"), sep = psep, collapse=""),
      cat(SpaceSep2, sep = " | ", collapse = "\n"), sep = "", collapse="")

    if (resid)
      cat(cat(SpaceSep1, sep = " | ", collapse = ""),
      cat(formatC(CST$observed[i, ]-CST$expected[i, ], digits = digits,
          format = "f", width = CWidth-1), sep = nsep,
        collapse = ""),
      cat(SpaceSep2, sep = " | ", collapse = "\n"), sep = "", collapse="")

    if (sresid)
      cat(cat(SpaceSep1, sep = " | ", collapse = ""),
      cat(formatC(CST$residual[i, ], digits = digits,
          format = "f", width = CWidth-1), sep = nsep,
        collapse = ""),
      cat(SpaceSep2, sep = " | ", collapse = "\n"), sep = "", collapse="")

    if (asresid)
      cat(cat(SpaceSep1, sep = " | ", collapse = ""),
      cat(formatC(ASR[i, ], digits = digits,
          format = "f", width = CWidth-1), sep = nsep,
        collapse = ""),
      cat(SpaceSep2, sep = " | ", collapse = "\n"), sep = "", collapse="")

    cat(RowSep1, rep(RowSep, ncol(t) + 1), sep = "|", collapse = "\n")
  }

  ## Print Column Totals
  cat(cat(ColTotal, sep = " | ", collapse=""),
    cat(formatC(c(CS, GT), width = CWidth-1, format = "d"), sep = nsep,
      collapse = "\n"), sep = "", collapse="")

  if (prop.c)
    cat(cat(SpaceSep1, sep = " | ", collapse=""),
    cat(formatC(hdd*CS/GT, width = CWidth-1, digits = digits,
        format = "f"), sep = psep, collapse = ""),
    cat(SpaceSep2, sep = " | ", collapse = "\n"), sep = "", collapes="")

  cat(RowSep1, rep(RowSep, ncol(t) + 1), sep = "|", collapse = "\n")

  ## Print Statistics
  if (chisq)
  {
    cat(rep("\n", 2))
    cat(gettext("Statistics for All Table Factors", domain = "R-descr"),
        "\n\n\n", sep="")

    cat(CST$method, "\n")
    cat("------------------------------------------------------------\n")
    cat(gettext("Chi^2 =", domain = "R-descr"), CST$statistic,
        "    ", gettext("d.f. =", domain = "R-descr"), CST$parameter,
        "    ", gettext("p =", domain = "R-descr"), CST$p.value, "\n\n")

    if (all(dim(t) == 2))
    {
      cat(CSTc$method, "\n")
      cat("------------------------------------------------------------\n")
      cat(gettext("Chi^2 =", domain = "R-descr"), CSTc$statistic,
          "    ", gettext("d.f. =", domain = "R-descr"), CSTc$parameter,
          "    ", gettext("p =", domain = "R-descr"), CSTc$p.value, "\n")
    }
  }

  ## Print McNemar tests
  if (is.na(McN[1]) == FALSE)
  {
    cat(rep("\n", 2))
    cat(McN$method, "\n")
    cat("------------------------------------------------------------\n")
    cat(gettext("Chi^2 =", domain = "R-descr"), McN$statistic,
        "    ", gettext("d.f. =", domain = "R-descr"), McN$parameter,
        "    ", gettext("p =", domain = "R-descr"), McN$p.value, "\n\n")

    if (is.na(McNc[1]) == FALSE)
    {
      cat(McNc$method, "\n")
      cat("------------------------------------------------------------\n")
      cat(gettext("Chi^2 =", domain = "R-descr"), McNc$statistic,
          "    ", gettext("d.f. =", domain = "R-descr"), McNc$parameter,
          "    ", gettext("p =", domain = "R-descr"), McNc$p.value, "\n")
    }
  }

  ## Pint Fisher Tests
  if (fisher)
  {
    cat(rep("\n", 2))

    cat(gettext("Fisher's Exact Test for Count Data", domain = "R-descr"))
    cat("\n------------------------------------------------------------\n")

    if (all(dim(t) == 2))
    {
      cat(gettext("Sample estimate odds ratio:", domain = "R-descr"), FTt$estimate, "\n\n")

      cat(gettext("Alternative hypothesis: true odds ratio is not equal to 1",
              domain = "R-descr"), "\n")
      cat(gettext("p =", domain = "R-descr"), FTt$p.value, "\n")
      cat(gettext("95% confidence interval:", domain = "R-descr"), FTt$conf.int, "\n\n")

      cat(gettext("Alternative hypothesis: true odds ratio is less than 1",
              domain = "R-descr"), "\n")
      cat(gettext("p =", domain = "R-descr"), FTl$p.value, "\n")
      cat(gettext("95% confidence interval:", domain = "R-descr"), FTl$conf.int, "\n\n")

      cat(gettext("Alternative hypothesis: true odds ratio is greater than 1",
              domain = "R-descr"), "\n")
      cat(gettext("p =", domain = "R-descr"), FTg$p.value, "\n")
      cat(gettext("95% confidence interval:", domain = "R-descr"), FTg$conf.int, "\n\n")
    }
    else
    {
      cat(gettext("Alternative hypothesis: two.sided", domain = "R-descr"),
          "\n")
      cat(gettext("p =", domain = "R-descr"), FTt$p.value, "\n")
    }
  } ## End Of If(Fisher) Loop

  cat(rep("\n", 2))

  if(format == "SPSS"){
    if (any(dim(t) >= 2) & any(chisq, mcnemar, fisher))
    {
      MinExpF = min(CST$expected)
      cat("       ", gettext("Minimum expected frequency:", domain = "R-descr"), MinExpF, "\n")
      NMinExpF = length(CST$expected[which(CST$expected<5)])
      if (NMinExpF > 0)
      {
        NCells = length(CST$expected)
        cat(gettext("Cells with Expected Frequency < 5:", domain = "R-descr"),
            " ", NMinExpF, " ", gettext("of", domain = "R-descr"), NCells,
            " (", 100*NMinExpF/NCells, "%)\n", sep = "")
      }
      cat("\n")

    } ## End of if (any(dim(t)...
  }
  return(invisible(x))
}

