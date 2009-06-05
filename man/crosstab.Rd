\name{crosstab}
\alias{crosstab}
\title{Cross tabulation}
\description{
Outputs a cross table for two factors.
}
\usage{
crosstab(x, y, weight = NULL, digits = 3, max.width = 5, expected = FALSE,
        prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE,
        chisq = FALSE, fisher = FALSE, mcnemar = FALSE, resid = FALSE,
        sresid = FALSE, asresid = FALSE, missing.include = FALSE,
        format = "SPSS", dnn = NULL, plot = getOption("descr.plot"),
        main = "", xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
        col = gray.colors(length(levels(y))), \dots)
}

\arguments{
  \item{x, y}{Vectors in a matrix or a dataframe.}
  \item{weight}{An optional vector for a weighted cross tabulation.}
  \item{digits}{See \code{\link[gmodels]{CrossTable}}.}
  \item{max.width}{See \code{\link[gmodels]{CrossTable}}.}
  \item{expected}{See \code{\link[gmodels]{CrossTable}}.}
  \item{prop.r}{See \code{\link[gmodels]{CrossTable}}.}
  \item{prop.c}{See \code{\link[gmodels]{CrossTable}}.}
  \item{prop.t}{See \code{\link[gmodels]{CrossTable}}.}
  \item{prop.chisq}{See \code{\link[gmodels]{CrossTable}}.}
  \item{chisq}{See \code{\link[gmodels]{CrossTable}}.}
  \item{fisher}{See \code{\link[gmodels]{CrossTable}}.}
  \item{mcnemar}{See \code{\link[gmodels]{CrossTable}}.}
  \item{resid}{See \code{\link[gmodels]{CrossTable}}.}
  \item{sresid}{See \code{\link[gmodels]{CrossTable}}.}
  \item{asresid}{See \code{\link[gmodels]{CrossTable}}.}
  \item{missing.include}{See \code{\link[gmodels]{CrossTable}}.}
  \item{format}{See \code{\link[gmodels]{CrossTable}}.}
  \item{dnn}{See \code{\link[gmodels]{CrossTable}}.}
  \item{plot}{Logical: if \code{TRUE} (default), a mosaic plot is produced.
    You may put \code{options(descr.plot = FALSE)} in your \file{.Rprofile} to
    change the default function behavior.}
  \item{main}{An overall title for the plot (see
          \code{\link[graphics]{title}}).}
  \item{xlab}{A title for the x axis (see \code{\link[graphics]{title}}).}
  \item{ylab}{A title for the y axis (see \code{\link[graphics]{title}}).}
  \item{col}{A specification for the default plotting color. (See section
          \sQuote{Color Specification} of \code{\link[graphics]{par}}).}
  \item{\dots}{Further arguments to be passed to
      \code{\link[graphics]{mosaicplot}}.}
}

\details{
\code{crosstab} invokes the \code{\link[gmodels]{CrossTable}} function in the
\pkg{gmodels} package with all boolean options set to \code{FALSE} and
\code{"SPSS"} as the default \code{format} option.
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}, based on function written
by Dirk Enzmann}

\seealso{\code{\link[gmodels]{CrossTable}}}

\keyword{file}
