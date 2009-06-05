\name{freq}
\alias{freq}
\title{Frequency table}
\description{
Prints a frequency table of the selected object. Optionally, the frequency
might be weighted.
}
\usage{
freq(x, w, returnMatrix = FALSE, digits = 6, na.print = "",
    plot = getOption("descr.plot"), barchart = "count", \dots)
}
\arguments{
  \item{x}{The factor from which the frequency of values is desired.}
  \item{w}{An optional vector for a weighted frequency table.}
  \item{returnMatrix}{If \code{TRUE}, the return value will be of class
      "matrix".}
  \item{digits}{Minimal number of \emph{significant} digits, see
      \code{\link[base]{print.default}}.}
  \item{na.print}{A character string which is used to indicate
    \code{\link{NA}} values in printed output (see
            \code{\link[base]{print.default}}).}
  \item{plot}{Logical: if \code{TRUE} (default), a barplot is produced.
    You may put \code{options(descr.plot = FALSE)} in your \file{.Rprofile} to
    change the default function behavior.}
  \item{barchart}{Character string, indicating what variable to use in the y
  axisto use: "count" or "percent".}
  \item{\dots}{Further arguments to be passed to
      \code{\link[graphics]{barplot}}.}
}

\value{
  An object of class freqtable if \code{x} has the attribute "label" and
      \code{returnMatrix} is false. Otherwise, a object of class "matrix".
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}, based on function written by Dirk Enzmann}

\keyword{file}
