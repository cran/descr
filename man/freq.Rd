\name{freq}
\alias{freq}
\title{Frequency table with barplot}
\description{
  Prints a frequency table of the selected object. Optionally, the frequency
  might be weighted.
}
\usage{
freq(x, w, plot = getOption("descr.plot"), y.axis = c("count", "percent"),
     user.missing, \dots)
}
\arguments{
  \item{x}{The factor from which the frequency of values is desired.}
  \item{w}{An optional vector for a weighted frequency table.}
  \item{plot}{Logical: if \code{TRUE} (default), a barplot is produced.
    You may put \code{options(descr.plot = FALSE)} in your \file{.Rprofile} to
    change the default function behavior.}
  \item{y.axis}{Character string, indicating what variable to use in the y
    axis: "count" or "percent".}
  \item{user.missing}{Character vector, indicating what levels must be
    considered missing values while calculating the valid percents. Levels
    representing user missing values are not shown in the barplot.}
  \item{\dots}{Further arguments to be passed to \code{\link[graphics]{barplot}}.}
}
\details{
  A column with cumulative percents are added to the frequency table if \code{x}
  is an \code{ordered factor}.
}
\value{
  A matrix with class \code{c("matrix", "freqtable")} with the attribute
  \code{"xlab"} wich is a character string correponding to either the
  attribute "label" of \code{x} or, if \code{x} does not have this attribute,
  the name of \code{x}.
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}, based on function written by Dirk Enzmann}

\examples{
x <- c(rep(1, 100), rep(2, 120), rep(3, 10), rep(NA, 12))
w <- c(rep(1.1, 122), rep(0.9, 120))
x <- factor(x, levels = c(1, 2, 3), labels = c("No", "Yes", "No answer"))
attr(x, "label") <- "Do you agree?"
freq(x)
freq(x, w, y.axis = "percent", user.missing = "No answer")
}
\keyword{file}
