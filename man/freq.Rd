\name{freq}
\alias{freq}
\title{Frequency table with barplot}
\description{
Prints a frequency table of the selected object. Optionally, the frequency
might be weighted.
}
\usage{
freq(x, w, plot = getOption("descr.plot"), y.axis = c("count", "percent"), \dots)
}
\arguments{
  \item{x}{The factor from which the frequency of values is desired.}
  \item{w}{An optional vector for a weighted frequency table.}
  \item{plot}{Logical: if \code{TRUE} (default), a barplot is produced.
    You may put \code{options(descr.plot = FALSE)} in your \file{.Rprofile} to
    change the default function behavior.}
  \item{y.axis}{Character string, indicating what variable to use in the y
  axis: "count" or "percent".}
  \item{\dots}{Further arguments to be passed to
      \code{\link[graphics]{barplot}}.}
}
\details{
A column with cumulative percents are added to the frequency table if \code{x}
is an \code{ordered factor}.
}
\value{
  A list with class \code{freqtable} containing the following components:
  \item{label}{A character string correponding to either the attribute "label"
    of \code{x} or, if \code{x} does not have this attribute, the name of
      \code{x}.}
  \item{freqtable}{A matrix with frequencies, sums and standard deviations.}
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}, based on function written by Dirk Enzmann}

\keyword{file}
