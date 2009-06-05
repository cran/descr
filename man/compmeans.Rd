\name{compmeans}
\alias{compmeans}
\title{Means of a numerical vector according to a factor}
\description{Calculates the means of a numerical vector according to a factor.}
\usage{
compmeans(x, f, w, digits = 6, sort = FALSE, maxlevels = 60, 
    plot = getOption("descr.plot"), xlab = deparse(substitute(f)),
    ylab = deparse(substitute(x)), \dots)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{f}{A factor.}
  \item{w}{Optional vector with weights.}
  \item{digits}{Precision of output.}
  \item{sort}{If \code{TRUE}, sorts the lines by the means values.}
  \item{maxlevels}{Maximum number of levels that \code{x} converted into factor
      should have.}
  \item{plot}{Logical: if \code{TRUE} (default), a boxplot is produced.
    You may put \code{options(descr.plot = FALSE)} in your \file{.Rprofile} to
    change the default function behavior.}
  \item{xlab}{A title for the x axis (see \code{\link[graphics]{title}}).}
  \item{ylab}{A title for the y axis (see \code{\link[graphics]{title}}).}
  \item{\dots}{Further arguments to be passed to
      \code{\link[graphics]{boxplot}}.}
}
\value{
A object of class table.
}
\author{Jakson A. Aquino, \email{jalvesaq@gmail.com}.}
\keyword{file}
