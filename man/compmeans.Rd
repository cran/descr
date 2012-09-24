\name{compmeans}
\alias{compmeans}
\title{Means of a numerical vector according to a factor}
\description{Calculates the means of a numerical vector according to a factor.}
\usage{
compmeans(x, f, w, sort = FALSE, maxlevels = 60,
          user.missing, plot = getOption("descr.plot"),
          relative.widths = TRUE, col = "lightgray", \dots)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{f}{A factor.}
  \item{w}{Optional vector with weights.}
  \item{sort}{If \code{TRUE}, sorts the lines by the means values.}
  \item{maxlevels}{Maximum number of levels that \code{x} converted into factor
    should have.}
  \item{user.missing}{Character vector, indicating what levels of \code{f}
    must be treated as missing values.}
  \item{plot}{Logical: if \code{TRUE} (default), a boxplot is produced.
    You may put

    \code{options(descr.plot = FALSE)}

    in your \file{.Rprofile} to change the default function behavior.}
  \item{relative.widths}{If \code{TRUE}, the boxes widths will be proportional
    to the number of elements in each level of \code{f}.}
  \item{col}{Vector with the boxes colors.}
  \item{\dots}{Further arguments to be passed to \code{\link[graphics]{boxplot}}.}
}
\value{
  A matrix with class \code{c("matrix", "meanscomp")} with labels attributes
  for \code{x} and \code{f}. The returned object can be plotted, generating
  a \code{\link[graphics]{boxplot}} of \code{x} grouped by \code{f}.
}
\author{Jakson A. Aquino, \email{jalvesaq@gmail.com}.}

\seealso{\code{\link[graphics]{boxplot}}.}

\examples{
educ <- sample(c(1, 2), 200, replace = TRUE, prob = c(0.3, 0.7))
educ <- factor(educ, levels = c(1, 2), labels = c("Low", "High"))
income <- rnorm(200, 1000, 100) + 100 * as.numeric(educ)
attr(educ, "label") <- "Education level"
attr(income, "label") <- "Income"
w <- sample(c(10, 15, 19), 200, replace = TRUE)

compmeans(income, educ, col = c("#FFD0E0", "#D8E8FF"))
comp <- compmeans(income, educ, w, plot = FALSE)
comp
\dontrun{
plot(comp)
print(xtable(comp))
# If the decimal separator in your country is a comma:
# options(OutDec = ",")
print(xtable(comp, align = "lrrr", display = c("s", "f", "d", "f")))
}
}
