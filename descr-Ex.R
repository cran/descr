pkgname <- "descr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('descr')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CrossTable")
### * CrossTable

flush(stderr()); flush(stdout())

### Name: CrossTable
### Title: Cross tabulation with tests for factor independence
### Aliases: CrossTable

### ** Examples

# Simple cross tabulation of education versus prior induced abortions
# using infertility data
data(infert, package = "datasets")
CrossTable(infert$education, infert$induced, expected = TRUE)
CrossTable(infert$education, infert$induced, expected = TRUE, format = "SAS")
CrossTable(infert$education, infert$induced, expected = TRUE, format = "SPSS")
CrossTable(warpbreaks$wool, warpbreaks$tension, dnn = c("Wool", "Tension"))



cleanEx()
nameEx("LogRegR2")
### * LogRegR2

flush(stderr()); flush(stdout())

### Name: LogRegR2
### Title: Pseudo R² of logistic regression
### Aliases: LogRegR2

### ** Examples

# Suppose that "happy" is a factor and "income" is a numeric vector in a data frame:
## Not run: 
##D m <- glm(happy ~ income, family=binomial(link="logit"))
##D LogRegR2(m)
## End(Not run)



cleanEx()
nameEx("compmeans")
### * compmeans

flush(stderr()); flush(stdout())

### Name: compmeans
### Title: Means of a numerical vector according to a factor
### Aliases: compmeans

### ** Examples

educ <- sample(c(1, 2), 200, replace = TRUE, prob = c(0.3, 0.7))
educ <- factor(educ, levels = c(1, 2), labels = c("Low", "High"))
income <- rnorm(200, 1000, 100) + 100 * as.numeric(educ)
attr(educ, "label") <- "Education level"
attr(income, "label") <- "Income"
w <- sample(c(10, 15, 19), 200, replace = TRUE)

compmeans(income, educ, col = "gray90")
comp <- compmeans(income, educ, w, plot = FALSE)
comp
plot(comp)
## Not run: 
##D print(xtable(comp))
##D # If the decimal separator in your country is a comma:
##D # options(OutDec = ",")
##D print(xtable(comp, align = "lrrr", display = c("s", "f", "d", "f")))
## End(Not run)



cleanEx()
nameEx("crosstab")
### * crosstab

flush(stderr()); flush(stdout())

### Name: crosstab
### Title: Cross tabulation with mosaic plot
### Aliases: crosstab

### ** Examples

educ <- sample(c(1, 2), 200, replace = TRUE, prob = c(0.3, 0.7))
educ <- factor(educ, levels = c(1, 2), labels = c("Low", "High"))
income <- sample(c(1, 2, 3), 200, replace = TRUE, prob = c(0.3, 0.4, 0.3))
income <- factor(income, levels = c(1, 2, 3), labels = c("Low", "Middle", "High"))
attr(educ, "label") <- "Education level"
attr(income, "label") <- "Income level"
w <- sample(c(10, 15, 19), 200, replace = TRUE)

crosstab(income, educ, ylab = "Education", xlab = "Income")
ct <- crosstab(income, educ, w, expected = TRUE, plot = FALSE)
ct
plot(ct, inv.y = TRUE)
## Not run: 
##D print(xtable(ct))
##D print(xtable(ct, decimal.mark = ",", digits = 1, multirow = TRUE, hline = TRUE, align = "ll|rr|r"),
##D       sanitize.text.function = function(x) x, include.rownames = FALSE) 
## End(Not run)



cleanEx()
nameEx("freq")
### * freq

flush(stderr()); flush(stdout())

### Name: freq
### Title: Frequency table
### Aliases: freq

### ** Examples

x <- c(rep(1, 100), rep(2, 120), rep(3, 10), rep(NA, 12))
w <- c(rep(1.1, 122), rep(0.9, 120))
x <- factor(x, levels = c(1, 2, 3), labels = c("No", "Yes", "No answer"))
attr(x, "label") <- "Do you agree?"

freq(x, y.axis = "percent")
f <- freq(x, w, user.missing = "No answer", plot = FALSE)
f
plot(f)
## Not run: 
##D print(xtable(f))
##D # If the decimal separator in your country is a comma:
##D # options(OutDec = ",")
##D print(xtable(f, align = "lrrr", display = c("s", "d", "f", "f")))
## End(Not run)



cleanEx()
nameEx("fwf2csv")
### * fwf2csv

flush(stderr()); flush(stdout())

### Name: fwf2csv
### Title: Fast conversion of a fwf file into a csv one
### Aliases: fwf2csv

### ** Examples

## Not run: 
##D begin <- c(1, 3, 6, 9, 10, 11, 13)
##D end <- c(2, 5, 8, 9, 10, 12, 16)
##D names <- c("state", "municp", "house", "cond", "sex", "age", "income")
##D fwf2csv("example.txt", "example.csv", names, begin, end)
##D df <- read.table("example.csv", header = TRUE, sep = "\t", quote = "")
## End(Not run)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
