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
### Keywords: category univar

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
### Keywords: file

### ** Examples

# Suppose that "happy" is a factor and "income" is a numeric vector in a data frame:
## Not run: 
##D m <- glm(happy ~ income, family=binomial(link="logit"))
##D LogRegR2(m)
## End(Not run)



cleanEx()
nameEx("freq")
### * freq

flush(stderr()); flush(stdout())

### Name: freq
### Title: Frequency table with barplot
### Aliases: freq
### Keywords: file

### ** Examples

x <- c(rep(1, 100), rep(2, 120), rep(3, 10), rep(NA, 12))
w <- c(rep(1.1, 122), rep(0.9, 120))
x <- factor(x, levels = c(1, 2, 3), labels = c("No", "Yes", "No answer"))
attr(x, "label") <- "Do you agree?"
freq(x)
freq(x, w, y.axis = "percent", user.missing = "No answer")



cleanEx()
nameEx("fwf2csv")
### * fwf2csv

flush(stderr()); flush(stdout())

### Name: fwf2csv
### Title: Fast conversion of a fwf file into a csv one
### Aliases: fwf2csv
### Keywords: file

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
