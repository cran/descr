
file.head <- function(file, n = 6, truncate.cols = TRUE){
    lns <- readLines(file, n = n)
    lns <- gsub("\t", "\\\\t", lns)
    if(truncate.cols)
	lns <- substr(lns, 1, getOption("width") - 1)
    cat(lns, sep = "\n")
}

