
file.head <- function(file, n = 6, truncate.cols = TRUE){
  wd <- getOption("width") - 1
  s <- readLines(file, n = n)
  len <- length(s)
  for(i in 1:len){
    if(truncate.cols == FALSE){
      cat(s[i], "\n", sep = "")
    } else {
      ss <- charToRaw(s[i])
      sslen <- length(ss)
      j <- 1
      k <- 1
      while(j < wd && k <= sslen) {
        if(ss[k] == 9){
          cat("\\t")
          j <- j + 2
        } else {
          cat(rawToChar(ss[k]))
          j <- j + 1
        }
        k <- k + 1
      }
      cat("\n", sep = "")
    }
  }
}

