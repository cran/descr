
# Converts a variable from UTF-8 into something else
fromUTF8 <- function (x, to = "WINDOWS-1252")
{
    if (is.list(x)) {
        l <- length(x)
	for(i in 1:l) x[[i]] <- fromUTF8(x[[i]], to)
    } else {
        if (is.factor(x)) {
            levels(x) <- iconv(levels(x), "UTF-8", to, sub = "byte")
        } else {
	    if (is.character(x)) {
	        x <- iconv(x, "UTF-8", to, sub = "byte")
	    }
	}
        lb <- attr(x, "label")
        if (length(lb) > 0) {
            attr(x, "label") <- iconv(attr(x, "label"), "UTF-8",
                to, sub = "byte")
        }
    }
    x
}

# Converts a variable from something into UTF-8
toUTF8 <- function (x, from = "WINDOWS-1252")
{
    if (is.data.frame(x)) {
        l <- length(x)
	for(i in 1:l) x[[i]] <- toUTF8(x[[i]], from)
    } else {
        if (is.factor(x)) {
            levels(x) <- iconv(levels(x), from, "UTF-8", sub = "byte")
        } else {
	    if (is.character(x)) {
	        x <- iconv(x, from, "UTF-8", sub = "byte")
	    }
	}
        lb <- attr(x, "label")
        if (length(lb) > 0) {
            attr(x, "label") <- iconv(attr(x, "label"), from,
                "UTF-8", sub = "byte")
        }
    }
    x
}

