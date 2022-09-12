
newConcordance <- function() 
    local({
	lastSrcref <- NULL
	srcLinenum <- integer()
	offset <- 0
	lastText <- ""
		
	saveSrcref <- function(node) {
	    # Node may be a function which we don't
	    # want to evaluate unnecessarily
	    lastSrcref <<- getSrcref(node)
	}
		
	addToConcordance <- function(text) {
	    if (any(nchar(text) > 0)) {
	    	lastText <<- text
	        concordanceUsed <- length(srcLinenum)	
	        newlines <- sum(nchar(gsub("[^\n]", "", text)))
	        srcLine <- getSrcLocation(lastSrcref, "line")
	        if (!is.null(srcLine)) {
	    	
	    	    # Do we have a later node on the same output
	    	    # line? Save it instead
	            if (concordanceUsed)
	            	if (text[1] != "\n")
	                    concordanceUsed <- concordanceUsed - 1
	                else
	                    newlines <- newlines - 1
	            # Save the current line(s)
	    	    srcLinenum[concordanceUsed + seq_len(1 + newlines)] <<- srcLine
	        } else if (!concordanceUsed)
	            offset <<- offset + newlines
	    }
	}
		
	finish <- function(srcfile) {
		# Drop the last line if it is empty
	    if (length(srcLinenum) && (len <- nchar(lastText)) && substr(lastText, len, len) == "\n")
		srcLinenum <- srcLinenum[-length(srcLinenum)]
	    list(offset = offset, srcLines = srcLinenum, srcFile = srcfile)
	}
	
	save <- function(obj, srcfile) {
	    attr(obj, "concordance") <- conc$finish(srcfile)
	    obj
	}
		
	environment()
    })

matchConcordance <- function(linenum, obj, 
			     concordance = attr(obj, "concordance"),
			     targetfile = obj,
			     srcfile = concordance$srcFile) {
    if (!all(c("offset", "srcLines", "srcFile") %in% names(concordance)))
	stop("no concordance found")
    linenum <- as.numeric(linenum)
    targetfile <- basename(targetfile)
    
    result <- matrix(character(), length(linenum), 2, 
    		     dimnames = list(NULL, 
    		     		    c("srcFile", "srcLine")))
    for (i in seq_along(linenum)) {
	if (linenum[i] <= concordance$offset)
	    result[i,] <- c("", "")
	else
	    result[i,] <- c(basename(srcfile), 
	    		      with(concordance, srcLines[linenum[i] - offset]))
    }
    result
}

concordanceToString <- function(obj, 
			      concordance = attr(obj, "concordance"),
			      targetfile = obj,
			      srcfile = concordance$srcFile) {
    offset <- concordance$offset
    src <- concordance$srcLines
    if (!length(src))
    	return(character())
    first <- src[1]
    vals <- with(rle(diff(src)), as.numeric(rbind(lengths, values)))
    paste0("concordance:", 
           basename(targetfile), ":",
           basename(srcfile), ":",
           if (offset) paste0("ofs ", offset, ":"),
           concordance$srcLines[1], " ",
           paste(vals, collapse = " ")
           )
}

stringToConcordance <- function(s) {
    # clean comments etc.
    s <- sub("^.*(concordance){1}?", "concordance", sub("[^[:digit:]]*$", "", s))
    if (!grepl("^concordance:", s))
    	return(NULL)
    split <- strsplit(s, ":")[[1]]
    targetfile <- split[2]
    srcFile <- split[3]
    if (length(split) == 4) {
    	ofs <- 0
    	vi <- 4
    } else {
    	ofs <- as.integer(sub("^ofs ([0-9]+)", "\\1", split[4]))
    	vi <- 5
    }
    values <- as.integer(strsplit(split[vi], " ")[[1]])
    firstline <- values[1]
    rledata <- matrix(values[-1], nrow = 2)
    rle <- structure(list(lengths=rledata[1,], values=rledata[2,]), class="rle")
    diffs <- inverse.rle(rle)
    srcLines <- c(firstline, firstline + cumsum(diffs))
    list(offset = ofs, srcFile = srcFile, srcLines = srcLines)
}