# There are three kinds of objects used when working with
# concordances.  
#
# 1.  The "activeConcordance" is an environment used to build a 
#     concordance one string at a time.  
# 2.  The "Rconcordance" is a list object.
# 3.  "String concordances" are representations of concordance
#     objects suitable for embedding in text files.

# This function produces an activeConcordance.

activeConcordance <- function(srcfile = NA_character_) 
    local({
	lastSrcref <- NULL
	srcLinenum <- integer()
	srcFile <- srcfile
	offset <- 0
	lastText <- ""
		
	saveSrcref <- function(node) {
	    # Node may be a function which we don't
	    # want to evaluate unnecessarily
	    lastSrcref <<- utils::getSrcref(node)
	}
		
	addToConcordance <- function(text) {
	    if (any(nchar(text) > 0)) {
	    	lastText <<- text
	        concordanceUsed <- length(srcLinenum)	
	        newlines <- sum(nchar(gsub("[^\n]", "", text)))
	        srcLine <- utils::getSrcLocation(lastSrcref, "line")
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
		
	finish <- function() {
		# Drop the last line if it is empty
	    if (length(srcLinenum) && (len <- nchar(lastText)) && substr(lastText, len, len) == "\n")
		srcLinenum <- srcLinenum[-length(srcLinenum)]
	    
	    structure(list(offset = offset, srcLines = srcLinenum,
	    	           srcFile = srcFile), 
	    	      class = "Rconcordance")
	}
		
	structure(environment(), class = "activeConcordance")
    })

print.activeConcordance <- function(x, ...) {
    cat("lastSrcref:")
    print(x$lastSrcref)
    cat("lastText:")
    print(x$lastText)
    print(x$finish())
    invisible(x)
}

print.Rconcordance <- function(x, ...) {
    df <- data.frame(srcFile = x$srcFile, srcLines = x$srcLines)
    rownames(df) <- seq_len(nrow(df)) + x$offset
    print(df)
    invisible(x)
}


# This function takes a location in a file and uses a concordance
# object to find the corresponding location in the source for that
# file.

matchConcordance <- function(linenum, concordance) {
    if (!all(c("offset", "srcLines", "srcFile") %in% names(concordance)))
	stop("concordance is not valid")
    linenum <- as.numeric(linenum)
    srcLines <- concordance$srcLines
    srcFile <- rep_len(concordance$srcFile, length(srcLines))
    offset <- concordance$offset
    
    result <- matrix(character(), length(linenum), 2, 
    		     dimnames = list(NULL, 
    		     		    c("srcFile", "srcLine")))
    for (i in seq_along(linenum)) {
	if (linenum[i] <= concordance$offset)
	    result[i,] <- c("", "")
	else
	    result[i,] <- c(basename(srcFile[linenum[i] - offset]), 
	    		      with(concordance, srcLines[linenum[i] - offset]))
    }
    result
}

# This function converts concordance objects to string representations
# of them.  

concordanceToStrings <- function(concordance,
			      targetfile = "") {
    offset <- concordance$offset
    src <- concordance$srcLines
    
    result <- character()
    
    srcfile <- rep_len(concordance$srcFile, length(src))
    
    while (length(src)) {
        first <- src[1]
        if (length(unique(srcfile)) > 1)
            n <- which(srcfile != srcfile[1])[1] - 1
        else
            n <- length(srcfile)
        
        vals <- with(rle(diff(src[seq_len(n)])), as.numeric(rbind(lengths, values)))
        result <- c(result, paste0("concordance:", 
               basename(targetfile), ":",
               basename(srcfile[1]), ":",
               if (offset) paste0("ofs ", offset, ":"),
               concordance$srcLines[1], " ",
               paste(vals, collapse = " ")
               ))
        offset <- offset + n
        drop <- seq_len(n)
        src <- src[-drop]
        srcfile <- srcfile[-drop]
    }
    result    
}

# This takes concordance strings and combines them
# into one concordance object.

stringsToConcordance <- function(strings) {
    # clean comments etc.
    s <- sub("^.*(concordance){1}?", "concordance", sub("[^[:digit:]]*$", "", strings))
    s <- grep("^concordance:", s, value = TRUE)
    if (!length(s))
    	return(NULL)
    result <- stringToConcordance(s[1])
    for (line in s[-1])
    	result <- addConcordance(result, line)
    result
}

# This takes one concordance string and produces a single concordance
# object

stringToConcordance <- function(s) {
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
    structure(list(offset = ofs, srcFile = srcFile, srcLines = srcLines),
    	      class = "concordance")
}

# This modifies an existing concordance object to incorporate
# one new concordance string

addConcordance <- function(conc, s) {
    prev <- stringToConcordance(s)
    if (!is.null(prev)) {
    	conc$srcFile <- rep_len(conc$srcFile, length(conc$srcLines))
        i <- seq_along(prev$srcLines)
        conc$srcFile[prev$offset + i] <- prev$srcFile
        conc$srcLines[prev$offset + i] <- prev$srcLines
    }
    conc
}

# This modifies an existing concordance by following links specified
# in a previous one.

followConcordance <- function(conc, prevConcordance) {
    if (!is.null(prevConcordance)) {
    	curLines <- conc$srcLines
    	curFile <- rep_len(conc$srcFile, length(curLines))
    	curOfs <- conc$offset
    	
    	prevLines <- prevConcordance$srcLines
    	prevFile <- rep_len(prevConcordance$srcFile, length(prevLines))
    	prevOfs <- prevConcordance$offset
    	
    	if (prevOfs) {
    	  prevLines <- c(rep(NA_integer_, prevOfs), prevLines)
    	  prevFile <- c(rep(NA_character_, prevOfs), prevFile)
    	  prevOfs <- 0
    	}
	n0 <- max(curLines)
	n1 <- length(prevLines)
	if (n1 < n0) {
	    prevLines <- c(prevLines, rep(NA_integer_, n0 - n1))
	    prevFile <- c(prevFile, rep(NA_character_, n0 - n1))
	}
	new <- is.na(prevLines[curLines])
		
	conc$srcFile <- ifelse(new, curFile,
			       prevFile[curLines])
	conc$srcLines <- ifelse(new, curLines,
				prevLines[curLines])
    }
    conc
}