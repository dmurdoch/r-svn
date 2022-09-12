\name{matchConcordance}
\alias{matchConcordance}
\alias{concordanceString}
\title{
Match source and target lines using a concordance.
}
\description{
The Rd parser records locations in \file{.Rd} files from
which components of the file are read.  Output generators
\code{\link{Rd2HTML}} and \code{\link{Rd2latex}} can match these to
output lines.  This function converts from output locations
to source locations.
}
\usage{
matchConcordance(linenum, obj, 
                 concordance = attr(obj, "concordance"),
		 targetfile = obj,
	         srcfile = concordance$srcFile)
concordanceString(obj,
                  concordance = attr(obj, "concordance"),
	          targetfile = obj,
	          srcfile = concordance$srcfile)
}
\arguments{
  \item{linenum}{
One or more line numbers being queried.
}
  \item{obj}{
The object produced by the Rd conversion function run with
\code{concordance = TRUE}.
  }
  \item{concordance}{
The concordance data.
}
  \item{targetfile, srcfile}{
The output and input filenames respectively.
  }
}
\details{
The correspondence between target lines and source lines 
in Rd file conversion
is not one to one.  Often a single source line can 
lead to the generation of multiple output lines, and 
sometimes more than one source line triggers output
on the same output line.

This function converts from target lines to
source lines.  This can be used to help in understanding how 
particular output lines
depend on the source, e.g. when an error is found in the 
output file.  When more than one line contributes
to the output, the last one will be returned.
}
\value{
\code{matchConcordance} returns a character array with one row 
per input \code{linenum} entry and
two columns, \code{"srcFile"} and \code{"srcLine"}.

For \code{concordanceString}, a string similar to 
the ones used in \code{\link{Sweave}} to embed 
the concordance in a file.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{Rd2HTML}}, \code{\link{Rd2latex}}
}
