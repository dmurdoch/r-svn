\name{matchConcordance}
\alias{matchConcordance}
\alias{concordanceToStrings}
\alias{stringsToConcordance}
\title{
Match source and target lines using a concordance.
}
\description{
The Rd parser records locations in \file{.Rd} files from
which components of the file are read.  Output generators
\code{\link{Rd2HTML}} and \code{\link{Rd2latex}} can match these to
output lines.  \code{matchConcordance} converts from output locations
to source locations.  \code{concordanceToStrings} produces
strings to embed in output files that can be interpreted
by \code{matchConcordance}.
}
\usage{
matchConcordance(linenum, concordance)
concordanceToStrings(concordance, targetfile = "")
stringsToConcordance(strings)	          
}
\arguments{
  \item{linenum}{
One or more line numbers being queried.
}
  \item{concordance}{
The concordance data for the file containing the lines.
}
  \item{targetfile}{
The output filename.  
  }
  \item{strings}{
A character vector which contains output from \code{concordanceToStrings}, typically in comments.
  }
}
\details{
The correspondence between target lines and source lines 
in Rd file conversion
is not one to one.  Often a single source line can 
lead to the generation of multiple output lines, and 
sometimes more than one source line triggers output
on the same output line.

\code{matchConcordance} converts from target lines to
source lines.  This can be used to help in understanding how 
particular output lines
depend on the source, e.g. when an error is found in the 
output file.  When more than one line contributes
to the output, the last one will be returned.

\code{concordanceToStrings} converts a concordance object
to strings suitable for embedding (e.g. in comments) in an
output file.

\code{stringsToConcordance} searches for strings matching the
pattern of \code{concordanceToStrings} output, then converts
those lines back to a single concordance.

There are 3 kinds of objects used to hold concordances.  

Objects of class \code{"activeConcordance"} are internal to \pkg{tools}; they
are used by \code{Rd2HTML} and \code{Rd2latex} while building the output
file and saving links to the source file.

Objects of class \code{"concordance"} are visible to users.

Concordance strings are produced by \code{concordanceToStrings}; 
they are simply character vectors encoding the concordance data.
}
\value{
\code{matchConcordance} returns a character array with one row 
per input \code{linenum} entry and
two columns, \code{"srcFile"} and \code{"srcLine"}.

For \code{concordanceToStrings}, a character vector similar to 
the ones used in \code{\link{Sweave}} to embed 
the concordance in a file.

For \code{stringsToConcordance}, a concordance object, or \code{NULL}
if no concordance strings are found.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{Rd2HTML}}, \code{\link{Rd2latex}}
}
