\name{connect}
\alias{connect}

\title{
  Connect \code{Producer} and \code{Consumer} streams together  and return a
  named \code{list} of \code{stream}'s that the user can invoke the method
  \code{yield} on. 
}

\description{
  The function \code{connect} can be used to connect \code{Producer} and 
  \code{Consumer} components together. 
  
  For simple streams, it may be more appropriate to use the
  \code{\link{stream}} method. The \code{connect} function is useful for connecting
  together more complex \code{stream}s involving classes such as \code{YConnector},
  \code{TConnector}, \code{ParallelConnector} etc which cannot be handled by the 
  \code{stream} method.

  The \code{connect} function returns a named list of possible \code{stream}s
  from the connection information provided by the user. The user can then call
  \code{yield} on the streams to obtain records.

}

\usage{
connect(blocks, df)
}

\arguments{
  \item{blocks}{A named list of instances of classes  \code{Consumer} and  
      \code{Producer} to the connected together in a \code{stream}}
  \item{df}{ A \code{data.frame} with two columns: "from" and  "to" which are 
    character vectors corresponding to the names of the blocks.  Each row of 
    \code{df} describes a connection between \code{Consumer} or \code{Producer} 
    blocks.}   
}

\details{

  Arguments \code{blocks} must consist of a named list of a single \code{Producer}
  and zero or more \code{Consumer} components.

}

\value{A named \code{list} of instances of class \code{\linkS4class{Stream}}.}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{yield}},\code{\link{connect}}, 
    \code{\linkS4class{Stream}-class}.}

\examples{
###  A simple stream involving a Producer and Consumer class 
fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
b <- RawInput(fl, 100L, reader=rawReaderFactory(1e4))
c <- RawToChar(10L)

###  Create a named list of the blocks to be connected together
blocks <- structure(list(b,c), names = c("b", "c"))

## Create a data.frame that describes the connection between blocks
df <- data.frame(from ="b", to = "c")
res <- connect(blocks, df)
yield(res$c)
reset(res$c)
while (length(yield(res$c))) cat("tick\n")
}

\keyword{manip}
