\name{UserFunction-class}
\Rdversion{1.1}
\docType{class}
\alias{UserFunction}
\alias{UserFunction-class}

\title{Class "UserFunction"}

\description{

  The \code{UserFunction} class is provided as a convenience class enabling
  users to quickly create \code{Consumer}-classes that can be added to a stream
  without having to go into more complex details about the implementation of the 
  classes hierarchy provided by the \code{Streamer}-package.

  The users pass in a function \code{fun} to the constructor of the
  \code{UserFunction}-class to manipulate the records returned by
  the class intended to be connected upstream. The constructor returns an
  instance of the \code{UserFunction}-class with a with a
  \code{yield} method that the user can directly invoke.
}

\usage{UserFunction(FUN, ...)}

\arguments{
   \item{FUN}{User defined function that operates on records yielded by the class
       connected upstream.}
  \item{...}{Arguments passed to the \code{\linkS4class{Consumer}}-class
    constructors.}
}

\section{Constructors}{
  Use \code{UserFunction} to construct instances of this class.
}

\section{Fields}{
  \describe{

    \item{\code{.FUN}:}{A user supplied\code{function} that operates on
      records yielded by the class connected up-stream. }

    }
}

\section{Methods}{There are no methods unique to this class.}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{
f <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
b <- RawInput(f, 100L, reader=rawReaderFactory(1e4))
### Create a user defined function to convert raw bytes to character
myFun <- function(x) {
    sapply(x, rawToChar)
}

#### Pass the function to the UserFunction constructor
d <- UserFunction(myFun)

#### Create a stream
s <- stream(b, d)
yield(s)

}

\keyword{classes}

