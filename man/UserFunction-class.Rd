\name{UserFunction-class}
\Rdversion{1.1}
\docType{class}
\alias{UserFunction}
\alias{UserFunction-class}

\title{Class "UserFunction"}

\description{

  A \code{\linkS4class{Consumer}}-class that users can plugin their
  own function to quickly create a component that can be connected to a stream.
}

\usage{UserFunction(fun, ..., yieldSize=1e6, verbose=FALSE)}

\arguments{
   \item{fun}{User defined function that operates on records yielded by the class
       connected upstream.}
   \item{...}{Additional arguments, passed to the \code{$new} method of
    this class. Currently ignored.}
   \item{yieldSize}{A \code{integer(1)} indicating the number of
      records to yield.}
   \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
}

\section{Constructors}{
  Use \code{UserFunction} to construct instances of this class.
}

\section{Fields}{
  \describe{
    \item{\code{.fun}:}{A user supplied\code{function} that operates on records
        yielded by the class connected upstream. }
    }
}

\section{Methods}{
  \describe{
    \item{\code{yield()}:}{Retrieves records from the  class connected
        upstream to it.}
  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{showClass("UserFunction")}

\keyword{classes}

