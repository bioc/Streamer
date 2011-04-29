\name{TOut-class}
\Rdversion{1.1}
\docType{class}
\alias{TOut}
\alias{TOut-class}

\title{Class "TOut"}

\description{

  A \code{\linkS4class{Consumer}}-class that is used internally to connect 
  several consumer streams to a T connector.
}

\usage{TOut(..., yieldSize=1e6, verbose=FALSE)}

\arguments{
   \item{...}{Additional arguments, passed to the \code{$new} method of
    this class. Currently ignored.}
   \item{yieldSize}{A \code{integer(1)} indicating the number of
      records to yield.}
   \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
}

\section{Constructors}{
  Use \code{TOut} to construct instances of this class.
}

\section{Fields}{
  \describe{
    \item{\code{start}:}{A \code{integer(1)} indicating the start position of the 
        record to be read next.}
    \item{\code{yieldSize}:}{A \code{integer(1)} indicating the number of
      records to yield.}
    \item{\code{verbose}:}{Object of class \code{logical}. Display
      method invokation messages to the user.}
  }
}

\section{Methods}{
  \describe{
    \item{\code{yield()}:}{Retrieves records from the TConnector class connected
        upstream to it.}
  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{showClass("TOut")}

\keyword{classes}

