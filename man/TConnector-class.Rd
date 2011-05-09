\name{TConnector-class}
\Rdversion{1.1}
\docType{class}
\alias{TConnector}
\alias{TConnector-class}

\title{Class "TConnector"}

\description{

  A \code{\linkS4class{Consumer}}-class that is used to connect 
  several consumer streams to a single producer/consumer stream.
}

\usage{TConnector(..., yieldSize=1e6, verbose=FALSE)}

\arguments{
   \item{...}{ Additional arguments}
   \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
  \item{yieldSize}{The number of records the input parser is to yield.}
}

\section{Constructors}{
  Use \code{TConnector} to construct instances of this class.
}

\section{Fields}{
  \describe{
    \item{\code{.records}:}{Records retrieved from the stream connected upstream 
        to this class.}
    \item{\code{.tOuts}:}{A \code{list} of objects of class \code{TOut} of
        length correponding to the number of streams connected downstream to
        it.}
    \item{\code{yieldSize}:}{A \code{integer(1)} indicating the number of
      records to yield.}
    \item{\code{verbose}:}{Object of class \code{logical}. Display
      method invokation messages to the user.}
  }
}

\section{Methods}{
  \describe{
    \item{\code{initialize(..., inputPipe, verbose)}:}{A method to
      initialize the \code{TConnector}.}
    \item{\code{yield()}:}{Retrieves records from the class connected
        upstream to it.}
  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}},\code{\link{YConnector}}, \code{\link{connect}} }

\examples{showClass("TConnector")}

\keyword{classes}

