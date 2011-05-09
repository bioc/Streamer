\name{YConnector-class}
\Rdversion{1.1}
\docType{class}
\alias{YConnector}
\alias{YConnector-class}

\title{Class "YConnector"}

\description{

  A \code{\linkS4class{Consumer}}-class that is used to join  
  several streams to a single consumer stream using a user defined function to
  combine the outputs from the input streams.
}

\usage{YConnector(fun, ..., yieldSize=1e6, verbose=FALSE)}

\arguments{
   \item{fun}{A \code{function} that takes named arguments correponding to the
     objects connnected directly upstream to it. The function is passed values
     obtained by calling \code{yield} on the objects connected upstream to it. }
   \item{...}{ Additional arguments}
   \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
  \item{yieldSize}{The number of records the input parser is to yield.}
}

\section{Constructors}{
  Use \code{YConnector} to construct instances of this class.
}

\section{Fields}{
  \describe{
    \item{\code{.fun}:}{User defined \code{function} to combine the output of
        several streams.}
      \item{\code{yieldSize}:}{A \code{integer(1)} indicating the number of
      records to yield.}
    \item{\code{verbose}:}{Object of class \code{logical}. Display
      method invokation messages to the user.}
  }
}

\section{Methods}{
  \describe{
    \item{\code{initialize(..., inputPipe, verbose)}:}{A method to
      initialize the \code{YConnector}.}
    \item{\code{yield()}:}{Retrieves records from the class connected
        upstream to it.}
  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}},\code{\link{TConnector}}, \code{\link{connect}} }

\examples{showClass("YConnector")}

\keyword{classes}

