\name{TOut-class}
\Rdversion{1.1}
\docType{class}
\alias{TOut}
\alias{TOut-class}

\title{Class "TOut"}

\description{

  A \code{\linkS4class{Consumer}}-class that is used internally to connect 
  several \code{Consumer} streams to a \code{TConnector}-class.
 
  This class is only for use by functions internal to the \code{Streamer}
  package. The \code{TOut}-class is responsible for filing the \code{.records}
  field of the \code{TConnector} with adequate number of records.
}

\usage{TOut(..., yieldSize=1e6, verbose=FALSE)}

\arguments{
   \item{...}{ Additional arguments. Currently not used}
   \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
  \item{yieldSize}{The number of records the input parser is to yield.}
  
}

\section{Constructors}{
  Use \code{TOut} to construct instances of this class.
}

\section{Fields}{
  \describe{
    \item{\code{start}:}{A \code{integer(1)} indicating the start position of the 
        record to be read next.}
    \item{\code{.records}:}{A temporary buffer of the \code{TConnector} from which
         the next set of records are to be read.}
  }
}

\section{Methods}{
  \describe{
    \item{\code{initalize(...)}:}{Initializes the fields of the \code{TOut} 
        class.}
    \item{\code{yield()}:}{Retrieves records from the \code{TConnector}-class 
        connected up-stream to it.}
  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{TConnector}}}

\examples{showClass("TOut")}

\keyword{classes}

