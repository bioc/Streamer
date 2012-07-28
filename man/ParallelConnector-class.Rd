\name{ParallelConnector-class}
\Rdversion{1.1}
\docType{class}
\alias{ParallelConnector-class}
\alias{ParallelConnector}
\title{Class "ParallelConnector"}

\description{

  The \code{ParallelConnector} \code{\linkS4class{Consumer}}-class can be used
  to parallelize the computations done by blocks directly connected to the 
  \code{ParallelConnector} and all blocks down-stream to the
  \code{ParallelConnector}. i.e Computations performed by the block directly 
  connected  up-stream to the \code{ParallelConnector} and all blocks connected
  down-stream to the \code{ParallelConnector} in a stream happen simultaneously.
}

\usage{ParallelConnector(..., yieldSize=1e6, verbose=FALSE)}

\arguments{
  \item{...}{Additional arguments to be passed to the constructor.}
  \item{yieldSize}{The number of records the input parser is to yield.}
  \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
}

\section{Constructors}{
  Use \code{ParallelConnector} to construct instances of this class.
}

\section{Fields}{
  \describe{
    \item{\code{.upstream}:}{Object of class \code{ANY}. The output of a call 
      to the \code{parallel} function from the \code{multicore}-package. This
      field is internal to the \code{ParallelConnector} class and will be
      populated by a call to the \code{stream} method or the \code{connect}
      function used to connect the \code{ParallelConnector} to other blocks in a
    \code{stream}.}
  }
}

\section{Methods}{
  \describe{
    \item{\code{initialize(...)}:}{Initializes the fields of the
        \code{ParallelConnector} class.}
    \item{\code{yield()}:}{Reads data from the child processes and converts the
        result (which must be a list of \code{raw}) into a vector of
        \code{character}.}
  }
}

\author{Nishant Gopalakrishnan, Martin Morgan}

\seealso{\code{\link{stream}}}

\examples{showClass("ParallelConnector")}

\keyword{classes}

