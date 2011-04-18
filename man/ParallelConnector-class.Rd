\name{ParallelConnector-class}
\Rdversion{1.1}
\docType{class}
\alias{RawToChar}
\alias{ParallelConnector-class}
\alias{ParallelConnector}
\title{Class "ParallelConnector"}

\description{

  A \code{\linkS4class{Consumer}}-class to parallelize streamer classes
  connected to it.

}

\usage{ParallelConnector(streamer, ..., verbose = FALSE)}

\arguments{
  \item{streamer}{An object of class \code{Producer} or \code{Consumer} which
          is to be parallelized.}
  \item{...}{Additional arguments to be passed to the constructor}
  \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
}

\section{Constructors}{
  Use \code{ParallelConnector} to construct instances of this class.
}

\section{Fields}{
  \describe{
    \item{\code{inputPipe}:}{Object of class \code{ANY}. The component
      from which input is retrieved.}
    \item{\code{upstream}:}{Object of class \code{ANY}. The output of a call 
      to the \code{parallel} function from the multicore package.}
  }
}

\section{Methods}{
  \describe{
    \item{\code{yield()}:}{Convert the result of applying \code{yield}
      to \code{inputPipe} (which must be a list of \code{raw}) into a
      vector of \code{character}.}
  }
}

\author{Nishant Gopalakrishnan, Martin Morgan}

\seealso{\code{\link{stream}}}

\examples{showClass("ParallelConnector")}

\keyword{classes}

