\name{status}

\alias{status}
\alias{status-methods}
\alias{status,Streamer-method}

\title{Function to report current status of a stream}

\description{

  \code{status} invoked on a stream yields the current status of the
  stream, as reported by the \code{status} methods of each component.

}

\usage{
status(x, ...)
\S4method{status}{Streamer}(x, ...)
}

\arguments{
  \item{x}{A \code{Stream}, \code{Producer}, or \code{Consumer} object.}
  \item{\dots}{Additional arguments, currently unused.}
}

\value{

  A component-specific summary the current status

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{stream}}, \code{\linkS4class{Producer}},
  \code{\linkS4class{Consumer}}.

}

\examples{
## see example(stream)
}

\keyword{ manip }
