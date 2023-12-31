\name{yield}

\alias{yield}
\alias{yield-methods}
\alias{yield,Streamer-method}

\title{Function to yield one task from a Stream or Producer}

\description{

  \code{yield} invoked on a stream yields one chunk of data or, if the
  stream is complete, a length zero element of the data. Successive
  invocations of \code{yield} produce successive chunks of data.

}

\usage{
yield(x, ...)
}

\arguments{

  \item{x}{A \code{Stream}, \code{Producer}, or \code{Consumer} object.}

  \item{\dots}{Additional arguments, currently unused.}

}

\value{

  A chunk of data, with the specific notion of chunk defined by the
  final component of the stream.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Stream}}, \code{\linkS4class{Producer}},
  \code{\linkS4class{Consumer}}.

}

\examples{
## see example(Stream)
}

\keyword{ manip }
