\name{reset}

\alias{reset}
\alias{reset-methods}
\alias{reset,Streamer-method}

\title{Function to reset a Stream, Producer, or Consumer}

\description{

  \code{reset} on a stream invokes the \code{reset} method of all
  components of the stream; on a component, it invokes the \code{reset}
  method of the component and all inputs to the component.

}

\usage{
reset(x, ...)
\S4method{reset}{Streamer}(x, ...)
}

\arguments{
  \item{x}{A \code{Stream}, \code{Producer}, or \code{Consumer} object.}
  \item{\dots}{Additional arguments, currently unused.}
}

\value{

  A reference to \code{x}, the stream or component on which \code{reset}
  was invoked.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{stream}}, \code{\linkS4class{Producer}},
  \code{\linkS4class{Consumer}}.

}

\examples{
## see example(stream)
}

\keyword{methods}
