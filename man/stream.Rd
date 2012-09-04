\name{Stream}

\alias{Stream}
\alias{Stream-methods}
\alias{Stream,Consumer-method}
\alias{Stream,Producer-method}

\title{Function to create a Stream from a Producer and zero or more Consumers}

\description{

  \code{Stream} is used to create a stream from a single \code{Producer}
  and zero or more \code{Consumer} instances.

}

\usage{
Stream(x, ..., verbose=FALSE)
}

\arguments{
  \item{x}{An instance of a \code{Consumer} or \code{Producer}}
  \item{\dots}{Additional \code{Consumer} or \code{Producer} instances.}
  \item{verbose}{A \code{logical(1)} indicating whether status
    information should be reported.}
}

\details{

  Arguments to \code{Stream} must consist of a single \code{Producer}
  and zero or more \code{Consumer} components.

  When invoked with the \code{Producer} as the first argument,
  \code{Stream(P, C1, C2)} produces a stream in which the data is read
  by \code{P}, then processed by \code{C1}, then processed by \code{C2}.

  When invoked with the \code{Consumer} as the first argument, the
  \code{...} must include a \code{Producer} as the \emph{last}
  argument. \code{Stream(C1, C2, P)} produces a stream in which the data
  is read by \code{P}, then processed by \code{C2}, then processed by
  \code{C1}.

}

\value{An instance of class \code{\linkS4class{Stream}}.}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{yield}}, \code{\linkS4class{Stream}-class}.}

\examples{
fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
b <- RawInput(fl, 100L, reader=rawReaderFactory(1e4))
s <- Stream(b, Rev(), RawToChar())
s
yield(s)
reset(s)
while (length(yield(s))) cat("tick\n")
}

\keyword{manip}
