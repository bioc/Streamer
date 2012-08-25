\name{Streamer-package}
\alias{Streamer-package}
\alias{Streamer}
\docType{package}
\title{Enable stream processing of large data}
\description{

  Large data files can be difficult to work with in R, where data
  generally resides in memory. This package encourages a style of
  programming where data is 'streamed' from disk into R through a series
  of components that, typically, reduce the original data to a
  manageable size. The package provides useful
  \code{\linkS4class{Producer}} and \code{\linkS4class{Consumer}}
  components for operations such as data input, sampling, indexing, and
  transformation.

}
\details{

  The central paradigm in this package is a \code{stream} composed of a
  \code{\linkS4class{Producer}} and zero or more
  \code{\linkS4class{Consumer}} components. The \code{Producer} is
  responsible for input of data, e.g., from the file system. A
  \code{Consumer} accepts data from a \code{Producer} and performs
  transformations on it. The \code{\link{stream}} function is used to
  assemble a \code{Producer} and zero or more \code{Consumer} components
  into a single string.

  The \code{\link{yield}} function can be applied to a stream to
  generate one `chunk' of data. The definition of chunk depends on the
  stream and its components. A common paradigm repeatedly invokes
  \code{yield} on a stream, retrieving chunks of the stream for further
  processing.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\keyword{ package }

\seealso{

  \code{\linkS4class{Producer}}, \code{\linkS4class{Consumer}} are the
  main types of stream components. Use \code{\link{stream}} to connect
  components, and \code{\link{yield}} to iterate a stream.

}

\examples{
## About this package
packageDescription("Streamer")

## Existing stream components
getClass("Producer")		# Producer classes
getClass("Consumer")            # Consumer classes

## An example
fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
b <- RawInput(fl, 100L, reader=rawReaderFactory(1e4))
s <- stream(RawToChar(), Rev(), b)
s
head(yield(s)) 			# First chunk

b <- RawInput(fl, 5000L, verbose=TRUE)
d <- Downsample(sampledSize=50)
s <- stream(RawToChar(), d, b)
s
s[[2]]

## Processing the first ten chunks of the file
i <- 1
while (10 >= i && 0L != length(chunk <- yield(s)))
{
   cat("chunk", i, "length", length(chunk), "\n")
   i <- i + 1
}
}

