\name{Downsample-class}
\Rdversion{1.1}
\docType{class}
\alias{Downsample}
\alias{Downsample-class}

\title{Consumer class to down-sample data}

\description{
  
  A \code{\linkS4class{Consumer}}-class to select records with fixed
  probability, returning a yield of fixed size. Successive calls to
  \code{yield} result in sampling of subsequent records in the stream,
  until the stream is exhausted.

}

\usage{
Downsample(probability=0.1, sampledSize=1e6, ...)
}

\arguments{

  \item{probability}{A \code{numeric(1)} between 0, 1 indicating the
    probability with which a record should be retained.}

  \item{...}{Additional arguments, passed to the \code{$new} method of
    the underlying reference class. Currently unused.}

  \item{sampledSize}{A \code{integer(1)} indicating the number of
    records to return.}

}
  
\section{Methods}{See \code{\link{Consumer}} Methods.}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with, e.g.,
  \code{getRefClass("Downsample")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("Downsample")$methods()} and
  \code{getRefClass("Downsample")$help()}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{Stream}}}

\examples{showClass("Downsample")}

\keyword{classes}
