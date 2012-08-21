\name{Downsample-class}
\Rdversion{1.1}
\docType{class}
\alias{Downsample}
\alias{Downsample-class}

\title{Class "Downsample"}

\description{
  
  A \code{\linkS4class{Consumer}}-class to select records with fixed
  probability, returning a yield of fixed size. Successive calls to
  \code{yield} result in sampling of subsequent records in the stream,
  until the stream is exhausted. Users interact with this class through
  the constructor \code{Downsample} and methods \code{\link{yield}},
  \code{\link{reset}}, and \code{\link{stream}}.

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
  
\section{Fields}{
  \describe{
    \item{\code{probability}:}{Object of class \code{numeric}. The
      probability of including a record in the \code{yield}.}
    \item{\code{sampledSize}:}{Object of class \code{integer}
      storing the number of records to produced each time \code{yield}
      is invoked.}
    \item{\code{.buffer}:}{Object of class \code{ANY}, used internally
      to store read but not yet parsed records.}
  }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{.sample(x)}:}{Sample from amongst the incoming records.}
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{showClass("Downsample")}

\keyword{classes}
