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
Downsample(probability=0.1, ..., yieldSize=1e5, verbose=FALSE)
}

\arguments{
  \item{probability}{A \code{numeric(1)} between 0, 1 indicating the
    probability with which a record should be retained.}
  \item{...}{Additional arguments, passed to the \code{$new} method of
    the underlying reference class. Currently unused.}
  \item{yieldSize}{A \code{integer(1)} indicating the number of
    records to yield.}
  \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
}
  
\section{Fields}{
  \describe{
    \item{\code{inputPipe}:}{Object of class \code{ANY}. The component
      from which input is retrieved.}
    \item{\code{probability}:}{Object of class \code{numeric}. The
      probability of including a record in the \code{yield}.}
    \item{\code{yieldSize}:}{Object of class \code{integer}
      storing the number of records to produced each time \code{yield}
      is invoked.}
    \item{\code{.buffer}:}{Object of class \code{ANY}, used internally
      to store read but not yet parsed records.}
    \item{\code{verbose}:}{Object of class \code{logical}. Display
      method invokation messages to the user.}
  }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(..., probability, yieldSize,
	verbose)}:}{Initialize the instance.
      \describe{
	\item{\code{probability}:}{The probability with which a record
	  is included in the sample.}
	\item{\code{yieldSize}:}{The number of records to return when
	  \code{yield} is invoked.}
	\item{\code{...}:}{Additional arguments, currently ignored.}
	\item{\code{verbose}:}{Display method invokation messages to the
	  user.}
      }}
    \item{\code{reset()}:}{Reset sample buffer and delegate \code{reset}
      to \code{inputPipe}.}
    \item{\code{yield()}:}{Continually invoke \code{yield} on
      \code{inputPipe}, accumulating a random sample of
      \code{yieldSize} records until the \code{yield} of
      \code{inputPipe} has length 0. The result is a \code{list} of
      length \code{yieldSize}.}
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{showClass("Downsample")}

\keyword{classes}
