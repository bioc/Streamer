\name{Streamer-class}
\Rdversion{1.1}
\docType{class}
\alias{Streamer-class}

\title{Class "Streamer"}
\description{

  A virtual base class from which all classes in the Streamer package
  derive.

}

\section{Methods}{
  \describe{
    \item{reset}{\code{signature(x = "Streamer")}: see \code{?reset}.}
    \item{yield}{\code{signature(x = "Streamer")}: see \code{?yield}.}
  }
}

\section{Fields}{
  \describe{
      \item{\code{yieldSize}:}{ An \code{integer} for the number of records
          to be returned.}
      \item{\code{verbose}:}{A \code{logical(1)} instance indicating
      whether methods invoked on the class should be reported to the
      user.}
      \item{\code{inUse}:}{ A \code{logical(1)} instance indicating whether
      the object instantiated has been used in a stream.}
  }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(..., verbose = FALSE)}:}{Initialize
      \code{Streamer}, setting \code{verbose}, \code{yieldSize} and \code{inUse}
      fields, returning \code{.self} invisibly.}
    \item{\code{msg(fmt, ...)}:}{Use \code{msg} to print
      \code{sprintf(fmt, ...)} messages to user.}
    \item{\code{reset()}:}{Reset \code{Streamer}; this default method is
      a no-op.}
    \item{\code{yield()}:}{Yield default value \code{list()}.}
    \item{\code{status()}:}{Reports the status of the \code{Streamer} class.
       A \code{list} of the status of \code{yieldSize}, \code{verbose} and 
       \code{inUse} fields is returned.}
    }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Producer}-class}, \code{\linkS4class{Stream}-class}.

}

\examples{showClass("Streamer")}

\keyword{classes}
