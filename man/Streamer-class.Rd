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
    \item{\code{inUse}:}{ A \code{logical(1)} instance indicating whether
      the class instantiated has been used in a stream.}
    \item{\code{verbose}:}{A \code{logical(1)} instance indicating
      whether methods invoked on the class should be reported to the
      user.}
  }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(..., verbose = FALSE)}:}{Initialize
      \code{Streamer}, setting \code{verbose} field. and returning
      \code{.self} invisibly.}
    \item{\code{reset()}:}{Reset \code{Streamer}; this default method is
      a no-op.}
    \item{\code{msg(fmt, ...)}:}{Use \code{message} to print
      \code{sprintf(fmt, ...)} to user.}
    \item{\code{yield()}:}{Yield default value \code{list()}.}
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Producer}-class}, \code{\linkS4class{Stream}-class}.

}

\examples{showClass("Streamer")}

\keyword{classes}
