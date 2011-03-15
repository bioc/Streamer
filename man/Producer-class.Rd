\name{Producer-class}
\Rdversion{1.1}
\docType{class}
\alias{Producer-class}
\alias{show,Producer-class}

\title{Class "Producer"}

\description{

  A virtual class representing components that can read data from
  connections, and yield records to the user or a \code{Consumer}
  instance. A \code{Producer} represents a source of data, responsible
  for parsing a file into records to be passed to \code{Consumer}
  classes. \code{Producer} and \code{Consumer} instances are associated
  with each other through the \code{\link{stream}} function.

}

\section{Methods}{
  Methods defined on this class include:
  \describe{
    \item{stream}{\code{signature(x = "Producer", ...)}: see
      \code{?stream}.}
    \item{show}{\code{signature(object = "Streamer")}: Display the
      stream.}
  }
}

\section{Fields}{
  \describe{
    \item{\code{verbose}:}{A \code{logical(1)} instance indicating
      whether methods invoked on the class should be reported to the
      user.}
  }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{reset()}:}{Reset this \code{Producer}. Derived classes
      should implement an appropriate method.}
    \item{\code{yield()}:}{Yield the current stream content. The default
      method returns \code{NULL}.}
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Stream}-class}.

}

\examples{showClass("Producer")}

\keyword{classes}
