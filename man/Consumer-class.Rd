\name{Consumer}
\Rdversion{1.1}
\docType{class}
\alias{Consumer}
\alias{Consumer-class}
\alias{show,Consumer-method}

\title{Class defining methods for all consumers}

\description{

  A virtual base class representing components that can consume data
  from a \code{Producer}, and yield data to the user or other
  \code{Consumer} instances. A \code{Consumer} typically transforms
  records from one form to another. \code{Producer} and \code{Consumer}
  instances are associated with each other through the
  \code{\link{Stream}} function.

}

\section{Methods}{
  Methods defined on this class include:

  \describe{

    \item{Stream}{Construct a stream from one \code{Producer} and one or
      more \code{Consumer}. See \code{?Stream}.}

  }
}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with, e.g.,
  \code{getRefClass("Consumer")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("Consumer")$methods()} and
  \code{getRefClass("Consumer")$help()}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Streamer}-class},
  \code{\linkS4class{Producer}-class},
  \code{\linkS4class{Stream}-class}.

}

\examples{showClass("Consumer")}
