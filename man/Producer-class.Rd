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
  with each other through the \code{\link{stream}} function or using the
  \code{\link{connect}} function.  

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
    The \code{Producer} class inherits the fields \code{verbose}, \code{inUse}
    and \code{yieldSize} fields from the \code{Streamer} class. Please refer to
    the \code{\link{Streamer}} class for more details.
  }
}

\section{Class-Based Methods}{
  \describe{
    The \code{Producer} class inherits the methods \code{initialize}, \code{msg},
    \code{reset}, \code{status} and \code{yield} from the \code{Streamer}
    virtual class. Please refer to the \code{\link{Streamer}} class for more
    details.
    
    Derived classes should implement an appropriate \code{initialize} method to
    initialize the fields of the derived class. Additionally, a \code{yield} method
    should be implemented to return the contents of the current stream. The
    default method for the base virtual \code{Streamer} class returns a \code{list()} 
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Streamer}-class}.

}

\examples{showClass("Producer")}

\keyword{classes}
