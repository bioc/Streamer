\name{ConnectionProducer-class}
\Rdversion{1.1}
\docType{class}
\alias{ConnectionProducer-class}

\title{Class "ConnectionProducer"}

\description{

  A virtual class containing components that are required to create a
  custom \code{Producer}-class to read data from file connections. Users
  can inherit from the \code{ConnectionProducer}-class to create their
  own \code{Producer} classes that interact with files. Users are
  expected to pass in appropriate \code{reader} and \code{parser}
  functions for files when creating instances of classes that inherit
  from \code{ConnectionProducer}-class.

}

\section{Fields}{
  \describe{

    The \code{ConnectionProducer} class inherits the fields
    \code{verbose} and \code{inUse} fields from the \code{Streamer}
    class. Please refer to the \code{\link{Streamer}} class for more
    details.

    \item{\code{con}:}{An object of class \code{connection}.}
    \item{\code{reader}:}{A function that reads data from a file
        connection}
    \item{\code{parser}:}{A function that parses  data to records.}
  }
}

\section{Class-Based Methods}{
  \describe{

    The \code{ConnectionProducer} class inherits the methods
    \code{initialize}, \code{msg}, \code{reset}, \code{status} and
    \code{yield} from the \code{Streamer} virtual class. Please refer to
    the \code{\link{Streamer}} class for more details.
  
    Derived classes should implement an appropriate \code{yield} method
    to return the contents of the current stream. The default method for
    the base virtual \code{Streamer} class returns a \code{list()}

  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{
  \code{\link{Streamer-package}}, \code{\linkS4class{Producer}-class},
  \code{\linkS4class{Streamer}-class}.
}

\examples{showClass("ConnectionProducer")}

\keyword{classes}
