\name{Utility-class}
\Rdversion{1.1}
\docType{class}
\alias{Utility-class}

\title{Class "Utility"}

\description{

  A virtual class containing components that are required to create light weight 
  \code{Consumer} classes that process data from other \code{Producer} or
  \code{Consumer} classes. Users can inherit from the \code{Utility}-class to 
  create their own \code{Consumer}-classes that performs some operation on the
  records passed down from a class upstream. The  classes \code{RawToChar}  and
  \code{Rev} implemented in the \code{Streamer}-package derive from the
  \code{Utility}-class.
}

\section{Fields}{
  \describe{
    The \code{Utility} class inherits the fields \code{verbose}, \code{inUse}
    and \code{yieldSize} fields from the \code{Streamer} class. Please refer to
    the \code{\link{Streamer}} class for more details.

    The \code{Utility} class inherits the fields \code{inputPipe},
    and \code{.records} from the \code{Consumer} class. Please refer to
    the \code{\linkS4class{Consumer}-class} class for more details.
  }
}

\section{Class-Based Methods}{
  \describe{
    The \code{ConnectionProducer} class inherits the methods \code{initialize}, \code{msg},
    \code{reset}, \code{status} and \code{yield} from the \code{Streamer}
    virtual class. Please refer to the \code{\link{Streamer}} class for more
    details.
  
    Derived classes should implement an appropriate  \code{yield} method to 
    return the contents of the current stream. The default method for the base 
    virtual \code{Streamer} class returns a \code{list()} 
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{
  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Streamer}-class}.
}

\examples{showClass("ConnectionProducer")}

\keyword{classes}
