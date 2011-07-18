\name{Consumer-class}
\Rdversion{1.1}
\docType{class}
\alias{Consumer-class}
\alias{show,Consumer-method}

\title{Class "Consumer"}

\description{

  A virtual base class representing components that can consume data
  from a \code{Producer}, and yield data to the user or other
  \code{Consumer} instances. A \code{Consumer} typically transforms
  records from one form to another. \code{Producer} and \code{Consumer}
  instances are associated with each other through the
  \code{\link{stream}} function or using the \code{\link{connect}} function.

}

\section{Methods}{
  Methods defined on this class include:
  \describe{
    \item{stream}{\code{signature(x = "Consumer", ...)}: see
      \code{?stream}.}
    \item{show}{\code{signature(object = "Consumer")}: Display the
      stream.}
  }
}

\section{Fields}{
  \describe{
    \item{\code{inputPipe}:}{Object of class \code{Streamer},
      representing the \code{Producer} or \code{Consumer} connected up-stream to
      it and from which records are yielded.}
    \item{\code{.records}:}{Object of class\code{list} which is used as a
        temporary buffer for storing records.}
    
    The \code{Consumer} class inherits the fields \code{yieldSize},
    \code{verbose} and \code{inUse} from the virtual \code{Streamer} class.
    Please refer to the \code{\link{Streamer}} class for more details.
  }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(..., inputPipe)}:}{A method to
      initialize the fields of the \code{Consumer} class.
      \describe{
	\item{\code{inputPipe}:}{An object of class \code{Streamer} connected
        up-stream to it. The class could be a  \code{Consumer} or \code{Producer}
	  which yields data to the \code{Consumer} class.}
	\item{\code{...}:}{Additonal arguments, currently unused.}
	\item{\code{verbose}:}{A \code{logical(1)} instance indicating
	  whether methods invoked on the class should be reported to the
	  user.}
      }
    }
    \item{\code{reset()}:}{Return the result of delegating
      \code{reset()} to the object in the field \code{inputPipe}.}
    \item{\code{yield()}:}{Return the result of delegating
      \code{yield()} to the object in the field \code{inputPipe}.}
    \item{\code{inputs()}:}{Return a \code{character} vector
      representing up-stream components.}
    \item{\code{status{}}:}{Reports the status of the \code{Consumer} class. A
        \code{list} of the status of the length of the object in the \code{.records} 
        field, the classes connected to the \code{inputPipe} field and the status of the fields
        of the virtual class \code{Streamer} are returned.}
    \item{\code{.fill()}:}{An internal method that fills the \code{.records}
        field with \code{yieldSize} records if available.}
    \item{\code{.add(input)}:}{An internal method that appends the value passed to the argument
    \code{input} to the \code{.records} field.}
    }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}


\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Streamer}-class}, \code{\linkS4class{Producer}-class},
  \code{\linkS4class{Stream}-class}.

}

\examples{showClass("Consumer")}
