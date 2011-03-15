\name{Consumer-class}
\Rdversion{1.1}
\docType{class}
\alias{Consumer-class}
\alias{show,Consumer-method}

\title{Class "Consumer"}

\description{

  A virtual base class representing components that can consume data
  from a \code{Producer}, and yield data to the user or other
  \code{Conusmer} instances. A \code{Consumer} typically transforms
  records from one form to another. \code{Producer} and \code{Consumer}
  instances are associated with each other through the
  \code{\link{stream}} function..

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
      representing the \code{Producer} or \code{Consumer} from which
      records are yielded.} 
    \item{\code{verbose}:}{A \code{logical(1)} instance indicating
      whether methods invoked on the class should be reported to the
      user.}
  }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(..., inputPipe, verbose)}:}{A method to
      initialize the \code{Consumer}.
      \describe{
	\item{\code{inputPipe}:}{A \code{Consumer} or \code{Producer}
	  which yields data to this \code{Consumer}.}
	\item{\code{...}:}{Additonal arguments, currently unused.}
	\item{\code{verbose}:}{A \code{logical(1)} instance indicating
	  whether methods invoked on the class should be reported to the
	  user.}
      }
    }
    \item{\code{reset()}:}{Return the result of delegating
      \code{reset()} to the \code{inputPipe}.}
    \item{\code{yield()}:}{Return the result of delegating
      \code{yield()} to the \code{inputPipe}.}
    \item{\code{inputs()}:}{Return a \code{character} vector
      representing up-stream components.}
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}


\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Producer}-class},
  \code{\linkS4class{Stream}-class}.

}

\examples{showClass("Consumer")}
