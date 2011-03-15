\name{Stream-class}
\Rdversion{1.1}
\docType{class}
\alias{Stream-class}
\alias{[[,Stream,numeric-method}
\alias{length,Stream-method}
\alias{show,Stream-method}

\title{Class "Stream"}

\description{

  An ordered collection of \code{Consumer} and \code{Producer}
  components combined into a single entity. Applying a method such as
  \code{yield} to \code{Stream} invokes \code{yield} on the terminal
  \code{Consumer} component of the stream, yielding one batch from the
  stream. The result of \code{yield} is defined by the \code{Producer}
  and \code{Consumer} components of the stream.

}

\section{Constructors}{
  Instances from this class are constructed with calls to \code{stream};
  see \code{?stream}
}

\section{Methods}{
  This class inherits the following methods:
  \describe{
    \item{reset}{\code{signature(x = "Streamer", ...)}: see
      \code{?reset}.}
    \item{yield}{\code{signature(x = "Streamer", ...)}: see
      \code{?yield}.}
  }
  Methods defined on this class include:
  \describe{
    \item{length}{\code{signature(x = "Stream")}: the number of
      components in this stream}
    \item{[[}{\code{signature(x = "Stream", i = "numeric")}: The
      \code{i}th component (including inputs) of this stream. }
    \item{show}{\code{signature(object = "Stream")}: Display the
      stream.}
  }
}

\section{Fields}{
  \describe{
    \item{\code{inputPipe}:}{Object of class \code{ANY} ~~ }
    \item{\code{verbose}:}{A \code{logical(1)} instance indicating
      whether methods invoked on the class should be reported to the
      user.}
  }
}

\section{Class-Based Methods}{

  The following methods are inherited (from the corresponding class):
  initialize ("Streamer"), yield ("Streamer"), msg ("Streamer"), yield
  ("Consumer"), initialize ("Consumer"), reset ("Consumer"), reset
  ("Streamer"), inputs ("Consumer")

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Producer}-class}.

}

\examples{showClass("Stream")}

\keyword{classes}
