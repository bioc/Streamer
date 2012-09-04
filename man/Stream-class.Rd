\name{Stream-class}
\Rdversion{1.1}
\docType{class}
\alias{Stream-class}
\alias{[[,Stream,numeric-method}
\alias{length,Stream-method}
\alias{lapply,Stream-method}
\alias{sapply,Stream-method}

\title{Class to represent a Producer and zero or more Consumers}

\description{

  An ordered collection of \code{Consumer} and \code{Producer}
  components combined into a single entity. Applying a method such as
  \code{yield} to \code{Stream} invokes \code{yield} on the terminal
  \code{Consumer} component of the stream, yielding one batch from the
  stream. The result of \code{yield} is defined by the \code{Producer}
  and \code{Consumer} components of the stream.

}

\usage{
\S4method{length}{Stream}(x)

\S4method{[[}{Stream,numeric}(x, i, j, ...)

\S4method{lapply}{Stream}(X, FUN, ...)

\S4method{sapply}{Stream}(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
}

\arguments{

  \item{x, X}{An instance of class \code{Stream}.}
 
  \item{FUN}{A function to be applied to each successful \code{yield()}
    of \code{X}.}

  \item{i, j}{Numeric index of the ith stream element (\code{j} is
    ignored by this method).}

  \item{...}{For \code{lapply}, \code{sapply}, additional arguments to
    \code{FUN}.}

  \item{simplify}{See \code{?base::sapply}.}

  \item{USE.NAMES}{See \code{?base::sapply} but note that names do not
    usually make sense for instances of class \code{Producer}.}

}

\section{Constructors}{
  Instances from this class are constructed with calls to \code{Stream};
  see \code{?Stream}
}

\section{Methods}{

  Methods defined on this class include:

  \describe{

    \item{length}{The number of components in this stream.}

    \item{[[}{The \code{i}th component (including inputs) of this
      stream.}

    \item{yield}{Yield a single result (e.g., \code{data.frame}) from
      the stream.}

    \item{reset}{Reset, if possible, each component of the stream.}

    \item{lapply, sapply}{Apply \code{FUN} to each result applied to
      \code{yield()}, simplifying (using \code{simplify2array}) if
      possible for \code{sapply}. Partial results on error can be
      recovered using \code{\link{tryCatch}}, as illustrated
      on the help page \code{\link{Producer}}.}

  }

}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with, e.g.,
  \code{getRefClass("FunctionProducer")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("FunctionProducer")$methods()} and
  \code{getRefClass("FunctionProducer")$help()}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Producer}-class}.

}

\examples{showClass("Stream")}

\keyword{classes}
