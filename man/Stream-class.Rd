\name{Stream}
\Rdversion{1.1}
\docType{class}
\alias{Stream-class}

\alias{Stream}
\alias{Stream-methods}
\alias{Stream,Consumer-method}
\alias{Stream,Producer-method}

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
Stream(x, ..., verbose=FALSE)

\S4method{length}{Stream}(x)

\S4method{[[}{Stream,numeric}(x, i, j, ...)

\S4method{lapply}{Stream}(X, FUN, ...)

\S4method{sapply}{Stream}(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
}

\arguments{

  \item{x, X}{For \code{Stream}, \code{x} is a \code{Producer}
  instance. For other functions, an instance of class \code{Stream}.}

  \item{FUN}{A function to be applied to each successful \code{yield()}
    of \code{X}.}

  \item{i, j}{Numeric index of the ith stream element (\code{j} is
    ignored by this method).}

  \item{...}{For \code{Stream}, zero or more \code{Consumer}
    instances. For \code{lapply}, \code{sapply}, additional arguments to
    \code{FUN}.}

  \item{simplify}{See \code{?base::sapply}.}

  \item{USE.NAMES}{See \code{?base::sapply} but note that names do not
    usually make sense for instances of class \code{Producer}.}

  \item{verbose}{A \code{logical(1)} indicating whether status
    information should be reported.}

}

\section{Constructors}{

  Arguments to \code{Stream} must consist of a single \code{Producer}
  and zero or more \code{Consumer} components.

  When invoked with the \code{Producer} as the first argument,
  \code{Stream(P, C1, C2)} produces a stream in which the data is read
  by \code{P}, then processed by \code{C1}, then processed by \code{C2}.

  When invoked with the \code{Consumer} as the first argument, the
  \code{...} must include a \code{Producer} as the \emph{last}
  argument. \code{Stream(C1, C2, P)} produces a stream in which the data
  is read by \code{P}, then processed by \code{C2}, then processed by
  \code{C1}.

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

\examples{
fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
b <- RawInput(fl, 100L, reader=rawReaderFactory(1e4))
s <- Stream(b, Rev(), RawToChar())
s
yield(s)
reset(s)
while (length(yield(s))) cat("tick\n")
close(b)

strm <- Stream(Seq(to=10), FunctionConsumer(function(y) 1/y))
sapply(strm, c)
}


\keyword{classes}
