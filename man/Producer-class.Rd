\name{Producer}
\Rdversion{1.1}
\docType{class}
\alias{Producer-class}
\alias{Producer}
\alias{lapply,Producer-method}
\alias{sapply,Producer-method}

\title{Class defining methods for all Producers}

\description{

  A virtual class representing components that can read data from
  connections, and yield records to the user or a \code{Consumer}
  instance. A \code{Producer} represents a source of data, responsible
  for parsing a file or other data source into records to be passed to
  \code{Consumer} classes. \code{Producer} and \code{Consumer} instances
  are associated with each other through the \code{\link{Stream}}
  function.

}

\usage{
\S4method{lapply}{Producer}(X, FUN, ...)

\S4method{sapply}{Producer}(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
}

\arguments{

  \item{X}{An instance of class \code{Producer}}

  \item{FUN}{A function to be applied to each successful \code{yield()}
    of \code{X}.}

  \item{...}{Additional arguments to \code{FUN}.}

  \item{simplify}{See \code{?base::sapply}.}

  \item{USE.NAMES}{See \code{?base::sapply} but note that names do not
    usually make sense for instances of class \code{Producer}.}

}

\section{Methods}{

  Methods defined on this class include:

  \describe{

    \item{Stream}{Construct a stream from one \code{Producer} and one or
      more \code{Consumer}. See \code{?Stream}.}

    \item{yield}{Yield a single result (e.g., \code{data.frame}) from
      the Producer.}

    \item{reset}{Reset, if possible, the Producer.}

    \item{lapply, sapply}{Apply \code{FUN} to each result applied to
      \code{yield()}, simplifying (using \code{simplify2array}) if
      possible for \code{sapply}. Partial results on error can be
      recovered using \code{\link{tryCatch}}, as illustrated
      below. Infinite producers will of course exhaust memory.}

  }
}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with, e.g.,
  \code{getRefClass("Producer")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("Producer")$methods()} and
  \code{getRefClass("Producer")$help()}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Streamer}-class}.

}

\examples{
showClass("Producer")
showMethods(class="Producer", where="package:Streamer")

sapply(Seq(to=47, yieldSize=7), function(elt) {
    c(n = length(elt), xbar = mean(elt))
})

## recover partial results
fun = function(i) if (i == 5) stop("oops, i == 5") else i
res <- tryCatch(sapply(Seq(to=10), fun), error=function(err) {
    warning(conditionMessage(err),
            "\n  only partial results available")
    simplify2array(err$partialResult)
})
res
}

\keyword{classes}
