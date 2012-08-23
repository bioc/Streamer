\name{Producer-class}
\Rdversion{1.1}
\docType{class}
\alias{Producer-class}
\alias{lapply,Producer-method}
\alias{sapply,Producer-method}
\alias{show,Producer-method}

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

  \code{lapply} and \code{sapply} apply \code{FUN} to each result
  applied to \code{yield()}. Partial results on error can be recovered
  using \code{\link{tryCatch}}, as illustrated below. Infinite producers
  will of course exhaust memory.

  Inherited methods defined on this class include:
  \describe{
    \item{stream}{\code{signature(x = "Producer", ...)}: see
      \code{?stream}.}
    \item{show}{\code{signature(object = "Streamer")}: Display the
      stream.}
  }
}

\section{Fields}{
  \describe{

    The \code{Producer} class inherits the fields \code{verbose} and
    \code{inUse} fields from the \code{Streamer} class. Please refer to
    the \code{\link{Streamer}} class for more details.

  }
}

\section{Class-Based Methods}{
  \describe{

    The \code{Producer} class inherits the methods \code{initialize},
    \code{msg}, \code{reset}, \code{status} and \code{yield} from the
    \code{Streamer} virtual class. Please refer to the
    \code{\link{Streamer}} class for more details.
    
    Derived classes should implement an appropriate \code{initialize}
    method to initialize the fields of the derived class. Additionally,
    a \code{yield} method should be implemented to return the contents
    of the current stream. The default method for the base virtual
    \code{Streamer} class returns a \code{list()}

  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{

  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Streamer}-class}.

}

\examples{
showClass("Producer")
showMethods(class="Producer", where="package:Streamer")

sapply(Seq(to=47, length.out=7), function(elt) {
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
