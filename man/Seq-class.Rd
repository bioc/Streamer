\name{Seq-class}
\Rdversion{1.1}
\docType{class}
\alias{Seq-class}
\alias{Seq}

\title{Class "Seq"}

\description{

  A \code{\linkS4class{Producer}}-class to generate a sequence (possibly
  long) of numbers.

}

\usage{Seq(from = 1L, to, by = 1L, length.out=1L, ...)}

\arguments{

  \item{from}{A \code{numeric(1)} starting value.}

  \item{to}{A \code{numeric(1)} ending value. If missing, it is set to
    the maximum (or minimum, depending on \code{by}) value representable
    for the class of \code{from}, creating an (effectively) infinite
    stream.}

  \item{by}{A \code{numeric(1)} indicating the increment between
    successive numbers in the sequence.}

  \item{length.out}{A \code{integer(1)} indicating the length of the
    output sequence each time \code{yield()} is invoked.}

  \item{...}{Additional arguments passed to \code{\linkS4class{Producer}}.}

}

\section{Constructors}{
  Use \code{Seq} to construct instances of this class.
}

\section{Fields}{
  Fields of this class are
  \describe{
    \item{\code{from}:}{\code{numeric(1)} (current) sequence starting value.}
    \item{\code{to}:}{\code{numeric(1)} sequence ending value.}
    \item{\code{by}:}{\code{numeric(1)} sequence increment value.}
    \item{\code{length.out}:}{\code{integer(1)} sequence length on yield.}
  }
}

\section{Methods}{
  Methods of this class are
  \describe{

    \item{\code{yield()}:}{A sequence as specified by current
      \code{from}, \code{to}, \code{by}, and \code{length.out} values.}

  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{
s <- Seq(1, 10, length.out=5)
while(length(y <- yield(s)))
    print(y)
}

\keyword{classes}

