\name{Seq-class}
\Rdversion{1.1}
\docType{class}
\alias{Seq-class}
\alias{Seq}

\title{Producer class to generate (numeric) sequences}

\description{

  A \code{\linkS4class{Producer}}-class to generate a sequence (possibly
  long) of numbers.

}

\usage{
Seq(from = 1L, to=.Machine$integer.max, by = 1L, yieldSize=1L,
    ...)
}

\arguments{

  \item{from}{A starting value of any type (e.g., \code{integer},
    \code{numeric} supported by \code{base::seq}.}

  \item{to}{An ending value, typically of the same type as \code{from}.}
  
  \item{by}{A value, typically of the same class as \code{from},
    indicating the increment between successive numbers in the
    sequence. \code{by = 0} can create an infinite stream.}

  \item{yieldSize}{A \code{integer(1)} indicating the length of the
    output sequence each time \code{yield()} is invoked.}

  \item{...}{Additional arguments passed to \code{\linkS4class{Producer}}.}

}

\section{Constructors}{
  Use \code{Seq} to construct instances of this class.
}

\section{Methods}{See \code{\link{Producer}} Methods.}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with
  \code{getRefClass("Seq")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("Seq")$methods()} and
  \code{getRefClass("Seq")$help()}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{
s <- Seq(1, 10, yieldSize=5)
while(length(y <- yield(s)))
    print(y)
}

\keyword{classes}

