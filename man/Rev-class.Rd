\name{Rev-class}
\Rdversion{1.1}
\docType{class}
\alias{Rev}
\alias{Rev-class}

\title{Class "Rev"}

\description{

  A \code{\linkS4class{Consumer}}-class to reverse the order of
  records. Note that the content of the \code{yield} is reversed, and
  not the entire stream.

}

\usage{Rev(...)}

\arguments{
  \item{...}{Arguments passed to the \code{\linkS4class{Consumer}}-class
    constructors.}
}

\section{Constructors}{Use \code{Rev} to construct instances of this class.}

\section{Fields}{There are no fields unique to this class.}

\section{Methods}{There are no methods unique to this class.}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{showClass("Rev")}

\keyword{classes}

