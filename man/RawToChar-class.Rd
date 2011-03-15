\name{RawToChar-class}
\Rdversion{1.1}
\docType{class}
\alias{RawToChar}
\alias{RawToChar-class}

\title{Class "RawToChar"}

\description{

  A \code{\linkS4class{Consumer}}-class to convert \code{raw} (binary)
  records to \code{char}, applying \code{rawToChar} to each record.

}

\usage{RawToChar(verbose = FALSE)}

\arguments{
  \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
}

\section{Constructors}{
  Use \code{RawToChar} to construct instances of this class.
}

\section{Fields}{
  \describe{
    \item{\code{inputPipe}:}{Object of class \code{ANY}. The component
      from which input is retrieved.}
    \item{\code{verbose}:}{Object of class \code{logical}. Display
      method invokation messages to the user.}
  }
}

\section{Methods}{
  \describe{
    \item{\code{yield()}:}{Convert the result of applying \code{yield}
      to \code{inputPipe} (which must be a list of \code{raw}) into a
      vector of \code{character}.}
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{showClass("RawToChar")}

\keyword{classes}
