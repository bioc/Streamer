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

\usage{Rev(verbose=FALSE)}

\arguments{
  \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
}

\section{Constructors}{
  Use \code{Rev} to construct instances of this class.
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
    \item{\code{yield()}:}{Reverse the result of applyng \code{yield} to
      \code{inputPipe}.}
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{showClass("Rev")}

\keyword{classes}

