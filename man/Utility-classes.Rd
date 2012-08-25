\name{Utility-class}
\Rdversion{1.1}
\docType{class}
\alias{Utility-classes}
\alias{Utility-class}
\alias{RawToChar}
\alias{RawToChar-class}
\alias{Rev}
\alias{Rev-class}

\title{Consumer classes  with simple functionality, e.g., RawToChar, Rev}

\description{

  \code{Utility} is a virtual class containing components to create
  light weight \code{Consumer} classes.

  \code{RawToChar} is a class to convert \code{raw} (binary) records to
  \code{char}, applying \code{rawToChar} to each record.

  \code{Rev} reverses the order of current task.

}

\usage{
RawToChar(...)
Rev(...)
}

\arguments{

  \item{...}{Arguments passed to the
    \code{\linkS4class{Consumer}}-class.}

}

\section{Construction}{

  Use constructors \code{RawToChar}, \code{Rev}.

}

\section{Methods}{See \code{\link{Consumer}} Methods.}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with, e.g.,
  \code{getRefClass("Utility")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("Utility")$methods()} and
  \code{getRefClass("Utility")$help()}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{
  \code{\link{Streamer-package}}, \code{\linkS4class{Consumer}-class},
  \code{\linkS4class{Streamer}-class}.
}

\examples{showClass("Utility")}

\keyword{classes}
