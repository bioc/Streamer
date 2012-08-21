\name{TOut-class}
\Rdversion{1.1}
\docType{class}
\alias{TOut}
\alias{TOut-class}

\title{Class "TOut"}

\description{

  A \code{\linkS4class{Consumer}}-class that is used internally to connect 
  several \code{Consumer} streams to a \code{TConnector}-class.
 
  This class is only for use by functions internal to the \code{Streamer}
  package. The \code{TOut}-class is responsible for filing the \code{.records}
  field of the \code{TConnector} with adequate number of records.
}

\usage{TOut(...)}

\arguments{
  \item{...}{Arguments passed to the \code{\linkS4class{Consumer}}-class
    constructors.}
}

\section{Constructors}{
  Use \code{TOut} to construct instances of this class.
}

\section{Fields}{
  \describe{

    \item{\code{.start}:}{A \code{integer(1)} indicating the start
      position of the record to be read next.}

  }
}

\section{Methods}{
  \describe{

    \item{\code{.add(input)}:}{Temporarily store records fro sbusequent
      use.}

    \item{\code{.dump()}:}{Remove records not required for subsequent
      use.}
  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{TConnector}}}

\examples{showClass("TOut")}

\keyword{classes}

