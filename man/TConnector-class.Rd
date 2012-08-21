\name{TConnector-class}
\Rdversion{1.1}
\docType{class}
\alias{TConnector}
\alias{TConnector-class}

\title{Class "TConnector"}

\description{

  A \code{\linkS4class{Consumer}}-class that is used to connect the
  output of one stream to several \code{Consumer} \code{stream}s that
  perform different operations on the records. The \code{TConnector}
  manages the records supplied to it to ensure that all streams get
  access to \emph{all} records passed to the TConnector.

  A \code{TConnector} can be connected to other \code{Producer} and
  \code{Consumer} objects using the \code{\link{connect}} function.
}

\usage{TConnector(...)}

\arguments{
  \item{...}{Arguments passed to the \code{\linkS4class{Consumer}}-class
    constructors.}
}

\section{Constructors}{
  Use \code{TConnector} to construct instances of this class.
}

\section{Fields}{
  \describe{

    \item{\code{.records}:}{ A temporary buffer used to save records
      retrieved from the \code{Producer} or \code{Consumer} class
      connector up-stream to the \code{TConnector}. This field is used
      internally by class methods and is not intended to be manipulated
      directly by the user.}

    \item{\code{.tOuts}:}{A \code{list} of objects of class \code{TOut}
      of length equal to the number of streams connected down-stream to
      it. This field is used internally by the \code{TConnector}-class
      method and not intended to be manipulated directly by the user.}

   }
}

\section{Methods}{
  \describe{

    \item{\code{.add()}:}{An internal method to add records to the
      internal buffer(\code{.records}).}

    \item{\code{.dump()}:}{An internal method to remove records that
      have been passed down to all the down-stream classes (and are no
      longer needed) from the internal buffer(\code{.records}).}

  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}},\code{\link{YConnector}}, \code{\link{connect}} }

\examples{

### Two Streams b, c1 and b, c2 connected with a Tconnector t
fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
b <- RawInput(fl, 100L, reader=rawReaderFactory(1e4))
### c1 and c2 return different number of records
c1 <- RawToChar()
c2 <- RawToChar()
t <- TConnector()

### Connect the blocks together using the connect function
blocks <- list(b=b, c1=c1, c2=c2, t=t)
df <- data.frame(from = c("b", "t", "t"), 
                 to = c("t", "c1", "c2"))
res <- connect(blocks, df)

### yield on c2 returns 20 records 
yield(res$c2)
### yield on c1 returns the same records as with yield on c2 
### 10 records at a time
yield(res$c1)
yield(res$c1)

}

\keyword{classes}

