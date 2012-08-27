\name{FunctionProducerConsumer-classes}
\Rdversion{1.1}
\docType{class}

\alias{FunctionProducerConsumer-classes}
\alias{FunctionProducer}
\alias{FunctionProducer-class}
\alias{FunctionConsumer}
\alias{FunctionConsumer-class}

\title{Classes for user-defined Producers and Consumers}

\description{

  The \code{FunctionProducer} and \code{FunctionConsumer} classes
  provide an easy way to quickly create \code{Producer} and
  \code{Consumer} instances from user-provided functions.

}

\usage{
FunctionProducer(FUN, RESET, ..., state=NULL)
FunctionConsumer(FUN, RESET, ..., state=NULL)
}

\arguments{

  \item{FUN}{User defined function to yield successive records in the
    stream. The \code{FunctionProducer} function must return an object
    of length 0 (e.g., \code{logical(0)}) when the stream is complete.}

  \item{RESET}{An optional function of one arugment (\sQuote{state}) to
    reset the stream to its original state. If missing, the stream
    cannot be reset.}

  \item{...}{Arguments passed to the \code{\linkS4class{Producer}}-class
    or \code{\linkS4class{Consumer}}-class constructors.}

  \item{state}{Any information, made available to \code{RESET}.}

}

\section{Constructors}{

  Use \code{FunctionProducer} or \code{FunctionConsumer} to construct
  instances of this class.

}

\section{Methods}{

  See \code{\link{Producer}} and \code{\link{Consumer}} Methods.

}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with, e.g.,
  \code{getRefClass("FunctionProducer")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("FunctionProducer")$methods()} and
  \code{getRefClass("FunctionProducer")$help()}.

}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{
## A ProducerFunction
producerFun <- function() 
    ## produce the mean of 10 random uniform numbers
    ## stop when the mean is greater than 0.8
{
    x <- mean(runif(10))
    if (x > .8) numeric(0) else x
}
randomSampleMeans <- FunctionProducer(producerFun)
result <- sapply(randomSampleMeans, c)
length(result)
head(result)

## A FunctionConsumer:
consumerFun <- function(y)
    ## transform input by -10 log10
{
    -10 * log10(y)
}

neg10log10 <- FunctionConsumer(consumerFun)

strm <- stream(randomSampleMeans, neg10log10)
result <- sapply(strm, c)
length(result)
head(result)
}

\keyword{classes}

