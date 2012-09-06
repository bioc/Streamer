\name{Reducer}
\Rdversion{1.1}
\docType{class}
\alias{Reducer}
\alias{Reducer-class}

\title{Consumer class to combine successive records}

\description{
  
  A \code{\linkS4class{Consumer}}-class to reduce N successive records
  into a single yield.

}

\usage{
Reducer(FUN, init, ..., yieldNth = NA_integer_)
}

\arguments{

  \item{FUN}{A function of two arguments, where the first argument is
    the result of the previous reduction (or \code{init}, if specified,
    for the first record) and the second argument is the current
    record.}

  \item{init}{An optional initial value to initiate the reduction. When
    present, \code{init} is used to initial each yield.}

  \item{...}{Additional arguments, passed to the \code{$new} method of
    the underlying reference class. Currently unused.}

  \item{yieldNth}{A positive integer indicating how many upstream yields
    are combined before the Reducer yields. A value of
    \code{NA_integer_} indicates reduction of all records in the input
    stream.}

}
  
\section{Methods}{See \code{\link{Consumer}} Methods.}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with, e.g.,
  \code{getRefClass("Reducer")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("Reducer")$methods()} and
  \code{getRefClass("Reducer")$help()}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{Stream}}}

\examples{
s <- Stream(Seq(to=10), Reducer("+"))
yield(s)     ## sum(1:10), i.e., Reduce over the entire stream
s <- Stream(Seq(to=10), Reducer("+", yieldNth=5))
yield(s)     ## sum(1:5)
yield(s)     ## sum(6:10)
s <- Stream(Seq(to=10), Reducer("+", init=10, yieldNth=5))
sapply(s, c) ## 10 + c(sum(1:5), sum(6:10))
if (.Platform$OS.type != "windows") {
    s <- Stream(Seq(to=10),
                Team(function(i) { Sys.sleep(1); i },
                     param=MulticoreParam(10L)),
                Reducer("+"))
    system.time(y <- yield(s))
    y
}

}

\keyword{classes}
