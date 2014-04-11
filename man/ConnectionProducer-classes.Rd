\name{ConnectionProducer}
\Rdversion{1.1}
\docType{class}
\alias{ConnectionProducer}
\alias{ConnectionProducer-classes}
\alias{ConnectionProducer-class}
\alias{close.ConnectionProducer}
\alias{ScanProducer-class}
\alias{ReadLinesProducer-class}
\alias{ReadTableProducer-class}
\alias{ScanProducer}
\alias{ReadLinesProducer}
\alias{ReadTableProducer}

\title{Producer classes to read file connections}

\description{

  \code{ConnectionProducer} classes include \code{ScanProducer},
  \code{ReadLinesProducer}, and \code{ReadTableProducer}, providing
  Streamer interfaces to \code{scan}, \code{readLines}, and
  \code{read.table}.

}

\usage{
ScanProducer(file, ..., fileArgs=list(), scanArgs=list(...))
ReadLinesProducer(con, ..., conArgs=list(), readLinesArgs=list(...))
ReadTableProducer(file, ..., fileArgs=list(), readTableArgs=list(...))

\S3method{close}{ConnectionProducer}(con, ...)
}

\arguments{

  \item{file, con}{The file or connection to be used for input. See
    \code{\link{connections}}.}

  \item{...}{Additional arguments, e.g., \code{nlines}, to \code{scan},
    \code{readLines}, etc.}

  \item{fileArgs, conArgs}{Arguments, e.g., \code{mode},
    \code{encoding}, to be used when invoking \code{\link{reset}()}.}

  \item{scanArgs, readLinesArgs, readTableArgs}{Arguments to
    \code{scan}, \code{readLines}, etc., when reading a file or
    connection; provide this argument when \code{...} contains arguments
    (especially \code{verbose=TRUE}) to be used by the class.}

}

\section{Methods}{See \code{\link{Producer}} Methods.}

\section{Internal Class Fields and Methods}{

  Internal fields of this class are are described with, e.g.,
  \code{getRefClass("ReadLinesProducer")$fields}.

  Internal methods of this class are described with
  \code{getRefClass("ReadLinesProducer")$methods()} and
  \code{getRefClass("ReadLinesProducer")$help()}.

}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{
  \code{\link{Streamer-package}}, \code{\linkS4class{Producer}-class},
  \code{\linkS4class{Streamer}-class}.
}

\examples{
fl <- system.file(package="Rsamtools", "extdata", "ex1.sam")

p <- ReadLinesProducer(fl, n = 1000)  # read 1000 lines at a time
while (length(y <- yield(p)))
    print(length(y))
close(p)

p <- ReadTableProducer(fl, quote="", fill=TRUE, nrows=1000)
while (length(y <- yield(p)))
    print(dim(y))

reset(p)
dim(yield(p))

## connections opened 'under the hood' are closed, with warnings
rm(p); gc() 

## avoid warnings by managing connections
p <- ScanProducer(file(fl, "r"), verbose=TRUE,
                  scanArgs=list(what=character()))
length(yield(p))
close(p)
rm(p); gc()
}

\keyword{classes}
