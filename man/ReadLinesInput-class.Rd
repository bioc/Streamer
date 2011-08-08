\name{ReadLinesInput-class}

\Rdversion{1.1}
\docType{class}

\alias{ReadLinesInput-class}
\alias{readLinesReaderFactory}
\alias{scanReaderFactory}
\alias{readLinesParserFactory}
\alias{scanParserFactory}
\alias{concatenationParserFactory}
\alias{ReadLinesInput}

\title{Class "ReadLinesInput"}

\description{

  A \code{\linkS4class{Producer}}-class to interpret text files. Users interact 
  with this class through the constructor \code{ReadLinesInput} and methods 
  \code{\link{yield}}, \code{\link{reset}}, and \code{\link{stream}}.

  This class requires two helper functions; the \sQuote{factory} methods
  defined on this page can be used to supply these.
  \code{readLinesReaderFactory} creates a \sQuote{reader}, whose
  responsibility it is to accept a connection and return a  \code{character} 
  vector.  \code{readLinesParserFactory} creates  a \sQuote{parser}, 
  responsible for parsing a buffer and vector of the
  same type as produced by the reader into records. 
  
}

\usage{
ReadLinesInput(con, reader = readLinesReaderFactory(), 
    parser = readLinesParserFactory(), ..., yieldSize = 1e+06,
    verbose = FALSE)
readLinesReaderFactory(blockSize=1e+06, ...)
scanReaderFactory(blockSize=1e06, ...)
}

\arguments{
  \item{con}{A character string or connection (opened as \code{"r"}
    mode) from which character data will be retrieved.}
  \item{yieldSize}{The number of records the input parser is to yield.}
  \item{reader}{A function of one argument (\code{con}, an open
    connection positioned at the start of the file, or at the position
    the \code{con} was in at the end of the previous invocation of the
    reader function) that returns a vector of type \code{character}.}
  \item{parser}{A function of two arguments (\code{buf}, \code{bin}),
    parsing the \code{raw} vector \code{c(buf, bin)} into records.}
  \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
  % readLinesReaderFactory
  \item{blockSize}{The number of characters to read at one time.}
  \item{...}{Additional arugments.}
 }

\section{Fields}{
  \describe{
    \item{\code{con}:}{Object of class \code{connection}. An R
      \code{\link{connection}} opened in \dQuote{r} mode from which
      data will be read.}
    \item{\code{blockSize}:}{Object of class \code{integer}. Size of 
        input during each \code{\link{yield}}.}
    \item{\code{reader}:}{Object of class \code{function}. A function
      used to input \code{blockSize} elements. See
      \code{\link{readLinesReaderFactory}}.}
    \item{\code{parser}:}{Object of class \code{function}. A function
      used to parse character input into records. See
      \code{\link{readLinesParserFactory}}}
    \item{\code{.records}:}{Object of class \code{character}. Records that have
        been read and parsed but not yet yield-ed records.}
    \item{\code{verbose}:}{Object of class \code{logical}. Should
      progress be reported?}
  }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(..)}:}{Called during object creation with values to
      initialize fields.}
    \item{\code{reset()}:}{Remove buffer and current records, reset
      record counter, re-open \code{con}.}
    \item{\code{status()}:}{Summarize status of stream.}
    \item{\code{yield()}:}{Process stream to yield as many complete
      records as are represented in the current \code{blockSize}
      elements.}
    \item{\code{finalize()}:}{Close \code{con}.}
  }
}

\author{Martin Morgan \url{mtmorgan@fhcrc.org}}

\seealso{\code{\link{stream}}, \code{\link{connect}}}

\examples{
showClass("ReadLinesInput")
}

\keyword{classes}
