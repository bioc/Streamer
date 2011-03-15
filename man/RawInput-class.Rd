\name{RawInput-class}

\Rdversion{1.1}
\docType{class}

\alias{RawInput-class}
\alias{RawInput}
\alias{rawReaderFactory}
\alias{rawParserFactory}

\title{Class "RawInput"}

\description{

  A \code{\linkS4class{Producer}}-class to interpret files as raw
  (binary) data. Users interact with this class through the constructor
  \code{\link{RawInput}} and methods \code{\link{yield}},
  \code{\link{reset}}, and \code{\link{stream}}.

  This class requires two helper functions; the \sQuote{factory} methods
  defined on this page can be used to supply these.
  \code{rawReaderFactory} creates a \sQuote{reader}, whose
  responsibility it is to accept a connection and return a vector of
  predefined type, e.g., \code{raw}.  \code{rawParserFactory} creates
  a \sQuote{parser}, responsible for parsing a buffer and vector of the
  same type as produced by the reader into records. The final record may
  be incomplete (e.g., because \code{reader} does not return complete
  records), and regardless of completion status is the content of
  \code{buf} on the subsequent invocation of
  \code{parser}. \code{length(buf)} or \code{length(bin)} may be 0, as
  when the first or final record is parsed.

}

\usage{
RawInput(con, yieldSize = 1e+06, reader = rawReaderFactory(), 
    parser = rawParserFactory(), ..., verbose = FALSE)
rawReaderFactory(blockSize = 1e+06, what)
rawParserFactory(separator = charToRaw("\n"), trim = separator)
}

\arguments{
  \item{con}{A character string or connection (opened as \code{"rb"}
    mode) from which raw input will be retrieved.}
  \item{yieldSize}{The number of records the input parser is to yield.}
  \item{reader}{A function of one argument (\code{con}, an open
    connection positioned at the start of the file, or at the position
    the \code{con} was in at the end of the previous invocation of the
    reader function) that returns a vector of type \code{raw}.}
  \item{parser}{A function of two arguments (\code{buf}, \code{bin}),
    parsing the \code{raw} vector \code{c(buf, bin)} into records.}
  \item{...}{Additional arguments, passed to the \code{$new} method of
    this class. Currently ignored.}
  \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}
  % rawReaderFactory
  \item{blockSize}{The number of bytes to read at one time.}
  \item{what}{The type of data to read, as the argument to
    \code{\link{readBin}}.}
  % rawParserFactory
  \item{separator}{A \code{raw} vector indicating the unique sequence of
    bytes by which record starts are to be recognized. The parser
    supplied here includes the record separator at the start of each
    record.}
  \item{trim}{A \code{raw} vector that is a prefix of \code{separator},
    and that is to be removed from the record during parsing.}
}

\section{Fields}{
  \describe{
    \item{\code{con}:}{Object of class \code{connection}. An R
      \code{\link{connection}} opened in \dQuote{rb} mode from which
      data will be read.}
    \item{\code{blockSize}:}{Object of class \code{integer}. Size
      (e.g., number of raw bytes) input during each
      \code{\link{yield}}.}
    \item{\code{reader}:}{Object of class \code{function}. A function
      used to input \code{blockSize} elements. See
      \code{\link{rawReaderFactory}}.}
    \item{\code{parser}:}{Object of class \code{function}. A function
      used to parse raw input into records, e.g., breaking a
      \code{raw} vector on new lines \sQuote{\\n}. See
      \code{\link{rawParserFactory}}}
    \item{\code{.buffer}:}{Object of class \code{raw}. Contains read but
      not parsed raw stream data.}
    \item{\code{.records}:}{Object of class \code{list}. Parsed but not
      yet yield-ed records.}
    \item{\code{.parsedRecords}:}{Object of class \code{integer}. Total
      number of records parsed by the Producer.}
    \item{\code{verbose}:}{Object of class \code{logical}. Should
      progress be reported?}
  }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(con, blockSize, reader, parser,
	verbose)}:}{Called during object creation with values to
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

\seealso{\code{\link{stream}}}

\examples{showClass("RawInput")}

\keyword{classes}
