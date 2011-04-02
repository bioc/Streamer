\name{NetCDFInput-class}

\Rdversion{1.1}
\docType{class}

\alias{NetCDFInput-class}
\alias{NetCDFInput}
\alias{dimensions,NetCDFInput-method}

\title{Class "NetCDFInput"}

\description{

  A \code{\linkS4class{NetCDFInput}}-class to retrieve data store in
  NetCDF files. Users interact with this class through the constructor
  \code{\link{NetCDFInput}} and methods \code{\link{yield}},
  \code{\link{status}}, and \code{\link{reset}}.

}

\usage{
NetCDFInput(ncdf, var, slice, ..., verbose = FALSE)
\S4method{dimensions}{NetCDFInput}(x, ...)
}

\arguments{

  \item{ncdf}{An object of class \code{NetCDFFile} rrepresenting the
    file from which data is to be read.}

  \item{var}{A \code{character(1)} string naming the variable to be read
    from the NetCDF file.}

  \item{slice}{A named integer vector specifying the slice to be
    iterated over. The names correspond to dimensions of \code{var}, the
    values to the number of elements to be retrieved with each
    \code{yield}.}

  \item{...}{Additional arguments, passed to the \code{$new} method of
    this class. Currently ignored.}

  \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}

  \item{x}{An instance of the \code{NetCDFInput} class.}

}

\section{Class Methods}{

  \describe{

    \item{\code{dimensions()}:}{Return the dimensions associated with
      the variable this object is iterating over.}

    \item{\code{yield()}:}{Processes the NetCDF file and retrieves a
      matrix of data from the NetCDF file corresponding to the slice
      size that has been set.  Repeated calls to the \code{yield}
      function retrieves the next block of data until the end of file
      has been reached.}

    \item{\code{reset()}:}{Resets the cursor that tracks the next block
      of data to be read to the start of the file.}

     \item{\code{status()}:}{Returns a named numeric vector for the
       position of the start of the block from which data will be read
       for the next call to the \code{yield} function.}

  }
}

\section{Class Internal Fields: (For developers)}{
  \describe{

    \item{\code{ncdf}:}{An object of class \code{NetCDFFile} from which
      data is being read.}

    \item{\code{name}:}{A \code{character(1)} specifying the name of
      the variable that is being read.}

    \item{\code{slice}:}{A named numeric vector specifying the size of
      the chunk of data that will be retrived along each dimension using
      the \code{yield} method.}

    \item{\code{start}:}{A named numeric vector specifying the position
      along each dimension from which data will start to be read for the
      next call to the \code{yield} function.}

    \item{\code{verbose}:}{Report messages from evalaution?}

  }
}

\section{Class Internal Methods: (For developers)}{
  \describe{

    \item{\code{initialize(ncdf, var, slice, ..., verbose)}:}{Called
      during object creation with values to initialize fields.}

    \item{\code{yield()}:}{Processes the NetCDF file and retrieves a
      block of data from the NetCDF file corresponding to the slice size
      that has been set.  Repeated calls to the \code{yield} function
      retrieves the next block of data until the end of file has been
      reached.}

    \item{\code{reset()}:}{Resets the cursor that tracks the next block
      of data to be read to the start of the file.}

    \item{\code{status()}:}{Retrieves the position of the start of the
      block from which data will be read for the next call to the
      \code{yield} function.}

    \item{\code{.getCounts()}:}{Retrieve the dimensions of the next
      slice.}

    \item{\code{.getNextStart()}:}{Retrieve coordinates at which next
      yield starts.}

  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{NetCDFFile}}}

\examples{
showClass("NetCDFInput")

pth <- system.file("extdata", "NetCDFData.nc", package = "Streamer")
ncFile <- NetCDFFile(pth)
dimensions(ncFile)		# variable names and dimensions

ncProd <- NetCDFInput(ncFile, "2dIntData", c(sampleDim=5, snpDim=2))
yield(ncProd)
status(ncProd)
reset(ncProd)
yield(ncProd)
ncProd
}
\keyword{classes}
