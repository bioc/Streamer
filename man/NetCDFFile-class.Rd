\name{NetCDFFile-class}

\Rdversion{1.1}
\docType{class}

\alias{NetCDFFile-class}
\alias{NetCDFFile}
\alias{precision}
\alias{dimensions}
\alias{precision,NetCDFFile-method}
\alias{dimensions,NetCDFFile-method}

\title{Class "NetCDFFile"}

\description{

  A \code{\linkS4class{NetCDFFile}}-class to interpret data stored in
  NetCDF files. Users interact with this class through the constructor
  \code{\link{NetCDFFile}} and methods \code{\link{precision}}, and
  \code{\link{dimensions}}.

}

\usage{
NetCDFFile(file = character(), ...)
\S4method{dimensions}{NetCDFFile}(x, ...)
\S4method{precision}{NetCDFFile}(x, ...)
}

\arguments{

  \item{file}{A character string for the path to the NetCDF file.}

  \item{x}{An insance of the \code{NetCDFFile} class.}

  \item{...}{Additional arguments, passed to the \code{$new} method of
    this class. Currently ignored.}

}

\section{Class Methods}{
  \describe{

    \item{\code{precision()}:}{Returns a named character vector
      corresponding to the storage precision of the variables in the
      NetCDF file.}

    \item{\code{dimensions()}:}{Returns a named \code{list} containing
      the names and lengths of the dimensions for each variable in the
      NetCDF file.}

  }
}

\section{Class Internal Fields: (For developers)}{
  \describe{

    \item{\code{con}:}{Object of class \code{ncdf4}. An R ncdf4
      connection obtained by opening a NetCDF file from which data is to
      be read using the \code{nc_open} function.}

    \item{\code{dimensions}:}{A named \code{list} corresponding to the
      names of the variables in the NetCDF file. Each element of the
      \code{list} is a named integer vector, with names of the
      dimensions for each variable and values the length of the
      dimension in the NetCDF file.}

    \item{\code{precision}:}{A named character vector of the number of
      precision for each variable stored in the NetCDF file.}

  }
}

\section{Class Internal Methods: (For developers)}{
  \describe{

    \item{\code{initialize(file=character(), ...)}:}{Called during
      object creation with \code{file} being the path to a valid NetCDF
      file.}

    \item{\code{getPath()}:}{Retrive the path to the NetCDF file.}

    \item{\code{getDimensions()}:}{Retrieves a list of variables, with
      each element in the list containing a named integer vector of
      dimensions and their lengths.}

    \item{\code{getPrecision()}:}{Retrieves the precision of each
      variable in the NetCDF file.}

  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{
\dontrun{
pth <- system.file("extdata", "NetCDFData.nc", package = "Streamer")
ncFile <- NetCDFFile(pth)
dimensions(ncFile)
precision(ncFile)
ncFile
}
}
\keyword{classes}
