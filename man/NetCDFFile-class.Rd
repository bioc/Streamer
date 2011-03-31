\name{NetCDFFile-class}

\Rdversion{1.1}
\docType{class}

\alias{NetCDFFile-class}
\alias{NetCDFFile}
\alias{names}
\alias{precision}
\alias{dimensions}
\alias{dimensionLengths}
\alias{dimensionCount}
\alias{names,NetCDFFile-method}
\alias{precision,NetCDFFile-method}
\alias{dimensions,NetCDFFile-method}
\alias{dimensionLengths,NetCDFFile-method}
\alias{dimensionCount,NetCDFFile-method}

\title{Class "NetCDFFile"}

\description{

  A \code{\linkS4class{NetCDFFile}}-class to interpret data stored in NetCDF 
  files. Users interact with this class through the constructor
  \code{\link{NetCDFFile}} and methods \code{\link{names}},
  \code{\link{precision}}, \code{\link{dimensions}}, \code{\link{dimensionCount}} 
   and \code{\link{dimensionLengths}}.
}

\usage{
NetCDFFile(file = NULL, ...)
}

\arguments{
  \item{file}{A character string for the path to the NetCDF file.}
  \item{...}{Additional arguments, passed to the \code{$new} method of
    this class. Currently ignored.}
}

\section{Class Methods}{
  \describe{
    \item{\code{names()}:}{Returns the names of variables in the NetCDF file.}
    \item{\code{precision()}:}{Returns a named character vector corresponding to
        the storage precision of the variables in the NetCDF file.}
    \item{\code{dimensions()}:}{Returns a named \code{list} containing the names of 
        the dimensions for each variable in the NetCDF file. }
    \item{\code{dimensionLengths()}:}{Returns a named \code{list} containing named numeric
       vectors of the length of dimensions for each variable.}
    \item{\code{dimensionCount()}:}{Returns a named numeric vector the number of 
       dimensions for each variable.}
  }
}

\section{Class Internal Fields: (For developers)}{
  \describe{
    \item{\code{con}:}{Object of class \code{ncdf4}. An R
       ncdf4 connection obtained by opening a NetCDF file from which data 
       is to be read using the \code{nc_open} function.}
    \item{\code{variableNames}:}{ Names of the variables stored in the NetCDF
        file.}
    \item{\code{dimensionNames}:}{ A named \code{list} corresponding to the
        names of the variables in the NetCDF file. Each element of the
        \code{list} is a character vector of the names of the dimensions for
        each variable in the NetCDF file.}
    \item{\code{dimensionLengths}:}{ A named \code{list} corresponding to the
        names of the variables in the NetCDF file. Each element of the
        \code{list} is a named numeric vector of the lengths of the dimensions
        of each variable.}
     \item{\code{numDims}:}{A named numeric vector of the number of
         dimensions for each variable in the NetCDF file.}
     \item{\code{precision}:}{A named character vector of the number of
         precision for each variable stored in the NetCDF file.}
  }
}

\section{Class Internal Methods: (For developers)}{
  \describe{
    \item{\code{initialize(con, variableNames, dimensionNames, dimensionLengths,
	numDims, precision)}:}{Called during object creation with values to
      initialize fields.}
    \item{\code{getVariableNames()}:}{Retrieves the names for each variable in
        the NetCDF file.}
    \item{\code{getDimensionNames()}:}{Retrieves the names of the dimensions for 
        all variables in the NetCDF file.}
    \item{\code{getDimensionLengths()}:}{Retrieves the length of dimensions
        for each variable in the NetCDF file.}
    \item{\code{getDimensionCounts()}:}{Retrieves the number of dimensions for
        each variable in the NetCDF file.}
    \item{\code{getPrecision()}:}{Retrieves the precision of each variable in 
        the NetCDF file.}
    \item{\code{finalize()}:}{Closes the NetCDF connection \code{con}.}
  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{stream}}}

\examples{
showClass("NetCDFFile")
pth <- system.file("extdata", "NetCDFData.nc", package = "Streamer")
nFile <- NetCDFFile(pth)
names(nFile)
dimensions(nFile)
dimensionLengths(nFile)
dimensionCount(nFile)
precision(nFile)
}
\keyword{classes}
