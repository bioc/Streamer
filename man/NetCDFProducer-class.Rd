\name{NetCDFProducer-class}

\Rdversion{1.1}
\docType{class}

\alias{NetCDFProducer-class}
\alias{NetCDFProducer}
\alias{status}
\alias{slice}
\alias{slice<-}
\alias{names,NetCDFProducer-method}
\alias{slice,NetCDFProducer-method}
\alias{reset,NetCDFProducer-method}
\alias{yield,NetCDFProducer-method}
\alias{status,NetCDFProducer-method}
\alias{slice,NetCDFProducer-method}
\alias{slice<-,NetCDFProducer,list-method}
\alias{slice<-,NetCDFProducer,numeric-method}

\title{Class "NetCDFProducer"}

\description{

  A \code{\linkS4class{NetCDFProducer}}-class to retrieve data store in NetCDF
  files. Users interact with this class through the constructor
  \code{\link{NetCDFProducer}} and methods \code{\link{names}},
  \code{\link{slice}}, \code{\link{slice<-}}, \code{\link{yield}} 
   and \code{\link{status}}.
}

\usage{
NetCDFProducer(ncdf, var = NULL, ..., verbose = FALSE)
}

\arguments{
  \item{ncdf}{An object of class \code{NetCDFFile} for the NetCDF file from which
  data is to be read.}
  \item{var}{A character string for name of the variable to be read from the 
  NetCDF file. Only a single variable  can be specified.}
  \item{...}{Additional arguments, passed to the \code{$new} method of
    this class. Currently ignored.}
  \item{verbose}{\code{logical(1)} indicating whether class methods
    should report to the user.}

}

\section{Fields}{
  \describe{
    \item{\code{name}:}{ A character vector specifying the name of the variable 
        that is  being read using the \code{NetCDFProducer} class}
    \item{\code{slice}:}{ A named numeric vector specifying the size of the chunk of
        data that will be retrived along each dimension using the \code{yield} 
        method.}
    \item{\code{start}:}{ A named numeric vector specifying the position along each
        dimension from which data will be read for the next call to the \code{yield}
        function.}
   \item{\code{count}:}{ A named numeric vector specifying the count of the data 
        that will be read along each dimension for the current call to the 
        \code{yield} function.}
    \item{\code{ncdf}:}{An object of class \code{NetCDFFile} for the NetCDF file
        from which data is being read.}
     }
}

\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(ncdf, var, ..., verbose)}:}{Called during object creation with values to
      initialize fields.}
    \item{\code{getSlice()}:}{Retrieves the size of data chunk along each dimension
        that will be retreived using the \code{yield} function.}
    \item{\code{yield()}:}{Processes the NetCDF file and retrieves a block of data 
      from the NetCDF file corresponding to the slice size that has been set.
      Repeated calls to the \code{yield} function retrieves the next block of data
     until the end of file has been reached.}
    \item{\code{reset()}:}{Resets the cursor that tracks the next block of data
        to be read to the start of the file.}
    \item{\code{status()}:}{Retrieves the position of the start of the
    block from which data will be read for the next call to the \code{yield} 
    function.}
    \item{\code{setSlice(dim,...)}:}{Can be used to change the block size of data 
    along each dimension that will be retrieved by the \code{yield} function. A
    call to this function resets the next block of data to be read to the start 
    of the NetCDF file.}
  }
}


\section{Class-Wrapper Methods}{
  \describe{
    \item{\code{names()}:}{Returns the name of the variable being processed by the 
     NetCDFProducer class.}
    \item{\code{slice()}:}{Returns a named  numeric vector for the block size of
        data that will be retrieved using the \code{yield} function.}
    \item{\code{yield()}:}{Processes the NetCDF file and retrieves a matrix of data 
      from the NetCDF file corresponding to the slice size that has been set.
      Repeated calls to the \code{yield} function retrieves the next block of data
     until the end of file has been reached.}
    \item{\code{reset()}:}{Resets the cursor that tracks the next block of data
        to be read to the start of the file.}
     \item{\code{status()}:}{Returns a named numeric vector for the position of 
        the start of the  block from which data will be read for the next call
        to the \code{yield} function.}
  }
}

\author{Nishant Gopalakrishnan \url{ngopalak@fhcrc.org}}

\seealso{\code{\link{NetCDFFile}}}

\examples{
showClass("NetCDFProducer")
pth <- system.file("extdata", "NetCDFData.nc", package = "Streamer")
nFile <- NetCDFFile(pth)
ncProd <- NetCDFProducer(nFile, "2dIntData")
slice(ncProd) <- list(sampleDim = 5, snpDim = 2)
yield(ncProd)
status(ncProd)
reset(ncProd)
yield(ncProd)
}
\keyword{classes}
