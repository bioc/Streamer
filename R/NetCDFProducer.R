
.NetCDFProducer <- setRefClass("NetCDFProducer",
    contains = c("Producer"),
    fields = list(
        name = "character",
        slice = "numeric",
        start = "numeric",
        count = "numeric",
        ncdf = "NetCDFFile"
        ))

.NetCDFProducer$methods(
    initialize  = function(ncdf, var, ...) {

        "Initialize the fields netCDFSampler class "
        ## efault slices for the getSlice function
        .getDefaultSlice <- function(nc, var) {

            slicePercent = 0.05 
            dimLen <- dimensionLengths(nc)[[var]]
            numDims <- dimensionCount(nc)[[var]]
            dimNames <- dimensions(nc)[[var]]
            if (numDims ==2) {
                mx <- max(round(dimLen[2] * slicePercent), 1)
                return(structure(c(dimLen[1], mx ), names = dimNames))
            } else if (numDims == 3){
                mx <- max(round(dimLen[3] * slicePercent), 1)
                return(structure(c(dimLen[1], dimLen[2], mx ), names = dimNames))
            } else {
                structure(rep(NA, numDims), names = dimNames)
            }

        }

        callSuper(...)
        if (verbose) msg(".NetCDFProducer$initialize()")
        if (is.null(var))
            var <- names(ncdf)[1]
        .self$name <- var
        .self$slice <- .self$count <- .getDefaultSlice(ncdf, var) 
        .self$start <- .initializeStart(ncdf, var)
        .self$ncdf <- ncdf
        .self

    },
    getSlice = function() {

        "Returns the slice dimensions"
        if (verbose) msg(".NetCDFProducer$getSlice()")
        .self$slice
    },
    setSlice = function(dim, ...) {

        "Sets the slice dimensions"
        if (verbose) msg(".NetCDFProducer$setSlice()")
        .checkDimension <- function(dim) {
            ## FIXME: inside the class we should use class methods / variables
            numDim <- dimensionCount(ncdf)[[name]]
            dimNames <- dimensions(ncdf)[[name]]
            dimLength <- dimensionLengths(ncdf)[[name]]
            if(length(dim) != numDim) {
                msg <-
                    sprintf("'dim' length must equal dimension number of '%s'",
                            name)
                stop(msg)
            }
            nms <- names(dim) %in% dimNames
            if(!(all(nms) && length(nms))) {
                msg <-
                    sprintf("'dim' names must equal dimension names of '%s'",
                            name)
                stop(msg)
            }
            
            for (i in seq_len(numDim)) {
                if ( dim[i] < 1 || dim[i] > dimLength[i]) {
                    msg <-
                        sprintf("'%s' slice dimension must be in range 1, %d",
                                dimNames[i], dimLength[i])
                    stop(msg)
                }
            }
        }
        .checkDimension(dim) 
        .self$start <- .initializeStart(ncdf, name)
        .self$slice <- .self$count <- dim[dimensions(ncdf)[[name]]]
        .self
    }, 
    status = function() {
        "Gets the current start position of the slice being read"
        if (verbose) msg(".NetCDFProducer$status()")
        if(all(start == -1))
            dimensionLengths(ncdf)[[name]]
        else
            start
    
    },
    reset = function() {
    
        "Resets the position to be read to start of the file"
        callSuper()
        if (verbose) msg(".NetCDFProducer$reset()")
        .self$start <- .initializeStart(ncdf, name)
        .self$count <- slice
    
    },
    yield = function() {

        "Reads a slice. Repeated calls retrieves the next chunk of data until 
        the end of file has been reached."
        if (verbose) msg(".NetCDFProducer$yield()")
        dimLen <- dimensionLengths(ncdf)[[name]]
        if ( all(start == -1)) {
            message("Reached the end of the file ")
            ## FIXME: correct type for variable
            return(matrix(numeric(0), 0, 0))
        }
        nd <- dimensionCount(ncdf)[[name]]
        count <<- .getCurrentCount(start, slice, dimLen, count)
        dat <- ncvar_get(ncdf$con, name, start = as.vector(start), 
                         count = as.vector(count))
        start <<- .getNextStart(start, slice, dimLen, nd) 
        dat
    }
)

## Constructor for NetCDF producer class
NetCDFProducer <- function(ncdf, var = NULL, ..., verbose = FALSE) 
{
    if (missing(ncdf) || !is(ncdf, "NetCDFFile"))
        stop("'ncdf' must be a valid object of class 'NetCDFFile'")
    if (!is.character(var) || 1L != length(var) || !var %in% names(ncdf))
        stop("'var' must be one variable in 'file'")
    .NetCDFProducer$new(ncdf = ncdf, var = var, ... , verbose = verbose)

}

## Accessor methods for NetCDFProducer class

setMethod("names", "NetCDFProducer", function(x) x$getNames())

setMethod("slice", "NetCDFProducer", function(x) x$getSlice())

setMethod("yield", "NetCDFProducer", function(x) x$yield())

setMethod("reset", "NetCDFProducer", function(x) x$reset())

setMethod("status", "NetCDFProducer", function(x) x$status())

## Replacement methods for NetCDFProducer class
setReplaceMethod("slice", c("NetCDFProducer", "list"),
    function(x, ..., value) 
{
    value <- unlist(value)
    slice(x, ...) <- value
    x
})

setReplaceMethod("slice", c("NetCDFProducer", "numeric"),
    function(x, ..., value)
{
    nms <- names(value)
    if (!all( nms %in% names(slice(x))))
        stop("'names(value)' must match 'names(slice(x))'")
    x$setSlice(value)                 
})

## default start value for the cursor in NetCDF file
.initializeStart <- function(nc, var) {

    dimNames <- dimensions(nc)[[var]]
    dimLen <- dimensionLengths(nc)[[var]]
    numDims <- dimensionCount(nc)[[var]]
    structure(rep(1, numDims), names = dimNames)

}

## Helper functions for yield function
## Gets the start position for the next read
.getNextStart <- function(start, slice, dimLen , nd) {

    i <- 1
    new_start <- start
    while (i <= nd) {
        if ( start[i] + slice[i]  == dimLen[i] +1) {
            new_start[i] <- 1
            i <- i +1
        } else if (start[i] + slice[i] <= dimLen[i]) {
            new_start[i] <- slice[i] + start[i] 
            i <- nd +1
        } else if (start[i] + slice[i]  > dimLen[i] +1) {
            new_start[i] <- 1
            i <- i +1
        }
    }
    if (all(new_start == 1))
        new_start <- structure(rep(-1, nd), names = names(start))
    new_start

}

## Gets the count to be read for current yield being processed
.getCurrentCount <- function(start, slice, dimLen, count) {

    val <-  start + slice -1  
    indx <- val <= dimLen
    count[indx] <- slice[indx]
    tmp <- dimLen - start +1
    count[!indx] <- tmp[!indx]
    count                          

}

