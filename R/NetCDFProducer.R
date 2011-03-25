
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
        "Initialize slots of netCDFSampler class for all variables
        in file "
        if(is.null(var))
            var <- names(ncdf)[1]
        .self$name <- var
        .self$slice <- .self$count <- .getDefaultSlice(ncdf, var) 
        .self$start <- .initializeStart(ncdf, var)
        .self$ncdf <- ncdf
        .self 
    },
    getSlice = function() {
        "Returns the slice dimensions"
        slice                        
    },
    setSlice = function(dim, ...) {
        .checkDimension <- function(dim) {
            numDim <- dimensionCount(ncdf)[[name]]
            dimNames <- dimensions(ncdf)[[name]]
            dimLength <- dimensionLengths(ncdf)[[name]]
            if(length(dim) != numDim)
                stop(paste("'dim' should have the same length as the number of dimensions of", name, sep =" "))
 
            nms <- names(dim) %in% dimNames
            if(!(all(nms) && length(nms)))
                stop(paste("'dim' should have names corresponding to the names of dimensions for the variable",
                           name , sep =" "))
            sapply(seq_len(numDim), function(i) {
                   if( dim[i] < 1 || dim[i] > dimLength[i])
                       stop(paste("Slice dimension for", dimNames[i],  "should be within the range 1 and", dimLength[i], sep = " "))
                  }
            )
        }
        .checkDimension(dim) 
        .self$start <- .initializeStart(ncdf, name)
        .self$slice <- .self$count <- dim[dimensions(ncdf)[[name]]]
    }, 
    status = function() {
        "Gets the current start position of the slice being read"
        if(all(start == -1))
            return(dimensionLengths(ncdf)[[name]])
        else
            return(start)
    },
    reset = function() {
        "Resets the position to be read to start of the file"
        .self$start <- .initializeStart(ncdf, name)
        .self$count <- slice
    },
    yield = function() {
        "Reads a slice. Repeated calls retrieves the next chunk of data until 
        the end of file has been reached."
        dimLen <- dimensionLengths(ncdf)[[name]]
        if( all(start == -1)) {
            message("Reached the end of the file ")
            return(matrix(numeric(0), 0, 0))   

        }
        nd <- dimensionCount(ncdf)[[name]]
        count <<- .getCurrentCount(start, slice, dimLen, count)
        dat <- ncvar_get(ncdf$con, name, start = as.vector(start), count = as.vector(count))
        start <<- .getNextStart(start, slice, dimLen, nd) 
        dat

    }
)


## Constructor for NetCDF producer class
NetCDFProducer <- function(ncdf, var = NULL) {
   
    if(!is(ncdf, "NetCDFFile") || missing(ncdf))
        stop(paste(ncdf, "should be a valid object of class \"NetCDFFile\"", sep = " "))
    
    if(!is.null(var)) {
        if(!var %in% names(ncdf))
            stop("var must be one of the  variables in the NetCDF file")
    }
    
    if(length(var) != 1)
        stop("var must specify a single variable")
    .NetCDFProducer$new(ncdf, var)
}

## Helper functions for NetCDF producer class

## sets the default slices for the getSlice function
.getDefaultSlice <- function(nc, var) {
    slicePercent = 0.05 
    dimLen <- dimensionLengths(nc)[[var]]
    numDims <- dimensionCount(nc)[[var]]
    dimNames <- dimensions(nc)[[var]]
    if(numDims ==2) {
        mx <- max(round(dimLen[2] * slicePercent), 1)
        return(structure(c(dimLen[1], mx ), names = dimNames))
    } else if(numDims == 3){
        mx <- max(round(dimLen[3] * slicePercent), 1)
        return(structure(c(dimLen[1], dimLen[2], mx ), names = dimNames))
    } else {
        structure(rep(NA, numDims), names = diimNames)
    }
}

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
        } else if(start[i] + slice[i] <= dimLen[i]) {
            new_start[i] <- slice[i] + start[i] 
            i <- nd +1
        } else if(start[i] + slice[i]  > dimLen[i] +1) {
            new_start[i] <- 1
            i <- i +1
        }
    }
    if(all(new_start == 1))
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

