.NetCDFProducer <- setRefClass("NetCDFProducer",
                               contains = "Producer",
                               fields = list(
                                            con = "ANY",
                                            variableNames = "character",
                                            dimensionNames = "list",
                                            dimensionLengths = "list",
                                            precision = "character",
                                            numDims = "integer",
                                            sliceDimensions = "list",
                                            readStarts = "list",
                                            readCounts = "list"
                                            ))

.NetCDFProducer$methods(
                        initialize  = function(file) {
                            "Initialize slots of netCDFSampler class for all variables
                            in file "
                            .self$con <- nc_open(file)
                            .self$variableNames <- .getNcdfVariableNames(con)
                            .self$dimensionNames <- .getNcdfDimNames(con)
                            .self$dimensionLengths <- .getNcdfDimLengths(con)
                            .self$precision <- .getNcdfPrecision(con)
                            .self$numDims <- .getNcdfNumDims(con)
                            .self$sliceDimensions <-.self$readCounts <- .getDefaultSlice(con)
                            .self$readStarts <- .initializeStarts(con)
                            .self 
                        },
                        getVariableNames = function() {
                            "Returns the names for all variables in the file"
                            .getNcdfVariableNames(con)
                        },
                        getSliceDimensions = function(var) {
                            "Returns the slice dimensions for a variable"
                            if (missing(var))
                                return (sliceDimensions)
                            else
                                return (sliceDimensions[[var]])
                        },
                        setSliceDimensions = function(var, dim, ...) {
                            "Sets the default slice dimensions for a variable.
                            This determines the size of the slice retrieved by the
                            getSlice function"

                            if(missing(var) && length(variableNames) == 1L)
                                var <- variableNames
                            if(missing(dim))
                                dim <- unlist(list(...))
                            .checkSliceDimensions <- function(var, dim, .self) {
                                numDim <- .self$numDims[[var]]
                                dimName <- .self$dimensionNames[[var]]
                                dimLength <- .self$dimensionLengths[[var]]

                                if(length(var) != 1L)
                                    stop("'var' should specify a single variable")

                                if(length(dim) != numDim)
                                    stop(paste("'dim' should have the same length as the number of dimensions of", var, sep =" "))

                                nms <- names(dim) %in% dimName
                                if(!(all(nms) && length(nms)))
                                    stop(paste("'dim' should have names corresponding to the names of dimensions for the variable",
                                               var , sep =" "))
                                dim <- dim[dimName]
                                sapply(seq_len(numDim), function(i) {
                                       if( dim[i] < 1 || dim[i] > dimLength[i])
                                           stop(paste("Slice dimension for", dimName[i],
                                                      "should be within the range 1 and",
                                                      dimLength[i], sep = " "))
                                            })
                            }
                            .checkSliceDimensions(var, dim, .self) 
                            dim <- dim[dimensionNames[[var]]]
                            .self$sliceDimensions[[var]] <- .self$readCounts[[var]] <- dim
                        },
                        status = function(var) {
                            "Gets the current position of the block being read for a
                            particular variable"
                            ## FIXME
                            if(missing(var))
                                var <- variableNames[1]
                            if(length(var) != 1L)
                                stop("A single variable name must be specified")
                            if(!(var %in% variableNames))
                                stop("Variable specified was not found in the ncdfSampler class")
                            starts <- readStarts[[var]]
                            if(all(starts == -1))
                                return(structure(dimensionLengths[[var]], 
                                                 names = names(starts)))
                            else
                                return(starts)

                        },
                        yield = function(var) {
                            "Reads a slice for a variable. Repeated calls retrieves
                            the next chunk of data for the variable until the end of
                            file has been reached."
                            if (missing(var)) {
                                if (length(variableNames) != 1)
                                    stop("NetCDF file contains multiple varialbles: Please specify a variable name") 
                                else 
                                    var <- variableNames
                            }
                            if (length(var) != 1)
                                stop( "Please specify a single variable name")
                            
                            if(!(var %in% variableNames))
                                stop("Variable specified was not found in the ncdfSampler class")

                            slice <- sliceDimensions[[var]]
                            start <- readStarts[[var]]
                            count <- readCounts[[var]]
                            dimLen <- dimensionLengths[[var]]
                            if( all(start == -1)) {
                                message("Reached the end of the file ")
                                return(matrix(numeric(0), 0, 0))   

                            }
                            nd <- numDims[[var]]
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
                                    new_start <- structure(rep(-1, nd), names =
                                                           names(start))
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
                            new_start <- .getNextStart(start, slice, dimLen, nd) 
                            count <- .getCurrentCount(start, slice, dimLen, count)
                            readStarts[[var]] <<- new_start
                            readCounts[[var]] <<- count
                            ncvar_get(con, var, start = as.vector(start), 
                                      count = as.vector(count)  )
                        },
                        reset = function(var) {
                            "Resets the position to be read to start of the
                            file"
                            if(missing(var))
                                var <- variableNames
                            if(!all(var %in% variableNames))
                                stop("Not all variables specified were
                                     found in the ncdfSampler")
                            for(i in var) {
                                readStarts[[i]] <<- .initializeStarts(con)[[i]]
                                readCounts[[i]] <<- sliceDimensions[[i]]
                            }
                        },
                        finalize = function(){
                            nc_close(con)
                        }
                        )

## Constructor and other functions exposed to user

NetCDFProducer <- function(file) {
    .NetCDFProducer$new(file)
}




## Helper for precision and number of dims
.getNcdfInfo <- function(nc, info) {
    vars <- .getNcdfVariableNames(nc)
    structure(sapply(seq_len(length(vars)), function(i) {
                     nc$var[[i]][[info]]

    }), names = vars)
}

## Helper for dimension names, dim lengths
.getNcdfDimInfo <- function(nc,info ) {
    vars <- .getNcdfVariableNames(nc)
    get_dim_info <- function(varid, info) {
        ndims <- nc$var[[varid]]$ndims
        sapply(seq_len(ndims), function(i) nc$var[[varid]]$dim[[i]][[info]])
    }

    structure(lapply(vars, function(i) {
               get_dim_info(i, info)

               
               }), names = vars)
}


## gets variable names in a file 
.getNcdfVariableNames <- function(nc) {
    names(nc$var)
}


## gets precision info for all variables in file 
.getNcdfPrecision <- function(nc) {
    .getNcdfInfo(nc, "prec")

}

## gets the number of dimensions for all variables in the file
.getNcdfNumDims <- function(nc) {
    .getNcdfInfo(nc, "ndims")
}

## gets the dimension names for all variables as a named list
.getNcdfDimNames <- function(nc) {
    .getNcdfDimInfo(nc, "name")
}

## gets the dimension lengths for all variables as a named list
.getNcdfDimLengths  <- function(nc) {
    .getNcdfDimInfo(nc,"len")
}


## sets the default slices for the getSlice function
.getDefaultSlice <- function(nc) {
    variableNames <- .getNcdfVariableNames(nc)
    dimensionLengths <- .getNcdfDimLengths(nc)
    numDims <- .getNcdfNumDims(nc)
    dimensionNames <- .getNcdfDimNames(nc)
    structure(
        lapply(variableNames, function(i){
               dmlen <- dimensionLengths[[i]]
    nmdim <- numDims[[i]]
    dimnms <- dimensionNames[[i]]
    if(nmdim ==2) {
        mx <- max(round(dmlen[2] * 5 /100), 1)
        return(structure(c(dmlen[1], mx ), names = dimnms))
    } else if(nmdim == 3){
        mx <- max(round(dmlen[3] * 5 /100), 1)
        return(structure(c(dmlen[1], dmlen[2], mx ), names = dimnms))
    } else {
        structure(rep(NA, nmdim), names = dimnms)

    }

               }),  names = variableNames)
}


.initializeStarts <- function(nc) {
    variableNames <- .getNcdfVariableNames(nc)
    dimensionLengths <- .getNcdfDimLengths(nc)
    numDims <- .getNcdfNumDims(nc)
    dimensionNames <- .getNcdfDimNames(nc)
    structure(
        lapply(variableNames, function(i){
               dmlen <- dimensionLengths[[i]]
    nmdim <- numDims[[i]]
    dimnms <- dimensionNames[[i]]
           structure(rep(1, nmdim), names = dimnms)
               }),  names = variableNames)
}


# 
# 
#gets precicision, dimnames and lengths for a single variable 
# .getNcdfVarSummary <- function(nc, varid)
# {
#     ans_prec <- nc$var[[varid]]$prec
#     ans_ndims <- nc$var[[varid]]$ndims
#     get_dim_info <- function(varid, info) {
#         ndims <- nc$var[[varid]]$ndims
#         sapply(seq_len(ndims), function(i) nc$var[[varid]]$dim[[i]][[info]])
#     }
#     ans_dim_names <- get_dim_info(varid, "name")
#     ans_dim_lengths <- get_dim_info(varid, "len")
#     list(name = varid, prec = ans_prec, ndims = ans_ndims,
#                dimnames = ans_dim_names, dimlengths = ans_dim_lengths)
# }
#                            
