.NetCDFFile <- setRefClass("NetCDFFile",
    fields = list(
                  con = "ANY",
                  variableNames = "character",
                  dimensionNames = "list",
                  precision = "character",
                  numDims = "integer"))

.NetCDFFile$methods(
    initialize = function(file = NULL, ...) {
        "Initialize all the fields of the NetCDFFile class"
        if(!is.null(file)) {
            .self$con <- nc_open(file)
            .self$variableNames <- .getNcdfVariableNames(con)
            .self$dimensionNames <- .getNcdfDimNames(con)
            .self$dimensionLengths <- .getNcdfDimLengths(con)
            .self$numDims <- .getNcdfNumDims(con)
            .self$precision <- .getNcdfPrecision(con)
        }
        .self
    },
    getVariableNames = function() {
        "Returns the names for all variables in the file"
        variableNames
    },
    getDimensionNames = function() {
        "Returns the names of the dimensions "
        .getFieldValue(.self, "dimensionNames")
    }, 
    getDimensionLengths = function() {
        "Returns the dimension lengths"
        .getFieldValue(.self, "dimensionLengths")
    },
    getDimensionCounts = function() {
        "Returns the number of dimensions"
        .getFieldValue(.self, "numDims")
    },
    getPrecision = function() {
        "Returns the storage precision for a variable"
        .getFieldValue(.self, "precision") 
    },
    finalize = function(){
        if(!is(con, "uninitializedField"))
           nc_close(con)
    }
)

## retrieves the field from the NetCDFFile class
.getFieldValue <- function(self, field) {
    return(self[[field]])
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
        sapply(seq_len(ndims), function(i) {
               nc$var[[varid]]$dim[[i]][[info]]
        })
    }
    structure(
        lapply(vars, function(i) {
               val <- get_dim_info(i, info)
               if(info == "len") {
                   ndims <- nc$var[[i]]$ndims
                   nms <- sapply(seq_len(ndims), function(j) {
                                 nc$var[[i]]$dim[[j]]$name
                   })
                   names(val) <- nms
                }
                val
        }), names = vars)
}

## gets  names of variables in NetCDF file  
.getNcdfVariableNames <- function(nc) {
    names(nc$var)
}


## gets the dimension names for all variables as a named list
.getNcdfDimNames <- function(nc) {
    .getNcdfDimInfo(nc, "name")
}

## gets the dimension lengths for all variables as a named list
.getNcdfDimLengths  <- function(nc) {
    .getNcdfDimInfo(nc,"len")
}


## gets the number of dimensions for all variables in the file
.getNcdfNumDims <- function(nc) {
    .getNcdfInfo(nc, "ndims")
}

## gets precision info for all variables in file 
.getNcdfPrecision <- function(nc) {
    .getNcdfInfo(nc, "prec")

}

## Constructor for NetCDFFile class
NetCDFFile <- function(file) {
    .NetCDFFile$new(file)
}


