.NetCDFInput <- setRefClass("NetCDFInput",
    contains = c("Producer"),
    fields = list(ncdf="NetCDFFile", name="character",
      slice="integer", start="integer"))

.NetCDFInput$methods(
    .validSlice = function(slc)
    {
        "Validate slice dimensions"
        if (verbose) msg(".NetCDFInput$.validSlice()")
        dims <- dimensions(ncdf)[[name]]
        if(length(dims) != length(slc)) {
            msg <-
                sprintf("'slice' length must equal length of '%s'", name)
            stop(msg)
        }
        nms <- names(slc) %in% names(dims)
        if (!all(nms)) {
            msg <- sprintf("'slice' names must be in names of '%s'", name)
            stop(msg)
        }
        nms <- names(dims) %in% names(slc)
        if (!all(nms)) {
            msg <- sprintf("'%s' names must be in names of 'slice'", name)
            stop(msg)
        }
        slc <- slc[names(dims)]
        ok <- slc > 0 & slc <= dims
        if (!all(ok)) {
            msg <- sprintf("slice dimension '%s' must be >0, <= %d",
                           names(slc)[!ok], dims[slc[!ok]])
            stop(msg)
        }
        slc
    },
    initialize = function(ncdf, var, slice, ...)
    {
        "Initialize the fields of a netCDFSampler class "
        callSuper(..., yieldSize=prod(slice))
        if (verbose) msg(".NetCDFInput$initialize()")
        .self$ncdf <- ncdf
        .self$name <- var
        .self$slice <- .validSlice(slice)
        reset()
    },
    status = function() {
        "Gets the current start position of the slice being read"
        if (verbose) msg(".NetCDFInput$status()")
        if(all(start == -1))
            dimensions(ncdf)[[name]]
        else
            start
    },
    reset = function() {
        "Resets the position to be read to start of the file"
        callSuper()
        if (verbose) msg(".NetCDFInput$reset()")
        dimNames <- names(dimensions(ncdf)[[name]])
        .self$start <-
            structure(rep(1L, length(dimNames)), names = dimNames)
        invisible(.self)
    },
    ## yield-related
    .getCounts = function()
    {
        "Calculate next 'count' argument for ncdf subset call"
        dims <- dimensions(ncdf)[[name]]
        count <- slice
        indx <- (start + slice - 1L) > dims
        count[indx] <- (dims - start + 1L)[indx]
        as.vector(count)
    },
    .getNextStart = function()
    {
        dims <- dimensions(ncdf)[[name]]
        nd <- length(dims)
        i <- 1L
        new_start <- start
        while (i <= nd) {
            if (start[i] + slice[i]  == dims[i] + 1L) {
                new_start[i] <- 1L
                i <- i + 1L
            } else if (start[i] + slice[i] <= dims[i]) {
                new_start[i] <- slice[i] + start[i] 
                i <- nd + 1L
            } else if (start[i] + slice[i]  > dims[i] + 1L) {
                new_start[i] <- 1L
                i <- i + 1L
            }
        }
        if (all(1L == new_start))
            new_start <- structure(rep(-1L, nd), names = names(start))
        new_start

    },
    yield = function() {
        "Reads a slice. Repeated calls retrieves the next chunk of data until 
         the end of file has been reached."
        if (verbose) msg(".NetCDFInput$yield()")
        if (all(-1L == start)) {
            if (verbose) msg("end of file")
            prec <- precision(ncdf)[[name]]
            typ <- if(prec == "float" || prec == "double") numeric() else integer()
            return(structure(typ, .Dim=integer(length(start))))
        }
        count <- .getCounts()
        dat <- ncvar_get(ncdf$con, name, start = as.vector(start), 
                         count = count)
        .self$start <- .getNextStart()
        if (1L != length(count))
            dim(dat) <- count
        dat
    }
)

## Constructor for NetCDF producer class

.getDefaultSlice <-
    function(nc, var, slicePercent=0.05)
{
    dims <- dimensions(nc)[[var]]
    numDims <- length(dims)
    dimNames <- names(dims)
    if (2L == numDims) {
        mx <- max(round(dims[2] * slicePercent), 1)
        structure(c(dims[1], mx), names = dimNames)
    } else if (3L == numDims) {
        mx <- max(round(dims[3] * slicePercent), 1)
        structure(c(dims[1], dims[2], mx), names = dimNames)
    } else {
        structure(rep(NA, length(dims)), names = dimNames)
    }
}

NetCDFInput <-
    function(ncdf, var, slice, ..., verbose = FALSE) 
{
    if (missing(ncdf) || !is(ncdf, "NetCDFFile"))
        stop("'ncdf' must be a valid 'NetCDFFile' instance")
    if (missing(var) || 1L != length(var) ||
        !var %in% names(dimensions(ncdf)))
        stop("'var' must be one variable in 'file'")
    if (missing(slice))
        slice <- .getDefaultSlice(ncdf, var)
    .NetCDFInput$new(ncdf = ncdf, var = var, slice=slice, ...,
                        verbose = verbose)
}

setMethod(dimensions, "NetCDFInput",
          function(x, ...) dimensions(x$ncdf)[x$name])

setMethod(show, "NetCDFInput", function(object)
{
    .p1dim <- function(tag, dim, collapse=" x ")
        sprintf("%s: %s (%s)\n", tag, paste(dim, collapse=collapse),
                paste(names(dim), collapse=collapse))
    callNextMethod()
    cat(.ppath("path", object$ncdf$getPath()))
    dims <- dimensions(object)
    cat("dimensions:", .pdims(dimensions(object)), "\n")
    cat(.p1dim("slice dimension", object$slice))
    cat(.p1dim("start at", object$start, ", "))
})
