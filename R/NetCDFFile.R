.NetCDFFile <- setRefClass("NetCDFFile",
    fields = list(
        con = "ANY",
        dimensions = "list",
        precision = "character"))

.NetCDFFile$methods(
     initialize = function(file = character(0), ...)
     {
         "Initialize all the fields of the NetCDFFile class"

         if (1L == length(file)) {
             .self$con <- nc_open(file)
             .self$dimensions <- lapply(con$var, function(var) {
                 vals <- sapply(var$dim, "[[", "len")
                 names(vals) <- sapply(var$dim, "[[", "name")
                 vals
             })
             .self$precision <- sapply(con$var, "[[", "prec")
         }
         .self
    },
    getPath = function() {
        "Returns the file name of this instance"
        con$filename
    },
    getDimensions = function() {
        "Returns a list of variables and their dimensions"
        dimensions
    },
    getPrecision = function() {
        "Returns the storage precision for a variable"
        precision 
    },
    finalize = function() {
        "Closes the NetCDF file"
        if (!is(con, "uninitializedField"))
            try(nc_close(con), silent=!verbose)
    }
)

## Constructor for NetCDFFile class
NetCDFFile <-
    function(file = character(), ... )
{
    if (1L != length(file) || !file.exists(file))
        stop("'file' must be the path to a single NetCDF file")
    .NetCDFFile$new(file = file, ...)
}

## Accessor methods for NetCDFFile class

setMethod(precision, "NetCDFFile", function(x, ...) x$getPrecision())

setMethod(dimensions, "NetCDFFile", function(x, ...) x$getDimensions())

.pdims <-
    function(dims)
{
    dnames <- sapply(dims, function(elt) paste(names(elt), collapse=" x "))
    txt <- sprintf("%s (%s)", lapply(dims, paste, collapse=" x "),
                   dnames)
    paste(names(dnames), txt, sep=": ", collapse="\n  ")
}

setMethod(show, "NetCDFFile", function(object) {
    cat("class:", class(object), "\n")
    cat(.ppath("path", object$getPath()))
    cat("dimensions:\n ", .pdims(dimensions(object)), "\n")
})
