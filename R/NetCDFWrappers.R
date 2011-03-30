## FIXME: I'd put these in the same file as NetCDFProducer
## FIXME: (fixed) more compact formating
## FIXME: (fixed) stop(paste("foo", sep="")) as stop("foo")

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
    x$setSlice(value)                  # FIXME x$setSlice() returns x?
})


## Accessor methods for NetCDFFile class
setMethod("names", "NetCDFFile", function(x) x$getVariableNames())

setMethod("precision", "NetCDFFile", function(x) x$getPrecision())

setMethod("dimensionLengths", "NetCDFFile",
          function(x) x$getDimensionLengths())

setMethod("dimensions", "NetCDFFile",
          function(x) x$getDimensionNames())

setMethod("dimensionCount", "NetCDFFile",
          function(x) x$getDimensionCounts())




 

