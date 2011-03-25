## Accessor methods 
## Accessor methods 
setMethod("names", "NetCDFProducer", function(x) {
    x$name
})

setMethod("slice", "NetCDFProducer", function(x) {
    x$slice
})

setMethod("yield", "NetCDFProducer", function(x) {
    x$yield()
})

setMethod("reset", "NetCDFProducer", function(x) {
    x$reset()
})

setMethod("status", "NetCDFProducer", function(x) {
    x$status()
})


## Replacement methods 
setReplaceMethod("slice", c("NetCDFProducer", "list"),
                 function(x, ..., value) {
                     value <- unlist(value)
                     callNextMethod(...)
                     x
                 })

setReplaceMethod("slice", c("NetCDFProducer", "numeric"),
                 function(x, ..., value) {
                     nms <- names(value)
                     if( !all( nms %in% names(slice(x))))
                         stop(paste("Dimension names specified do not match those for the variable", sep = " "))
                     x$setSlice(value)
                     x
                 })


### Wrapper methods for NetCDFFile class
setMethod("names", "NetCDFFile", function(x) {
          x$variableNames
})

setMethod("precision", "NetCDFFile", function(x, var) {
          .checkVar(x, var = var)
          x$getPrecision(var = var)
})

setMethod("dimensionLengths", "NetCDFFile", function(x, var, ...) {
          .checkVar(x, var = var)
          x$getDimensionLengths(var = var,...)
})


setMethod("dimensions", "NetCDFFile", function(x, var,...) {
          .checkVar(x, var)
          x$getDimensionNames(var = var,...)
})


setMethod("dimensionCount", "NetCDFFile", function(x, var,...) {
          .checkVar(x, var)
          x$getDimensionCounts(var = var,...)
})




 

