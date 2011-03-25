## Accessor methods for NetCDFProducer class

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


## Replacement methods for NetCDFProducer class
setReplaceMethod("slice", c("NetCDFProducer", "list"),
                 function(x, ..., value) {
                     value <- unlist(value)
                     slice(x, ...) <- value
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


## Accessor methods for NetCDFFile class
setMethod("names", "NetCDFFile", function(x) {
          x$variableNames
})

setMethod("precision", "NetCDFFile", function(x) {
          x$getPrecision()
})

setMethod("dimensionLengths", "NetCDFFile", function(x) {
          x$getDimensionLengths()
})


setMethod("dimensions", "NetCDFFile", function(x) {
          x$getDimensionNames()
})


setMethod("dimensionCount", "NetCDFFile", function(x) {
          x$getDimensionCounts()
})




 

