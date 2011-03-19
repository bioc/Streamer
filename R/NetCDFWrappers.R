## Accessor methods 
setMethod("variableNames", "NetCDFProducer", function(x) {
    x$variableNames
})

setMethod("sliceDimensions", "NetCDFProducer", function(x, var) {
    x$getSliceDimensions(var)
})

setMethod("status", "NetCDFProducer", function(x, var) {
    x$status(var)

})
setMethod("reset", "NetCDFProducer", function(x, var,...) {
    x$reset(var,...)
})

setMethod("yield", "NetCDFProducer", function(x, var,...) {
    x$yield(var,...)
})


## Replacement methods 
setReplaceMethod("sliceDimensions", c("NetCDFProducer", "character", "list"),
                 function(x, var, value) {
     if(length(var) != 1)
         stop("A single variable has to be specified")
     if( ! var %in% variableNames(x))
         stop( paste(var, "was not found in the variables in the NetCDF file",
                      sep = " "))
     nms <- names(value)
     if( !all( nms %in% names(sliceDimensions(x, var))))
         stop(paste("Dimension names specified do not match those for the variable",
              var, sep = " "))
     x$setSliceDimensions(var, unlist(value))
     x
})

        

