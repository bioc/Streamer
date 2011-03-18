
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


