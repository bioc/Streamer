setGeneric("stream",
    function(x, ..., verbose=FALSE) standardGeneric("stream"),
    signature="x")

setGeneric("reset", function(x, ...) standardGeneric("reset"))

setGeneric("yield", function(x, ...) standardGeneric("yield"))

setGeneric("variableNames", function(x, ...) standardGeneric("variableNames"))
setGeneric("status", function(x, ...) standardGeneric("status"))

setGeneric("sliceDimensions", function(x, ...) standardGeneric("sliceDimensions"))
setGeneric("sliceDimensions<-", function(x, var, value)  standardGeneric("sliceDimensions<-"))

setGeneric("dimensionLengths", function(x, ...)
           standardGeneric("dimensionLengths"))

