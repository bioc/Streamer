setGeneric("stream",
    function(x, ..., verbose=FALSE) standardGeneric("stream"),
    signature="x")

setGeneric("reset", function(x, ...) standardGeneric("reset"))

setGeneric("yield", function(x, ...) standardGeneric("yield"))

setGeneric("status", function(x, ...) standardGeneric("status"))

setGeneric("slice", function(x, ...) standardGeneric("slice"))

setGeneric("slice<-", function(x, ..., value)  standardGeneric("slice<-"))


## Generic Functions for NetCDFFile class 
setGeneric("precision", function(x, ...) standardGeneric("precision"))

setGeneric("dimensions", function(x, ...) standardGeneric("dimensions"))

setGeneric("dimensionLengths", function(x, ...)
           standardGeneric("dimensionLengths"))

setGeneric("dimensionCount", function(x, ...)
           standardGeneric("dimensionCount"))



