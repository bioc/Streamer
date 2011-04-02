setGeneric("stream",
    function(x, ..., verbose=FALSE) standardGeneric("stream"),
    signature="x")

setGeneric("reset", function(x, ...) standardGeneric("reset"))

setGeneric("yield", function(x, ...) standardGeneric("yield"))

setGeneric("status", function(x, ...) standardGeneric("status"))

## Generic Functions for NetCDFFile class 

setGeneric("precision", function(x, ...) standardGeneric("precision"))

setGeneric("dimensions", function(x, ...) standardGeneric("dimensions"))
