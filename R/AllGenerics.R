setGeneric("stream",
    function(x, ..., verbose=FALSE) standardGeneric("stream"),
    signature="x")

setGeneric("reset", function(x, ...) standardGeneric("reset"))

setGeneric("yield", function(x, ...) standardGeneric("yield"))

setGeneric("status", function(x, ...) standardGeneric("status"))

setGeneric("Team", function(FUN, ..., param) standardGeneric("Team"),
           signature = "param")
