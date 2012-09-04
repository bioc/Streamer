setGeneric("reset", function(x, ...) standardGeneric("reset"))

setGeneric("yield", function(x, ...) standardGeneric("yield"))

setGeneric("status", function(x, ...) standardGeneric("status"))

setGeneric("Stream",
    function(x, ..., verbose=FALSE) standardGeneric("Stream"),
    signature="x")

setGeneric("DAGParam", function(x, ...) standardGeneric("DAGParam"))

setGeneric("Team", function(FUN, ..., param) standardGeneric("Team"),
           signature = "param")
