.ParallelConnector <-
    setRefClass("ParallelConnector", contains="Consumer",
                fields=list(.upstream="ANY"))

.ParallelConnector$methods(
    initialize = function(...)
    {
        "Initialize the fields of the ParallelConnector class"
        callSuper(...)
        if (verbose) msg(".ParallelConnector$initialize()")   
        .self
    },
    yield = function() 
   {  
        "Read data from childProcess"
        if (verbose) msg(".ParallelConnector$yield()")   
        if(is(.self$inputPipe, "uninitializedField")
           || is(.self$.upstream, "uninitializedField")) 
        {
            stop("ParallelConnector not connected to a valid stream")
        
        } else {
        
            res <- readChild(.self$.upstream)
            if(is.raw(res)) unserialize(res) else res 
        
        }
    },
    finalize = function() 
    {   
        "Close threads started by ParallelConnector"
        if (verbose) msg(".ParallelConnector$finalize()")
        kill(children(.upstream), SIGTERM)
        collect(children(.upstream))
    })

ParallelConnector <- function(..., yieldSize=1e6, verbose = FALSE) 
{
    .ParallelConnector$new(..., yieldSize=yieldSize, verbose = verbose)
}

