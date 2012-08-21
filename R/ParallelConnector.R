.ParallelConnector <-
    setRefClass("ParallelConnector", contains="Consumer",
                fields=list(.upstream="ANY"))

.ParallelConnector$methods(
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
    })

ParallelConnector <- function(...)
    .ParallelConnector$new(...)

