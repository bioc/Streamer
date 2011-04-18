.ParallelConnector <-
    setRefClass("ParallelConnector", contains="Consumer",
                fields=list(upstream="ANY"))

.ParallelConnector$methods(
    initialize = function(streamer, ...)
    {
        "Initialize the fields of the ParallelConnector class"
         callSuper(...)
         if (verbose) msg(".ParallelConnector$initialize()")   
        .self$inputPipe <- streamer
        .self$upstream <- parallel(quote({
            while(TRUE) {
                prime <- yield(streamer)
                sendMaster(prime)
        }}))
        .self
    },
    yield = function() 
    {
        "Read data from childProcess"
        if (verbose) msg(".ParallelConnector$yield()")   
        res <- readChild(upstream)
        if(is.raw(res)) unserialize(res) else res 
    },
    finalize = function() 
    {
        "Close threads started by ParallelConnector"
        if (verbose) msg(".ParallelConnector$finalize()")
        kill(children(upstream), SIGTERM)
        collect(children(upstream))
    })

ParallelConnector <- function(streamer, ..., verbose = FALSE) 
{
    .ParallelConnector$new(streamer = streamer, ..., verbose = verbose)

}

