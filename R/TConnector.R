.TOut <-
    setRefClass("TOut", contains="Consumer",
        fields=list(
            .start="numeric"
                ))

.TOut$methods(
    initialize =function(...) 
    {  
       "initialize TOut"
        callSuper(...)
        if (verbose) msg(".TOut$initialize()")
        .self$.start <- 1L
        .self
    },
    yield = function() 
    {  "Calls yield on the inputPipe"
       if (verbose) msg(".TOut$yield()")
       while(length(.self$inputPipe$.records) - .self$.start < yieldSize &&
              0 != length(input <- .self$inputPipe$.fill()) )
         .self$inputPipe$.add(input)
       if( .self$.start + yieldSize  <= length(.self$inputPipe$.records))
            width <-  .self$.start + yieldSize -1
       else
            width <- length(.self$inputPipe$.records) - .self$.start +1
       if(width>0)
            idx <- seq(.self$.start, .self$.start + width -1)
       else  idx <- 0
       .self$.start <- .self$.start + width
       dat <- .self$inputPipe$.records[idx]
       .self$inputPipe$.dump()
       dat
    })

TOut <- function( ..., yieldSize=1e6, verbose=FALSE) 
{
    .TOut$new(..., yieldSize=yieldSize, verbose=verbose)

}

.TConnector <- 
    setRefClass("TConnector", contains = "Consumer",
                fields = list(
                    .records = "list",
                    .tOuts="list"
                    ))
    
.TConnector$methods(
    initialize = function(...) 
    {   
        "initialize TConnector"
        callSuper(...)
        if (verbose) msg("TConnector$initialize()")
        .self$.tOuts <- list()
        .self$inUse <- FALSE
        .self
    },   
    .fill = function() 
    {   "Fills the stream with yieldSize records"
        if (verbose) msg("TConnector$.fill")
        .self$inputPipe$yield()
    },
    .add = function(input)
    { 
        ".add (incomplete) 'input'"
        if (verbose) msg("TConnector$.add()")
        .self$.records <- c(.records, input)
        .self    
    },
    .dump = function() 
    {    
        "Clear .records that are used"
        if (verbose) msg("TConnector$.dump()")
        mn <- min(sapply(.self$.tOuts, function(x) {
                    x$.start
               }))
        if(mn !=1) {
            len <- length(.self$.tOuts)
            for(i in 1:len) 
            {   
              .self$.tOuts[[i]]$.start <-  .self$.tOuts[[i]]$.start - mn +1

            }
            .self$.records[seq_len(mn-1)] <- NULL
        }
        .self
   })

                   

TConnector <- function(..., yieldSize=1e6, verbose=FALSE) {
    .TConnector$new(..., yieldSize=yieldSize, verbose=verbose)
}


