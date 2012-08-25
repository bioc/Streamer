## TOut

.TOut <- setRefClass("TOut", 
    contains="Consumer",
    fields=list(.start = "numeric"))

.TOut$methods(
    initialize =function(...) 
    {  
       "initialize TOut"
        callSuper(..., .start = 1L)
    },
    yield = function() 
    {
        "Calls yield on the inputPipe"
        if (verbose) msg(".TOut$yield()")
        inputPipe$.add(inputPipe$yield())
        dat <- inputPipe$.records
        inputPipe$.dump()
        dat
    })

TOut <- function(...) .TOut$new(...)

## TConnector

.TConnector <- setRefClass("TConnector", 
    contains = "Consumer",
    fields = list(
        .records = "list",
        .tOuts="list"
    ))
    
.TConnector$methods(
    initialize = function(...) 
    {   
        "initialize TConnector"
        callSuper(..., .tOuts = list())
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
        mn <- min(sapply(.tOuts, function(x) x$.start))
        if (mn != 1L) {
            for(i in seq_along(.tOuts))
                .self$.tOuts[[i]]$.start <- .tOuts[[i]]$.start - mn +1L
            .self$.records[seq_len(mn - 1L)] <- NULL
        }
        .self
   })

TConnector <- function(...) .TConnector$new(...)


