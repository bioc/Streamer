.YConnector <- setRefClass("YConnector",
    contains = "Consumer",
    fields = list(.upstream="list", .fun="function"
    ))
    
.YConnector$methods(
    initialize = function(fun, ...) 
    {   "initialize YConnector"
        callSuper(...)
        if(verbose) msg(".YConnector$initialize")
        .self$.upstream <- list()
        if(missing(fun))
            .self$.fun <- function(){}
        else .self$.fun <- fun
        .self
    },
    yield = function() 
    {
        "yield data from stream"
        args <- lapply(.self$.upstream, function(k) {
                       k$yield()
                   })
        do.call(.self$.fun, args)
    })

YConnector <- function(fun, ...,  yieldSize =1e6, verbose = FALSE) 
{
    new("YConnector", fun=fun, ..., yieldSize=yieldSize, verbose=verbose)
}


