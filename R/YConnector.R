
YConnector <- function(upstream, fun, ...,  yieldSize =1e6, verbose = FALSE) 
{
    new("YConnector", upstream=upstream, fun=fun, ..., yieldSize=yieldSize,
        verbose=verbose)
}

.YConnector <- setRefClass("YConnector",
    contains = "Consumer",
    fields = list(
        .upstream ="list", .fun="function"
    ))
    
.YConnector$methods(
    initialize = function(upstream, fun, ...) 
    {   "initialize YConnector"
        callSuper(...)
        if(verbose) msg(".YConnector$initialize")
        if (length(upstream) != length(formals(fun))) {
            msg <- "Number of arguments of fun not equal to number of inputs of 
                    YConnector"
            stop(msg)
        }
        .self$.upstream <- upstream
        .self$.fun <- fun
        len <- length(upstream)
        .self$inUse <- rep(FALSE, len) 
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

