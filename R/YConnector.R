.YConnector <- setRefClass("YConnector",
    contains = "Consumer",
    fields = list(.upstream="list", .fun="function"
    ))
    
.YConnector$methods(
    initialize = function(..., fun) 
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
    },
    show = function()
    {
        callSuper()
        upstream <- paste(lapply(.upstream, "class"), collapse = " ,")
        upstream[!nzchar(upstream)] <- "uninitialized field"
        txt <- sprintf("upstream: %s", upstream)
        cat(strwrap(txt, exdent=2), sep="\n")
    })

YConnector <- function(fun, ...,  yieldSize =1e6, verbose = FALSE) 
{
    .YConnector$new(fun=fun, ..., yieldSize=yieldSize, verbose=verbose)
}
