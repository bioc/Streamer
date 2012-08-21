.YConnector <- setRefClass("YConnector",
    contains = "Consumer",
    fields = list(.upstream="list", .fun="function"
    ))
    
.YConnector$methods(
    initialize = function(..., fun) 
    {
        "initialize YConnector"
        if(missing(fun))
            fun <- function(...) as.list(...)
        callSuper(..., .upstream=list(), .fun=fun)
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
        upstream <- paste(sapply(.upstream, "class"), collapse = " ,")
        upstream[!nzchar(upstream)] <- "uninitialized field"
        txt <- sprintf("upstream: %s", upstream)
        cat(strwrap(txt, exdent=2), sep="\n")
    })

YConnector <- function(fun, ...)
    .YConnector$new(fun=fun, ...)
