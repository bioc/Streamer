.RevStream <- setRefClass("RevStream",
    contains = "Consumer",
    fields = list(
        buffer = "character", # (temporary) file path to reversed records
        con = "connection",
        refhook = "function",
        .nRecords = "integer",
        .reversed = "logical"))

.RevStream$methods(
    initialize = function(..., refhook)
    {
        "initialize RevStream"
        callSuper(...)
        if (.self$verbose)
            .self$msg(".RevStream$initialize")
        .self$refhook <- 
            if (missing(refhook)) function(x)
            {
                txt <- sprintf("unserialized component, class '%s'",
                               class(x))
                if (.self$verbose)
                    msg(txt)
                NULL
            } else refhook
        .self$.nRecords <- 0L
        .self$.reversed <- FALSE
        .self
    },
    ## reset = function(...)
    ## {
    ##     .self$iRecords <- 0L
    ##     if (complete)
    ##         open(.self$con)
    ##     else {
    ##         warning("'reset' called on an incomplete stream")
    ##         open(.self$con)
    ##     }
    ##     .self
    ## },
    finalize = function()
    {
        "finalize RevStream"
        if (verbose) msg("RevStream$finalize")
        if (is(con, "connection") && isOpen(con))
            close(con)
    },
    .reverse = function(records, revcon)
    {
        for (i in seq_along(records))
            serialize(records[[i]], revcon)
        .self$.nRecords <- .nRecords + length(records)
    },
    .yield = function()
    {
        i <- yieldSize
    },
    yield = function()
    {
        "yield reversed content, creating reverse if necessary"
        if (!.reversed)
        {
            .self$buffer <- tempfile()
            revcon <- file(buffer, "wb")
            on.exit(close(revcon))
            while (0L != length(records <- callSuper()))
                .reverse(records, revcon)
            ## reset buffer
            .self$con <- file(buffer, "rb")
            .self$.reversed <- TRUE
        }
        n <- min(.self$.nRecords, .self$yieldSize)
        result <- vector("list", n)
        for (i in seq_len(n))
            result[[i]] <- unserialize(con)
        .self$.nRecords <- .nRecords - n
        result
    })

RevStream <-
    function(..., yieldSize = 1e6)
{
    .RevStream$new(..., yieldSize=yieldSize)
}
