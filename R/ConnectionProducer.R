.ConnectionProducer <- setRefClass("ConnectionProducer",
    contains = "Producer",
    fields = list(
        con = "connection",
        reader = "function", parser = "function"
    ))
.ConnectionProducer$methods(
    initialize = function(con,
        reader = rawReaderFactory(),
        parser = rawParserFactory(), ...)
    {
        "initialize ConnectionProducer"
        callSuper(...)
        if (verbose) msg(".ConnectionProducer$initialize")
        if (!missing(con))
            .self$con <- con
        .self$reader <- reader
        .self$parser <- parser
        .self
    },
    finalize = function()
    {
        "finalize ConnectionProducer"
        if (verbose) msg("ConnectionProducer$finalize")
        ## Commented out as the finalize method ends up getting called during gc
        ## and not when the object is destroyed as documented
       # if (is(con, "connection") && isOpen(con))
        #    close(con)
    },
    reset = function()
    {
        "reset ConnectionProducer: reopen connection"
        callSuper()
        if (is(con, "connection") && isOpen(con)) {
            if (verbose) msg("ConnectionProducer$reset re-open")
            s <- summary(con)
            class <- s$class
            desc <- s$description
            close(con)
            .self$con <- do.call(s$class, list(desc, "rb"))
        } else {
            open(con, "rb")
        }
        .self
    })
