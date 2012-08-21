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
        callSuper(..., reader=reader, parser=parser)
        if (!missing(con))
            .self$con <- con
        .self
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
