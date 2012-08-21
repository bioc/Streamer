.ConnectionProducer <- setRefClass("ConnectionProducer",
    contains = "Producer",
    fields = list(
      con = "connection", conArgs = "list",
      reader = "function", readerArgs = "list"))

.ConnectionProducer$methods(
    initialize = function(
      con, conArgs = list(),
      reader = scan, readerArgs = list(), ...)
    {
        "initialize ConnectionProducer"
        callSuper(..., conArgs = conArgs, reader=reader,
                  readerArgs=readerArgs)
        if (!missing(con)) {
            ## do not use con = file() in contructor, otherwise
            ## defining a derived class opens an unused connection
            .self$con <- con
            if (!isOpen(con))
                reset()
        }
        .self
    },
    reset = function()
    {
        "reset ConnectionProducer: reopen connection"
        if (verbose)
            msg("ConnectionProducer$reset")
        callSuper()
        if (is(con, "connection") && isOpen(con)) {
            s <- summary(con)
            cls <- s$class
            close(con)
            args <- conArgs
            args[c("description", "open")] <-
                s[c("description", "mode")]
            .self$con <- do.call(cls, args)
        } else {
            do.call(base::open, c(list(con), conArgs))
        }
        .self
    },
    yield = function()
    {
        "yield ConnectionProducer: read data from an open connection"
        if (verbose)
            msg("ConnectionProducer$reset")
        do.call(reader, c(list(con), readerArgs))
    })

close.ConnectionProducer <-
    function(con, ...)
{
    if (isOpen(con$con))
        close(con$con)
}

## 
## Scan/ReadLines/ReadTableProducer
## 


.connectionProducer <-
    function(generator, con, reader, conArgs, readerArgs, dotArgs)
{
    args <- list(con=con, conArgs=conArgs, reader=reader,
                 readerArgs=readerArgs)
    args[names(dotArgs)] <- dotArgs
    do.call(generator$new, args[!sapply(args, is.null)])
    
}

.ScanProducer <- setRefClass("ScanProducer",
    contains="ConnectionProducer")

ScanProducer <-
    function(file, ..., fileArgs=list(), scanArgs=list(...))
{
    dotArgs <- NULL
    if (!missing(scanArgs))
        dotArgs <- list(...)
    con <- if (is.character(file)) file(file) else file
    .connectionProducer(.ScanProducer, con, base::scan, fileArgs,
                        scanArgs, dotArgs)
}

.ReadLinesProducer <- setRefClass("ReadLinesProducer",
    contains = "ConnectionProducer")

ReadLinesProducer <-
    function(con, ..., conArgs=list(), readLinesArgs=list(...))
{
    dotArgs <- NULL
    if (!missing(readLinesArgs))
        dotArgs <- list(...)
    if (is.character(con))
        con <- file(con)
    .connectionProducer(.ReadLinesProducer, con, base::readLines,
                        conArgs, readLinesArgs, dotArgs)
}

.ReadTableProducer <- setRefClass("ReadTableProducer",
    fields = list(
      .template = "data.frame"),
    contains = "ConnectionProducer",
    methods = list(
      .yield_error = function(err) {
          if (conditionMessage(err) == "no lines available in input" &&
              ncol(.template) != 0L)
          {
              .template
          } else stop(err)
      },
      yield = function() {
          y <- tryCatch(callSuper(), error=.self$.yield_error)
          if (ncol(.template) == 0L)
              .self$.template <- y[FALSE,,drop=FALSE]
          y
      }))

ReadTableProducer <-
    function(file, ..., fileArgs=list(), readTableArgs=list(...))
{
    dotArgs <- NULL
    if (!missing(readTableArgs))
        dotArgs <- list(...)
    con <- if (is.character(file)) file(file) else file
    .connectionProducer(.ReadTableProducer, con, utils::read.table,
                        fileArgs, readTableArgs, dotArgs)
}
