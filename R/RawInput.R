rawReaderFactory <-
    function(blockSize=1e6, what)
{
    if (missing(what))
        what <- raw()
    function(con)
    {
        readBin(con, what, blockSize)
    }
}

rawParserFactory <-
    function(separator=charToRaw("\n"), trim=separator)
{
    if (!is(separator, "raw"))
        stop("'separator' must be 'raw()'")
    if (!identical(separator, trim))
    {
        if (!is(trim, "raw"))
            stop("'trim' must be 'raw()'")
        if (length(separator) < length(trim))
            stop("'length(separator)' must be >= length(trim)")
        trimBytes <- -seq_along(trim)
        if (!identical(separator[-trimBytes], trim))
            stop("'trim' must equal separator[seq_along(trim)]")
    }
    function(buf, bin)
    {
        .Call(.raw_parse, c(buf, bin), separator, trim,
              PACKAGE="Streamer")
    }
}

## 

.RawInput <- setRefClass("RawInput",
    contains="ConnectionProducer",
    fields = list(
      parser = "function", yieldSize = "integer",
      .buffer = "raw", .records = "list", .parsedRecords = "integer"
    ))

.RawInput$methods(
    initialize = function(...)
    {
        "initialize RawInput"
        callSuper(..., .records=list(), .parsedRecords=0L)
    },
    reset = function()
    {
        "reset RawInput"
        callSuper()
        if (verbose) msg("RawInput$reset()")
        .self$.buffer <- raw()
        .self$.records <- list()
        .self$.parsedRecords <- 0L
        .self
    },
    .add = function(input, flush=FALSE)
    {
        ".add (incomplete) 'input', possibly flush'ing buffer"
        if (verbose) msg("RawInput$.add()")
        stream <- parser(.buffer, input)
        if (flush) {
            .self$.buffer <- new(class(.buffer))
        } else {
            len <- length(stream)
            .self$.buffer <- stream[[len]]
            stream <- stream[-len]
        }
        .self$.records <- c(.records, stream)
        .self$.parsedRecords <- .parsedRecords + length(stream)
        .self
    },
    .fill = function()
    {
        "fill stream with yieldSize records, if available"
        if (verbose) msg("RawInput$.fill()")
        while (length(.records) < yieldSize &&
               0 != length(input <- reader(con)))
            .add(input)
        .self
    },        
    .flush = function()
    {
        "append remaining buffer to records"
        if (verbose) msg("RawInput$.flush()")
        if (0 != length(.buffer)) .add(raw(), TRUE)
        .self
    },
    yield = function()
    {
        "current stream, with flush if yieldSize not satisfied"
        if (verbose) msg("RawInput$yield()")
        .fill()
        if (length(.records) < yieldSize)
            .flush()
        idx <- seq_len(min(yieldSize, length(.records)))
        records <- .records[idx]
        .self$.records[idx] <- NULL
        records
    },
    status = function()
    {
        "report status of RawInput"
        if (verbose) msg("RawInput$status()")
        c(list(.parsedRecords = .parsedRecords,
               .recordLength = length(.records),
               .bufferLength = length(.buffer)),
          callSuper())
    },
    show = function()
    {
        callSuper()
        cat("file:", basename(summary(con)$description), "\n")
        s <- status()
        elts <- paste(names(s), s, sep="=", collapse=" ")
        txt <- sprintf("status: %s", elts)
        cat(strwrap(txt, exdent=2), sep="\n")
    })
    

RawInput <-
    function(con, yieldSize = 1e6, reader=rawReaderFactory(),
             parser=rawParserFactory(), ...)
{
    if (!is(con, "connection"))
        con <- file(con, "rb")
    yieldSize <- as.integer(yieldSize)
    .RawInput$new(con=con, yieldSize=yieldSize, reader=reader,
                  parser=parser, ...)
}
