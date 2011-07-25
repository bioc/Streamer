readLinesReaderFactory <-
    function(blockSize=1e6, ...)
{
    if ("n" %in% names(list(...)))
        stop("use 'blockSize' instead of 'n' to specify block size")
    function(con)
    {
        readLines(con, blockSize, ...)
    }
}

concatenationParserFactory <-
    function()
{
    function(buf, bin)
    {
        c(buf, bin)
    }
}

readLinesParserFactory <- concatenationParserFactory

## ReadLinesInput

.ReadLinesInput <- setRefClass("ReadLinesInput",
    contains = "ConnectionProducer",
    fields = list(
      .buffer = "character", .records = "character", .parsedRecords = "integer"
    ))

.ReadLinesInput$methods(
    initialize = function(...)
    {
        "initialize 'ReadLinesInput'"
        callSuper(...)
        if (verbose) msg("ReadLinesInput$initialize()")
        .self$.parsedRecords <- 0L
        .self
    },
    reset = function()
    {
        "reset ReadLinesInput"
        callSuper()
        if (verbose) msg("ReadLinesInput$reset()")
        .self$.buffer <- raw()
        .self$.records <- character()
        .self$.parsedRecords <- 0L
        .self
    },
    yield = function()
    {
        "current stream, with flush if yieldSize not satisfied"
        if (verbose) msg("ReadLinesInput$yield()")
    },
    status = function()
    {
        "report status of ReadLinesInput"
        if (verbose) msg("ReadLinesInput$status()")
        c(list(.parsedRecords = .parsedRecords,
               .recordLength = length(.records),
               .bufferLength = length(.buffer)),
          callSuper())
    }
)

ReadLinesInput <- 
    function(con, reader=readLinesReaderFactory(), ...,
             yieldSize=1e6, verbose=FALSE)
    {
        if (!is(con, "connection"))
            con <- file(con, "r")
        .ReadLinesInput$new(con=con, 
                      reader=reader, ...,
                      yieldSize=yieldSize, verbose=verbose)
    }


## ScanInput

scanReaderFactory <-
    function(blockSize=1e6, ...)
{
    if ("nmax" %in% names(list(...)))
        stop("use 'blockSize' instead of 'nmax' to specify block size")
    function(con)
    {
        scan(con, nmax=blockSize, ...)
    }
}

scanParserFactory <- concatenationParserFactory
