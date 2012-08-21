readLinesReaderFactory <-
    function(blockSize=1e6, ...)
{
    if ("n" %in% names(list(...)))
        stop("use 'blockSize' instead of 'n' to specify block size")
    function(con, blockSize)
    {
        readLines(con, blockSize, ...)
    }
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

concatenationParserFactory <-
    function()
{
    function(buf, bin)
    {
        c(buf, bin)
    }
}

readLinesParserFactory <- concatenationParserFactory

scanParserFactory <- concatenationParserFactory

## ReadLinesInput

.ReadLinesInput <- setRefClass("ReadLinesInput",
    contains = "ConnectionProducer",
    fields = list(
      yieldSize = "integer",
      .records = "character"))

.ReadLinesInput$methods(
    reset = function()
    {
        "reset ReadLinesInput"
        if (verbose) msg("ReadLinesInput$reset()")
        callSuper()
        .self$.records <- character()
        .self$.parsedRecords <- 0L
        .self
    },
    .add=function(input)
    {
        .self$.records <- .self$parser(.self$.records, input)
        .self
    },
    .fill=function()
    {
        while(length(.records) < yieldSize && 
              0 !=length(input <- .self$reader(con, yieldSize)))
            .add(input)
        .self
    },
    yield = function()
    {
        "current stream, with flush if yieldSize not satisfied"
        if (verbose) msg("ReadLinesInput$yield()")
        .fill()
        idx <- seq_len(min(yieldSize, length(.self$.records)))
        records <- .records[idx]
        .self$.records <- .self$.records[-idx]
        records
    },
    status = function()
    {
        "report status of ReadLinesInput"
        if (verbose) msg("ReadLinesInput$status()")
        c(list(.recordLength = length(.records)),
          callSuper())
    }
)

ReadLinesInput <- 
    function(con, reader=readLinesReaderFactory(),
             parser=readLinesParserFactory(), 
             yieldSize=1e6, ...)
{
    if (!is(con, "connection"))
        con <- file(con, "r")
    yieldSize <- as.integer(yieldSize)
    .ReadLinesInput$new(con=con, reader=reader, parser=parser,
                        yieldSize=yieldSize, ...)
}



