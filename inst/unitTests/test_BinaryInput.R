test_recordReader <- function()
{
}

test_binary_parse_count_records <- function()
{
    f <- function(...)
        .Call(Streamer:::.binary_parse_count_records, ...)

    sep <- charToRaw("\n")

    checkIdentical(0L, f(raw(), sep))
    checkIdentical(2L, f(charToRaw("\n"), sep))
    checkIdentical(3L, f(charToRaw("\n\n"), sep))

    checkIdentical(1L, f(charToRaw("foo"), sep))
    checkIdentical(2L, f(charToRaw("foo\n"), sep))
    checkIdentical(2L, f(charToRaw("foo\nfoo"), sep))
    checkIdentical(3L, f(charToRaw("foo\nfoo\n"), sep))

    sep <- charToRaw("\n@")

    checkIdentical(0L, f(raw(), sep))
    checkIdentical(2L, f(charToRaw("\n@"), sep))
    checkIdentical(3L, f(charToRaw("\n@\n@"), sep))

    checkIdentical(1L, f(charToRaw("foo"), sep))
    checkIdentical(2L, f(charToRaw("foo\n@"), sep))
    checkIdentical(2L, f(charToRaw("foo\n@foo"), sep))
    checkIdentical(3L, f(charToRaw("foo\n@foo\n@"), sep))

    checkIdentical(1L, f(charToRaw("foo\nfoo\n"), sep))
}

test_binary_parse <- function()
{
    f <- function(...) .Call(Streamer:::.binary_parse, ...)

    sep <- charToRaw("\n")
    trim <- sep

    checkIdentical(list(), f(raw(), sep, trim))
    checkIdentical(list(raw(), raw()),
                   f(charToRaw("\n"), sep, trim))
    checkIdentical(list(charToRaw("foo")),
                   f(charToRaw("foo"), sep, trim))
    checkIdentical(list(charToRaw("foo"), raw()),
                   f(charToRaw("foo\n"), sep, trim))
    checkIdentical(list(raw(), charToRaw("foo")),
                   f(charToRaw("\nfoo"), sep, trim))
    checkIdentical(list(charToRaw("foo"), charToRaw("bar")),
                   f(charToRaw("foo\nbar"), sep, trim))
    checkIdentical(list(charToRaw("foo"), charToRaw("bar"), raw()),
                   f(charToRaw("foo\nbar\n"), sep, trim))

    checkIdentical(list(raw(0), charToRaw("\n")),
                   f(charToRaw("\n"), sep, raw()))

    sep <- charToRaw("\n@")
    trim <- charToRaw("\n")

    checkIdentical(list(), f(raw(), sep, trim))
    checkIdentical(list(raw(), charToRaw("@")),
                   f(charToRaw("\n@"), sep, trim))
    checkIdentical(list(charToRaw("@foo")),
                   f(charToRaw("@foo"), sep, trim))
    checkIdentical(list(charToRaw("@foo"), charToRaw("@")),
                   f(charToRaw("@foo\n@"), sep, trim))
    checkIdentical(list(raw(), charToRaw("@foo")),
                   f(charToRaw("\n@foo"), sep, trim))
    checkIdentical(list(charToRaw("foo"), charToRaw("@bar")),
                   f(charToRaw("foo\n@bar"), sep, trim))
    checkIdentical(list(charToRaw("@foo"), charToRaw("@bar"),
                        charToRaw("@")),
                   f(charToRaw("@foo\n@bar\n@"), sep, trim))
}

test_binaryParserFactory <- function()
{
    checkException(binaryParserFactory("\n"),
                   "'separator' must be 'raw()'", TRUE)
    checkException(binaryParserFactory(trim="\n"),
                   "'trim' must be 'raw()'", TRUE)
    checkException(binaryParserFactory(charToRaw("\n@"),
                                       charToRaw("@")),
                   "'trim' must equal separator[seq_along(trim)]",
                   TRUE)
    checkException(binaryParserFactory(charToRaw("\n@"),
                                       charToRaw("\n@x")),
                   "'length(separator)' must be >= length(trim)",
                   TRUE)
}

test_binaryParser <- function()
{
    p <- binaryParserFactory()
    checkIdentical(list(), p(raw(), raw()))

    foo <- charToRaw("foo")
    checkIdentical(list(foo), p(foo, raw()))
    checkIdentical(list(foo), p(raw(), foo))
    checkIdentical(list(charToRaw("foofoo")), p(foo, foo))

    foo_n <- charToRaw("foo\n")
    checkIdentical(list(foo, raw()), p(foo_n, raw()))
    checkIdentical(list(foo, raw()), p(raw(), foo_n))
    checkIdentical(list(foo, foo, raw()), p(foo_n, foo_n))
    checkIdentical(list(foo, foo), p(foo_n, foo))
}

test_BinaryInput <- function()
{
    fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")

    ## default yield
    s <- BinaryInput(fl)
    y <- yield(s)
    checkIdentical(1024L,length(y))
    checkIdentical(list(), yield(s))    # no more records

    ## reset
    reset(s)
    checkIdentical(0L, s$.parsedRecords)
    y1 <- yield(s)
    checkIdentical(y, y1)
    checkIdentical(list(), yield(s))

    ## yield w/ sep & trim arguments -- fastq records
    parser <- binaryParserFactory(charToRaw("\n@"), charToRaw("\n"))
    s <- BinaryInput(fl, parser=parser)
    y2 <- yield(s)
    checkIdentical(256L, length(y2))
    checkIdentical(sapply(y1, rawToChar),
                   unlist(strsplit(sapply(y2, rawToChar), "\n")))
}
