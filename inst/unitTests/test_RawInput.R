test_raw_parse_count_records <- function()
{
    f <- function(...)
        .Call(Streamer:::.raw_parse_count_records, ...)

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

test_raw_parse <- function()
{
    f <- function(...) .Call(Streamer:::.raw_parse, ...)

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

test_rawParserFactory <- function()
{
    checkException(rawParserFactory("\n"),
                   "'separator' must be 'raw()'", TRUE)
    checkException(rawParserFactory(trim="\n"),
                   "'trim' must be 'raw()'", TRUE)
    checkException(rawParserFactory(charToRaw("\n@"),
                                       charToRaw("@")),
                   "'trim' must equal separator[seq_along(trim)]",
                   TRUE)
    checkException(rawParserFactory(charToRaw("\n@"),
                                       charToRaw("\n@x")),
                   "'length(separator)' must be >= length(trim)",
                   TRUE)
}

test_rawParser <- function()
{
    p <- rawParserFactory()
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

test_RawInput <- function()
{
    fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")

    ## default yield
    s <- RawInput(fl)
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
    parser <- rawParserFactory(charToRaw("\n@"), charToRaw("\n"))
    s <- RawInput(fl, parser=parser)
    y2 <- yield(s)
    checkIdentical(256L, length(y2))
    checkIdentical(sapply(y1, rawToChar),
                   unlist(strsplit(sapply(y2, rawToChar), "\n")))
}
