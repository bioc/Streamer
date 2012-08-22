test_ConnectionProducer <-
    function()
{
    fl <- system.file(package="Rsamtools", "extdata", "ex1.sam")

    p <- ScanProducer(file(fl, "r"), what="character", quiet=TRUE)
    checkIdentical(51431L, length(yield(p)))
    checkIdentical(character(0), yield(p))
    close(p)

    p <- ReadLinesProducer(file(fl, "r"), n = 1000)
    obs <- integer()
    while (length(y <- yield(p)))
        obs <- append(obs, length(y))
    exp <- as.integer(c(1000, 1000, 1000, 307))
    checkIdentical(exp, obs)
    close(p)

    p <- ReadTableProducer(file(fl, "r"), quote="", fill=TRUE, nrows=1000)
    obs <- integer()
    while (length(y <- yield(p)))
        obs <- append(obs, nrow(y))
    checkIdentical(c(0L, 0L), dim(yield(p)))
    exp <- as.integer(c(1000, 1000, 1000, 307))
    checkIdentical(exp, obs)
    close(p)

    ## reset
    p <- ReadTableProducer(file(fl, "r"), quote="", fill=TRUE, nrows=1000)
    exp <- yield(p)
    reset(p)
    checkIdentical(exp, yield(p))
    close(p)
}
