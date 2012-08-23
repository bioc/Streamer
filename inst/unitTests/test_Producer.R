test_lapply_Producer <- function()
{
    vals <- split(1:47, rep(1:7, each=7, length.out=47))
    exp <- unname(lapply(vals, mean))

    ## function
    obs <- lapply(Seq(to=47, length.out=7), mean)
    checkIdentical(exp, obs)

    ## anonymous function
    obs <- lapply(Seq(to=47, length.out=7), function(x) mean(x))
    checkIdentical(exp, obs)

    ## ... args
    obs <- lapply(Seq(to=47, length.out=7), function(x, z) mean(z), z=1:10)
    checkIdentical(mean(1:10), unique(unlist(obs)))

    ## env
    ZZZ <- 1:10
    res <- lapply(Seq(to=47, length.out=7), function(x) mean(ZZZ))
    checkIdentical(mean(ZZZ), unique(unlist(res)))

    ## error
    fun <- function(x) if (x == 3) stop("x: ", x) else x
    checkException(lapply(Seq(to=5), fun), silent=TRUE)

    ## error partial results
    res <- tryCatch(sapply(Seq(to=5), fun), error = function(err) {
        err$partialResult
    })
    checkIdentical(c(1, 2), res)

    ## trigger re-allocation
    ## res <- lapply(Seq(to=4096*4),
    ##               function(x) { if (x %% 1000 == 0) message(x); x })
}
