test_MulticoreTeam_yield <- function()
{
    if (.Platform$OS.type != "unix")
        return()
    t <- Team(function(i) i, param=MulticoreParam(1L))
    s <- Stream(Seq(to=10), t)
    checkIdentical(1L, yield(s))
    checkIdentical(2L, yield(s))

    t <- Team(function(i) i, param=MulticoreParam(1L))
    s <- Stream(Seq(to=10), t)
    checkIdentical(1:10, sapply(s, c))

    t <- Team(function(x) mean(x), param=MulticoreParam(5L))
    s <- Stream(Seq(to=50, yieldSize=5), t)
    exp <- as.vector(sapply(split(1:50, rep(1:10, each=5)), mean))
    checkIdentical(exp, sapply(s, c))
}
