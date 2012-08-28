test_MulticoreTeam_yield <- function()
{
    if (.Platform$OS.type != "unix")
        return()
    t <- Team(function(i) i, param=MulticoreParam(1L))
    s <- stream(Seq(to=10), t)
    checkIdentical(1L, yield(s))
    checkIdentical(2L, yield(s))

    t <- Team(function(i) i, param=MulticoreParam(1L))
    s <- stream(Seq(to=10), t)
    checkIdentical(1:10, sapply(s, c))
}
