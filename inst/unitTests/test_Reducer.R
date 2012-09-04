test_Reducer <- function()
{
    s <- Stream(Seq(to=10), Reducer("+"))
    checkIdentical(sum(1:10), yield(s))
    checkIdentical(numeric(), yield(s))
    s <- Stream(Seq(to=10, yieldSize=5L), Reducer("+"))
    checkIdentical(1:5 + 6:10, yield(s))
    checkIdentical(numeric(), yield(s))
    ## init
    s <- Stream(Seq(to=10), Reducer("+", init=10L))
    checkIdentical(10L + sum(1:10), yield(s))
    checkIdentical(numeric(), yield(s))
    ## yieldNth
    s <- Stream(Seq(to=10), Reducer("+", yieldNth=5))
    checkIdentical(c(sum(1:5), sum(6:10)), sapply(s, c))
    checkIdentical(numeric(), yield(s))
    ## reset
    s <- Stream(Seq(to=10), Reducer("+", init=10L)); yield(s)
    reset(s)
    checkIdentical(10L + sum(1:10), yield(s))
    checkIdentical(numeric(), yield(s))
}
