test_Reducer <- function()
{
    s <- stream(Seq(to=10), Reducer("+"))
    checkIdentical(sum(1:10), yield(s))
    checkIdentical(numeric(), yield(s))
    s <- stream(Seq(to=10, yieldSize=5L), Reducer("+"))
    checkIdentical(1:5 + 6:10, yield(s))
    checkIdentical(numeric(), yield(s))
    ## init
    s <- stream(Seq(to=10), Reducer("+", init=10L))
    checkIdentical(10L + sum(1:10), yield(s))
    checkIdentical(numeric(), yield(s))
    ## yieldNth
    s <- stream(Seq(to=10), Reducer("+", yieldNth=5))
    checkIdentical(c(sum(1:5), sum(6:10)), sapply(s, c))
    checkIdentical(numeric(), yield(s))
    ## reset
    s <- stream(Seq(to=10), Reducer("+", init=10L)); yield(s)
    reset(s)
    checkIdentical(10L + sum(1:10), yield(s))
    checkIdentical(numeric(), yield(s))
}
