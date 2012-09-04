test_DAGTeam_serial <- function()
{
    df <- data.frame(From = c("A", "A", "B", "C"),
                     To   = c("B", "C", "D", "D"),
                     stringsAsFactors=FALSE)
    dagParam <- DAGParam(df)
    dteam <- DAGTeam(A=FunctionConsumer(function(y) y),
                     B=FunctionConsumer(function(A) -A),
                     C=FunctionConsumer(function(A) 1 / A),
                     D=FunctionConsumer(function(B, C) B + C),
                     dagParam=dagParam)
    strm <- Stream(Seq(to=10), dteam)
    
    a <- 1:10
    checkIdentical(-a + 1 / a, sapply(strm, c))
    checkIdentical(numeric(0), yield(strm))
    reset(strm)
    checkIdentical(-a + 1 / a, sapply(strm, c))
}
