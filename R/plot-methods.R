plot.DAGParam <-
    function(x, y, ...)
{
    require(Rgraphviz)
    plot(x$dag, ...)
}

plot.DAGTeam <-
    function(x, y, ...)
{
    plot(x$dagParam, ...)
}
