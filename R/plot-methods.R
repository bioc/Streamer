plot.DAGParam <-
    function(x, y, ...)
{
    Rgraphviz::plot(x$dag, ...)
}

plot.DAGTeam <-
    function(x, y, ...)
{
    plot(x$dagParam, ...)
}
