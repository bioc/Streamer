setMethod("Team", c(param="missing"),
    function(FUN, ..., param)
{
    param <- parallelRegister()$param
    if (!param$inUse)
        if (.Platform$OS.type == "unix")
            param <- MulticoreParam()
        else
            stop("'Team' not supported on '",
                 .Platform$OS.type, "' operating system")
    Team(FUN, ..., param=param)
})

setMethod("Team", c(param="MulticoreParam"),
    function(FUN, ..., param)
{
    if (.Platform$OS.type != "unix")
        stop("'Team' with 'param=MulticoreParam()' not supported on '",
             .Platform$OS.type, "' operating system")
    require(parallel)
    tasks <- replicate(param$size, list(status="IDLE",
                       name=NA_character_), simplify=FALSE)
    .MulticoreTeam$new(FUN=FUN, tasks=tasks, ..., parallelParam=param)
})
