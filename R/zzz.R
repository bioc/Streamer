.mc_parallel <- .mc_collect <- .mc_sendMaster <- .mc_kill <-
    function(...) {}

.onLoad <-
    function(libname, pkgname)
{
    if (.Platform$OS.type == "unix") {
        ## conditional export in Windows
        .mc_parallel <<- parallel::mcparallel
        .mc_collect <<- parallel::mccollect
        ## non-exported
        .mc_kill <<- parallel:::mckill
        .mc_sendMaster <<- parallel:::sendMaster
    }
}

.ppath <-
    function(tag, filepath)
{
    wd <- options('width')[[1]] - nchar(tag) - 6
    if (0L == length(filepath) || nchar(filepath) < wd)
        return(sprintf("%s: %s\n", tag, filepath))
    bname <- basename(filepath)
    wd1 <- wd - nchar(bname)
    dname <- substr(dirname(filepath), 1, wd1)
    sprintf("%s: %s...%s%s\n",
            tag, dname, .Platform$file.sep, bname)
}
