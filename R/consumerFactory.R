consumerFactory <- function(name, fun) 
{
.name <- paste(".", name, sep ="")
classDefinition <- 
    sprintf("%s <-  setRefClass(\"%s\",
        contains = \"Consumer\",
        fields = list(
            .buffer = \"ANY\",
            .fun= \"function\")) \n",
         .name, name)

classMethods <- 
    sprintf("%s$methods(
        initialize = function(...)
        {
            \"initialize %s\"
            callSuper(...)
            if (.self$verbose)
                .self$msg(\"%s$initialize\")
            .self$.buffer <- NULL
            .self$.fun <- fun
            .self
        },
        finalize = function()
        {
            \"finalize %s\"
            if (verbose) msg(\"%s$finalize\")
        },
        yield = function()
        {
            \"yield data from %s\"
            if (verbose) msg(\"%s$yield\")
            .self$.buffer <- callSuper()
            .self$.fun(.self$.buffer)
        })\n",.name, name, name, name, name, name, name)

classConstructor <-
    sprintf("%s <- function(..., yieldSize = 1e6, verbose = FALSE)
        {
            %s$new(..., yieldSize=yieldSize, verbose=verbose)
        } \n", name, .name)

file <- paste(name, "-class.R", sep="")
cat(c(classDefinition, "\n", classMethods,"\n", classConstructor), file = file)
}  

