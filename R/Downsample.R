.Downsample <- setRefClass("Downsample",
    contains = "Consumer",
    fields = list(
      probability = "numeric",
      .buffer = "ANY"),
    methods = list(
    initialize = function(..., probability)
    {
        "initialize Downsample"
        callSuper(...)
        if (verbose) msg("Downsample$initialize")
        .self$probability <- probability
        .self$.buffer <- list()
        .self
    },
    .sample = function(x)
    {
        if (verbose) msg("Downsample$.sample()")
        len <- length(x)
        n <- rbinom(1L, len, probability)
        x[sort.int(base::sample(len, n))]
    },
    yield = function()
    {
        "sample records with 'probability' until 'yieldSize' retrieved"
        if (verbose) msg("Downsample$yield()")
        while (length(.buffer) < yieldSize &&
               0L != length(res <- callSuper()))
            .self$.buffer <- c(.buffer, .sample(res))
        idx <- seq_len(min(length(.buffer), yieldSize))
        result <- .buffer[idx]
        .self$.buffer <- .buffer[-idx]
        result
    },
    reset = function()
    {
        "reset .buffer to empty list"
        if (verbose) msg("Downsample$reset()")
        callSuper()
        .self$.buffer <- list()
        .self
    },
    status = function()
    {
        "report status of 'Downsample'"
        if (verbose) msg("Downsample$status()")
        c(list(probability=probability), callSuper())
    }))

Downsample <-
    function(probability = 0.1, ..., yieldSize = 1e6,
             verbose=FALSE)
{
    .Downsample$new(probability=probability, ..f,
                    yieldSize=yieldSize, verbose=verbose)
}
    
setMethod(show, "Downsample",
    function(object)
{
    callNextMethod()
    txt <- sprintf("probability: %.2f; yieldSize: %d",
                   object$probability, object$yieldSize)
    cat(txt, "\n")
})
