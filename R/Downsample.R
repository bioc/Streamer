.Downsample <- setRefClass("Downsample",
    contains = "Consumer",
    fields = list(
      probability = "numeric",
      sampledSize = "integer",
      .buffer = "ANY"),
    methods = list(
    initialize = function(...)
    {
        "initialize Downsample"
        callSuper(..., .buffer=list())
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
        "sample records with 'probability' until 'sampledSize' retrieved"
        if (verbose) msg("Downsample$yield()")
        while (length(.buffer) < sampledSize &&
               0L != length(res <- callSuper()))
            .self$.buffer <- c(.buffer, .sample(res))
        idx <- seq_len(min(length(.buffer), sampledSize))
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
    },
    show = function()
    {
        callSuper()
        txt <- sprintf("probability: %.2f; sampledSize: %d",
                       probability, sampledSize)
        cat(txt, "\n")
    }))

Downsample <-
    function(probability = 0.1, sampledSize = 1e6, ...)
{
    sampledSize <- as.integer(sampledSize)
    .Downsample$new(probability=probability, sampledSize=sampledSize,
                    ...)
}
