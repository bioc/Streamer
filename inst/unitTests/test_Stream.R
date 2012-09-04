.checkStream <- function(s, yield=list())
{
    checkTrue(validObject(s))
    checkIdentical(yield, s$yield())
}

test_Stream_producer <- function()
{
    ## Stream() should produce a stream with only a producer
    p <- getRefClass("Producer")$new()
    .checkStream(Stream(p))
}

test_Stream_consumer <- function()
{
    ## Stream() should error with only a consumer (?)
    DEACTIVATED("should error with only a consumer (?)")
    c <- getRefClass("Consumer")$new()
    checkException(Stream(c))
}

test_Stream_producer_consumer <- function()
{
    ## Stream should succeed with producer / consumer in any order
    p <- getRefClass("Producer")$new()
    c <- getRefClass("Consumer")$new()
    .checkStream(s1 <- Stream(c, p))

    p <- getRefClass("Producer")$new()
    c <- getRefClass("Consumer")$new()
    .checkStream(s2 <- Stream(p, c))

    checkEquals(s1, s2)
}
