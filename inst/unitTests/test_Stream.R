.checkStream <- function(s, yield=list())
{
    checkTrue(validObject(s))
    checkIdentical(yield, s$yield())
}

test_Stream_producer <- function()
{
    ## stream() should produce a stream with only a producer
    p <- getRefClass("Producer")$new()
    .checkStream(stream(p))
}

test_Stream_consumer <- function()
{
    ## stream() should error with only a consumer (?)
    DEACTIVATED("should error with only a consumer (?)")
    c <- getRefClass("Consumer")$new()
    checkException(stream(c))
}

test_Stream_producer_consumer <- function()
{
    ## stream should succeed with producer / consumer in any order
    p <- getRefClass("Producer")$new()
    c <- getRefClass("Consumer")$new()
    .checkStream(s1 <- stream(c, p))

    p <- getRefClass("Producer")$new()
    c <- getRefClass("Consumer")$new()
    .checkStream(s2 <- stream(p, c))

    checkEquals(s1, s2)
}

test_Stream_yieldSize <- function()
{
    ## stream should obey overall yield size
    
}
