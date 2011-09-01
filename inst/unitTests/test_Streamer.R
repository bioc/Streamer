test_Streamer_initialize_fields <- function()
{
    ## derived classes should have fields initialized automatically
    c1 <- setRefClass("Consumer1",
                      contains="Consumer",
                      fields=list(a="integer"))
    checkIdentical(1L, c1$new(a=1L)$a)

    p1 <- setRefClass("Producer1",
                      contains="Producer",
                      fields=list(a="integer"))
    checkIdentical(1L, p1$new(a=1L)$a)

    s1 <- setRefClass("Stream1",
                      contains="Stream",
                      fields=list(a="integer"))
    checkIdentical(1L, s1$new(a=1L)$a)
}
