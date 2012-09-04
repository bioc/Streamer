test_Rev <-
    function()
{
    checkTrue(validObject(Rev()))

    fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
    s <- Stream(Rev(), RawInput(fl))
    res <- yield(s)
}
