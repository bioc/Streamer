test_RevStream <-
    function()
{
    checkTrue(validObject(RevStream()))

    fl <- system.file("extdata", "s_1_sequence.txt", package="Streamer")
    s <- stream(RevStream(), BinaryInput(fl))
    res <- yield(s)
}
