test_2D_Data_read <- function() {
    file <- system.file("extdata", "NetCDFData.nc", package = "Streamer")
    ncFile <- NetCDFFile(file)

    vars <- names(dimensions(ncFile))
    checkIdentical(vars, c( "2dIntData", "2dFloatData"))

    current <- dimensions(ncFile)
    target <- list(`2dIntData` = c(sampleDim=5L, snpDim=10L),
                   `2dFloatData` = c(sampleDim=5L, snpDim=10L ))
    checkIdentical(current, target)
    
    ncprod <- NetCDFInput(ncFile, "2dIntData")
    current <- names(dimensions(ncprod))
    checkIdentical(current, "2dIntData")
    
    slice <- c(sampleDim = 5, snpDim = 5)
    ncprod <- NetCDFInput(ncFile, "2dIntData", slice)
    current <- ncprod$slice
    nms <-  c("sampleDim", "snpDim")
    target <-  structure( c(5L, 5L), names = nms)
    checkIdentical(current, target)

    dat <- yield(ncprod)
    current <- status(ncprod)
    target <- structure(c(1L, 6L), names = c("sampleDim", "snpDim"))
    checkEquals(target, current)
   
    reset(ncprod)
    current <- status(ncprod)
    target <- structure(c(1L, 1L), names = c("sampleDim", "snpDim"))
    checkEquals(target, current)

    current <- yield(ncprod)
    target <- matrix(1:25, ncol = 5)
    checkEquals(target ,current)
   
    current <- yield(ncprod)
    target <- matrix(26:50, ncol = 5)
    checkEquals(target ,current)

    current <- yield(ncprod)
    target <- matrix(numeric(0), 0, 0)
    checkEquals(target ,current)
  
    slice <- c(sampleDim = 4, snpDim = 4)
    ncprod <- NetCDFInput(ncFile, "2dIntData", slice)

    current <- yield(ncprod)
    target <- matrix(c(1:4, 6:9, 11:14, 16:19), ncol = 4)
    checkEquals(target ,current)
    
    current <- yield(ncprod)
    target <-  c(5,10, 15, 20)
    checkEquals(target , as.numeric(current))
    
    current <- yield(ncprod)
    target <- c(21:24, 26:29, 31:34, 36:39)
    checkEquals(target , as.numeric(current))
    current <- yield(ncprod)
    target <-  c(25, 30, 35, 40)
    checkEquals(target , as.numeric(current))

    current <- yield(ncprod)
    target <- matrix(c(41: 44, 46:49), ncol =2)
    checkEquals(target ,current)
    
    current <- yield(ncprod)
    target <- c(45, 50)
    checkEquals(target , as.numeric(current))
    
    current <- yield(ncprod)
    target <- matrix(numeric(0), 0, 0)
    checkEquals(target ,current)
}

