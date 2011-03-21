
test_2D_Data_read <- function() {
    file <- system.file("extdata", "NetCDFData.nc", package = "Streamer")
    ncprod <- NetCDFProducer(file)
    vars <- variableNames(ncprod)
    checkIdentical(vars, c( "2dIntData", "2dFloatData"))

    sliceDimensions(ncprod, "2dIntData") <- list(sampleDim = 5, snpDim =5)
    current <- sliceDimensions(ncprod, "2dIntData")
    target <- structure(c(5,5), names = c( "sampleDim", "snpDim"))
    checkEquals(target, current)

    dat <- yield(ncprod, "2dIntData")
    current <- status(ncprod, "2dIntData")
    target <- structure( c(1,6), names = c("sampleDim", "snpDim"))
    checkEquals(target, current)
    
    reset(ncprod, "2dIntData")
    current <- status(ncprod, "2dIntData")
    target <- structure( c(1,1), names = c("sampleDim", "snpDim"))
    checkEquals(target, current)

    current <- yield(ncprod, "2dIntData")
    target <- matrix(1:25, ncol = 5)
    checkEquals(target ,current)
   
    current <- yield(ncprod, "2dIntData")
    target <- matrix(26:50, ncol = 5)
    checkEquals(target ,current)

    current <- yield(ncprod, "2dIntData")
    target <- matrix(numeric(0), 0, 0)
    checkEquals(target ,current)
  
    reset(ncprod, "2dIntData")
    sliceDimensions(ncprod, "2dIntData") <- list(sampleDim = 4, snpDim = 4)

    current <- yield(ncprod, "2dIntData")
    target <- matrix( c(1:4, 6:9, 11:14, 16:19), ncol = 4)
    checkEquals(target ,current)
    
    current <- yield(ncprod, "2dIntData")
    target <-  c(5,10, 15, 20)
    checkEquals(target , as.numeric(current))
    
    current <- yield(ncprod, "2dIntData")
    target <- c(21:24, 26:29, 31:34, 36:39)
    checkEquals(target , as.numeric(current))
    current <- yield(ncprod, "2dIntData")
    target <-  c(25, 30, 35, 40)
    checkEquals(target , as.numeric(current))

    current <- yield(ncprod, "2dIntData")
    target <- matrix(c(41: 44, 46:49), ncol =2)
    checkEquals(target ,current)
    
    current <- yield(ncprod, "2dIntData")
    target <- c(45, 50)
    checkEquals(target , as.numeric(current))
    
    current <- yield(ncprod, "2dIntData")
    target <- matrix(numeric(0), 0, 0)
    checkEquals(target ,current)

}

