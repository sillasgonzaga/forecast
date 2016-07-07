pi_accuracy <- function(fc, yobs){
   # source: http://ellisp.github.io/blog/2016/01/30/hybrid-forecasts
   # checks the success of prediction intervals of an object of class 
   # forecast with actual values
   if(length(yobs) != length(fc$mean)){
      stop("yobs needs to be the same length as the forecast period.")
   }
   n <- length(yobs)
   yobsm <- cbind(yobs, yobs)
   In <- (yobsm > fc$lower & yobsm < fc$upper) 
   colnames(In) <- c("Series 1", "Series 2")
   Success <- colMeans(In)
   return(list(In = In, Success = Success, n = n))
 }
