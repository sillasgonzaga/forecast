# Defaults:
#For non-seasonal data, p chosen using AIC from linear AR(p) model
#For seasonal data, p chosen using AIC from linear AR(p) model after
#    seasonally adjusting with STL decomposition, and P=1
#size set to average of number of inputs and number of outputs: (p+P+1)/2
#if xreg is included then size = (p+P+ncol(xreg)+1)/2

nnetar <- function(x, p, P=1, size, repeats=20, xreg=NULL, lambda=NULL, ...)
{
  # Transform data
  if(!is.null(lambda))
    xx <- BoxCox(x,lambda)
  else
    xx <- x

  # Check xreg class & dim
  if(!is.null(xreg))
  {
    xreg <- as.matrix(xreg)
    if(length(x) != nrow(xreg))
      stop("Number of rows in xreg does not match series length")
  }
  # Scale data
  scale <- max(abs(xx),na.rm=TRUE)
  xx <- xx/scale
  # Set up lagged matrix
  n <- length(xx)
  xx <- as.ts(xx)
  m <- frequency(xx)
  if(m==1)
  {
    if(missing(p))
      p <- max(length(ar(na.interp(xx))$ar),1)
    lags <- 1:p
    P <- 0
  }
  else
  {
    if(missing(p))
    {
      x.sa <- seasadj(stl(na.interp(xx),s.window=7))
      p <- max(length(ar(x.sa)$ar),1)
    }
    if(P > 0)
      lags <- sort(unique(c(1:p,m*(1:P))))
    else
      lags <- 1:p
  }
  maxlag <- max(lags)
  nlag <- length(lags)
  y <- xx[-(1:maxlag)]
  lags.X <- matrix(NA,ncol=nlag,nrow=n-maxlag)
  for(i in 1:nlag)
    lags.X[,i] <- xx[(maxlag-lags[i]+1):(n-lags[i])]
  # Add xreg into lagged matrix
  lags.X <- cbind(lags.X, xreg[-(1:maxlag), ])
  if(missing(size))
    size <- round((ncol(lags.X)+1)/2)
  # Remove missing values if present
  j <- complete.cases(lags.X,y)
  # Fit average ANN.
  fit <- avnnet(lags.X[j,],y[j],size=size,linout=1,trace=FALSE,repeats=repeats, ...)
  # Return results
  out <- list()
  out$x <- as.ts(x)
  out$m <- m
  out$p <- p
  out$P <- P
  out$scale <- scale
  out$size <- size
  out$xreg <- xreg
  out$lambda <- lambda
  out$model <- fit
  fits <- c(rep(NA,maxlag), rowMeans(sapply(fit, predict)))
  fits <- ts(fits*scale)
  if(!is.null(lambda))
    fits <- InvBoxCox(fits,lambda)
  out$fitted <- ts(numeric(length(out$x)))
  out$fitted[j] <- fits
  tsp(out$fitted) <- tsp(out$x)
  out$residuals <- out$x - out$fitted
  out$lags <- lags
  out$series <- deparse(substitute(x))
  out$method <- paste("NNAR(",p,sep="")
  if(P>0)
    out$method <- paste(out$method,",",P,sep="")
  out$method <- paste(out$method,",",size,")",sep="")
  out$call <- match.call()
  return(structure(out,class=c("nnetar")))
}

# Aggregate several neural network models

avnnet <- function(x,y,repeats, ...)
{
  mods <- list()
  for(i in 1:repeats)
    mods[[i]] <- nnet::nnet(x, y, ...)
  return(structure(mods,class="nnetarmodels"))
}

print.nnetarmodels <- function(x, ...)
{
    cat(paste("\nAverage of",length(x),"networks, each of which is\n"))
    print(x[[1]])
}


forecast.nnetar <- function(object, h=ifelse(object$m > 1, 2 * object$m, 10), xreg=NULL, lambda=object$lambda, ...)
{
#  require(nnet)
  out <- object
  tspx <- tsp(out$x)

  # Check if xreg was used in fitted model
  if(is.null(object$xreg))
  {
    if(!is.null(xreg))
      warning("External regressors were not used in fitted model, xreg will be ignored")
    xreg <- NULL
  }
  else
  {
    if(is.null(xreg))
      stop("No external regressors provided")
    xreg <- as.matrix(xreg)
    if(ncol(xreg) != ncol(object$xreg))
      stop("Number of external regressors does not match fitted model")
    h <- nrow(xreg)
  }
  fcast <- numeric(h)
  xx <- object$x
  if(!is.null(lambda))
    xx <- BoxCox(xx,lambda)
  flag <- rev(tail(xx/object$scale, n=max(object$lags)))
  for(i in 1:h)
  {
    fcast[i] <- mean(sapply(object$model, predict, newdata=c(flag[object$lags], xreg[i, ])))
    flag <- c(fcast[i],flag[-length(flag)])
  }
  out$mean <- ts(fcast*object$scale,start=tspx[2]+1/tspx[3],frequency=tspx[3])
  if(!is.null(lambda))
    out$mean <- InvBoxCox(out$mean,lambda)

  return(structure(out,class="forecast"))
}

print.nnetar <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("Series:", x$series, "\n")
    cat("Model: ", x$method, "\n")
    #cat("  one hidden layer with",x$size,"nodes\n")
    cat("Call:   ")
    print(x$call)
    print(x$model)
    cat("\nsigma^2 estimated as ", format(mean(residuals(x)^2,na.rm=TRUE), digits = digits),
            "\n", sep = "")
    invisible(x)
}

is.nnetar <- function(x){
  inherits(x, "nnetar")
}

is.nnetarmodels <- function(x){
  inherits(x, "nnetarmodels")
}

# fitted.train <- function(object, ...)
# {
#   caret::predict.train(object)
# }

# residuals.train <- function(object, ...)
# {
#   object$trainingData[,".outcome"] - caret::predict.train(object)
# }

# fitted.avNNet <- function(object, ...)
# {
#   fit <- mean(unlist(lapply(object$model, predict.nnet)))
#   if(!is.null(object$lambda))
#     fit <- InvBoxCox(fit,object$lambda)
#   return(fit)
# }

# residuals.avNNet <- function(object, ...)
# {
#   object$model[[1]]$fitted.values + object$model[[1]]$residuals - caret::predict.avNNet(object)
# }
