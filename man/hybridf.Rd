\name{hybridf}
\alias{hybridf}

\title{Hybrid forecast}
\usage{
hybridf(x, h = ifelse(frequency(x) > 1, 2 * frequency(x), 10), 
    level = c(80, 95), fan = FALSE, simulate = FALSE, bootstrap.ets = FALSE, 
    bootstrap.aa = FALSE, npaths = 5000, lambda = NULL, 
    ic = c("aicc", "aic", "bic"), model = "ZZZ", damped = NULL, 
    alpha = NULL, beta = NULL, gamma = NULL, phi = NULL, additive.only = FALSE, 
    lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999, 3), 
        0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), 
    nmse = 3, bounds = c("both", "usual", "admissible"), restrict = TRUE, 
    allow.multiplicative.trend = FALSE, use.initial.values = FALSE, 
    d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2, 
    max.order = 5, max.d = 2, max.D = 1, start.p = 2, start.q = 2, 
    start.P = 1, start.Q = 1, stationary = FALSE, seasonal = TRUE, 
    stepwise = TRUE, trace = FALSE, approximation = (length(x) > 
        100 | frequency(x) > 12), test = c("kpss", "adf", "pp"), 
    seasonal.test = c("ocsb", "ch"), allowdrift = TRUE, allowmean = TRUE, 
    parallel = FALSE, num.cores = 2) 
}

\arguments{
\item{x}{a univariate time series.}
\item{h}{Number of periods for forecasting.}
\item{level}{Confidence level for prediction intervals.}
\item{fan}{If \code{TRUE}, level is set to \code{seq(51,99,by=3)}. This is suitable for fan plots.}

\item{simulate}{If \code{TRUE}, then prediction intervals in the \code{ets()} forecast are produced by simulation rather than using analytic formulae.}
\item{bootstrap.ets}{If \code{TRUE}, and if \code{simulate=TURE}, then simulation uses resampled errors rather than normally distributed errors in the \code{ets()} forecast.}
\item{bootstrap.aa}{If \code{TRUE}, then prediction intervals computed using simulation with resampled errors in the \code{auto.arima()} forecast.}
\item{npaths}{Number of sample paths used in computing simulated prediction intervals when \code{bootstrap.ets=TRUE} or \code{bootstrap.aa=TRUE}.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.}
\item{biasadj}{Use adjusted back-transformed mean for Box-Cox transformations. If TRUE, point forecasts and fitted values are mean forecast. Otherwise, these points can be considered the median of the forecast densities.}
\item{ic}{Information criterion to be used in model selection.}


\item{model}{For \code{ets()}.  Usually a three-character string identifying method using the framework terminology of Hyndman et al. (2002) and
Hyndman et al. (2008).
The first letter denotes the error type ("A", "M" or "Z"); the second letter denotes the trend
type ("N","A","M" or "Z"); and the third letter denotes the season type ("N","A","M" or "Z").
In all cases, "N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically selected.
So, for example, "ANN" is simple exponential  smoothing with additive errors,
"MAM" is multiplicative Holt-Winters' method with multiplicative errors, and so on.}
\item{damped}{For \code{ets()}.  If TRUE, use a damped trend (either additive or multiplicative). If NULL,
    both damped and non-damped trends will be tried and the best model (according to the
    information criterion \code{ic}) returned.}
\item{alpha}{For \code{ets()}.  Value of alpha. If NULL, it is estimated.}
\item{beta}{For \code{ets()}.  Value of beta. If NULL, it is estimated.}
\item{gamma}{For \code{ets()}.  Value of gamma. If NULL, it is estimated.}
\item{phi}{For \code{ets()}.  Value of phi. If NULL, it is estimated.}
\item{additive.only}{For \code{ets()}.  If TRUE, will only consider additive models. Default is FALSE.}
\item{lower}{For \code{ets()}.  Lower bounds for the parameters (alpha, beta, gamma, phi)}
\item{upper}{For \code{ets()}.  Upper bounds for the parameters (alpha, beta, gamma, phi)}
\item{opt.crit}{For \code{ets()}.  Optimization criterion. One of "mse" (Mean Square Error),
    "amse" (Average MSE over first \code{nmse} forecast horizons), "sigma"
    (Standard deviation of residuals), "mae" (Mean of absolute residuals), or "lik" (Log-likelihood, the default).}
\item{nmse}{For \code{ets()}.  Number of steps for average multistep MSE (1<=\code{nmse}<=30).}
\item{bounds}{For \code{ets()}.  Type of parameter space to impose: \code{"usual" }
indicates all parameters must lie between specified lower and
upper bounds; \code{"admissible"} indicates parameters must lie in the
admissible space; \code{"both"} (default) takes the intersection of these regions.}
\item{restrict}{For \code{ets()}.  If \code{TRUE} (default), models with infinite variance will not be allowed.}
\item{allow.multiplicative.trend}{For \code{ets()}.  If \code{TRUE}, models with multiplicative trend are allowed when searching for a model. Otherwise, the model space excludes them. This argument is ignored if a multiplicative trend model is explicitly requested (e.g., using \code{model="MMN"}).}


\item{d}{For \code{auto.arima()}.  Order of first-differencing. If missing, will choose a value based on KPSS test.}
\item{D}{For \code{auto.arima()}.  Order of seasonal-differencing. If missing, will choose a value based on OCSB test.}
\item{max.p}{For \code{auto.arima()}.  Maximum value of p.}
\item{max.q}{For \code{auto.arima()}.  Maximum value of q}
\item{max.P}{For \code{auto.arima()}.  Maximum value of P}
\item{max.Q}{For \code{auto.arima()}.  Maximum value of Q}
\item{max.order}{For \code{auto.arima()}.  Maximum value of p+q+P+Q if model selection is not stepwise.}
\item{max.d}{For \code{auto.arima()}.  Maximum number of non-seasonal differences}
\item{max.D}{For \code{auto.arima()}.  Maximum number of seasonal differences}
\item{start.p}{For \code{auto.arima()}.  Starting value of p in stepwise procedure.}
\item{start.q}{For \code{auto.arima()}.  Starting value of q in stepwise procedure.}
\item{start.P}{For \code{auto.arima()}.  Starting value of P in stepwise procedure.}
\item{start.Q}{For \code{auto.arima()}.  Starting value of Q in stepwise procedure.}
\item{stationary}{For \code{auto.arima()}.  If \code{TRUE}, restricts search to stationary models.}
\item{seasonal}{For \code{auto.arima()}.  If \code{FALSE}, restricts search to non-seasonal models.}

\item{stepwise}{For \code{auto.arima()}.  If \code{TRUE}, will do stepwise selection (faster). Otherwise, it searches over all models. Non-stepwise selection can be very slow, especially for seasonal models.}
\item{trace}{For \code{auto.arima()}.  If \code{TRUE}, the list of ARIMA models considered will be reported.}
\item{approximation}{For \code{auto.arima()}.  If \code{TRUE}, estimation is via conditional sums of squares andthe information criteria used for model selection are approximated. The final model is still computed using maximum likelihood estimation. Approximation should be used for long time series or a high seasonal period to avoid excessive computation times.}
\item{test}{For \code{auto.arima()}.  Type of unit root test to use. See \code{\link{ndiffs}} for details.}
\item{seasonal.test}{For \code{auto.arima()}.  This determines which seasonal unit root test is used. See \code{\link{nsdiffs}} for details.}
\item{allowdrift}{For \code{auto.arima()}.  If \code{TRUE}, models with drift terms are considered.}
\item{allowmean}{For \code{auto.arima()}.  If \code{TRUE}, models with a non-zero mean are considered.}
\item{parallel}{For \code{auto.arima()}.  If \code{TRUE} and \code{stepwise = FALSE}, then the specification search is done in parallel. This can give a significant speedup on mutlicore machines.}
\item{num.cores}{For \code{auto.arima()}.  Allows the user to specify the amount of parallel processes to be used if \code{parallel = TRUE} and \code{stepwise = FALSE}. If \code{NULL}, then the number of logical cores is automatically detected and all available cores are used.}

}

\description{Hybrid forecast combining \code{ets()} and \code{auto.arima()}.
}
\details{The methodology is fully automatic. The only required argument is the time series. An \code{ets()} model and an \code{auto.arima} model are both fit to the data.  The models
are chosen automatically if not specified.  The point estimates ("mean") in the resulting object are the
point-wise average of the forecasts from the two underlying models.  The prediction intervals are made of the widest points from the prediction intervals of the two underlying models.

This methodology performed extremely well on the
M3-competition data in generating point estimates; better than either \code{auto.arima} or \code{ets} individually (see first reference below).  In addition, the prediction intervals provided are wider than the prediction interval of either  \code{auto.arima} or \code{ets} and are successful at a rate closer to the given confidence level than is the case for the individual modesl (see second reference below).
}


\value{An object of class "\code{forecast}".  As well as the minimum elements (\code{model}, \code{method}, \code{mean}, etc) it has elements \code{fc_ets} and \code{fc_aa} which are the objects of class \code{forecast} created from the \code{ets()} and \code{auto.arima()} models that between them make up the hybrid.
}




\references{Hyndman's blog \url{http://XXX}

Peter's blog  \url{http://ellisp.github.io}.
}



\seealso{\code{\link{forecast.Arima}}, \code{\link{forecast.ets}}.}

\author{Peter Ellis}
\examples{fc <- hybridf(USAccDeaths)
par(mfrow = c(3, 1), bty = "l")
plot(fc)
plot(fc$fc_ets)
plot(fc$fc_aa)

}
\keyword{ts}
