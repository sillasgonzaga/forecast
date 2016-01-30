\name{hybridf}
\alias{hybridf}

\title{Hybrid forecast}
\usage{
hybridf(x, h = ifelse(frequency(x) > 1, 2 * frequency(x), 10), 
    level = c(80, 95), fan = FALSE, simulate = FALSE, bootstrap.ets = FALSE, 
    bootstrap.aa = FALSE, npaths = 5000, ..., lambda = NULL, 
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
\item{x}{a univariate time series}
\item{h}{Number of periods for forecasting.}
\item{level}{Confidence level for prediction intervals.}
\item{fan}{If \code{TRUE}, level is set to \code{seq(51,99,by=3)}. This is suitable for fan plots.}

\item{simulate}{If \code{TRUE}, then prediction intervals in the \code{ets()} forecast are produced by simulation rather than using analytic formulae.}
\item{bootstrap.ets}{If \code{TRUE}, then prediction intervals computed using simulation with resampled errors in the \code{ets()} forecast.}
\item{bootstrap.aa}{If \code{TRUE}, then prediction intervals computed using simulation with resampled errors in the \code{auto.arima()} forecast.}
\item{npaths}{Number of sample paths used in computing simulated prediction intervals when \code{bootstrap=TRUE}.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, forecasts back-transformed via an inverse Box-Cox transformation.}
\item{biasadj}{Use adjusted back-transformed mean for Box-Cox transformations. If TRUE, point forecasts and fitted values are mean forecast. Otherwise, these points can be considered the median of the forecast densities.}

\item{model}{Usually a three-character string identifying method using the framework terminology of Hyndman et al. (2002) and
Hyndman et al. (2008).
The first letter denotes the error type ("A", "M" or "Z"); the second letter denotes the trend
type ("N","A","M" or "Z"); and the third letter denotes the season type ("N","A","M" or "Z").
In all cases, "N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically selected.
So, for example, "ANN" is simple exponential  smoothing with additive errors,
"MAM" is multiplicative Holt-Winters' method with multiplicative errors, and so on.

It is also possible for the model to be of class \code{"ets"}, and equal to the output from a previous call to \code{ets}. In this case, the same model is fitted to \code{y} without re-estimating any smoothing parameters. See also the \code{use.initial.values} argument.}
\item{damped}{If TRUE, use a damped trend (either additive or multiplicative). If NULL,
    both damped and non-damped trends will be tried and the best model (according to the
    information criterion \code{ic}) returned.}
\item{alpha}{Value of alpha. If NULL, it is estimated.}
\item{beta}{Value of beta. If NULL, it is estimated.}
\item{gamma}{Value of gamma. If NULL, it is estimated.}
\item{phi}{Value of phi. If NULL, it is estimated.}
\item{additive.only}{If TRUE, will only consider additive models. Default is FALSE.}
\item{lambda}{Box-Cox transformation parameter. Ignored if NULL. Otherwise, data transformed before model is estimated. When \code{lambda=TRUE}, \code{additive.only} is set to FALSE.}
\item{biasadj}{Use adjusted back-transformed mean for Box-Cox transformations. If TRUE, point forecasts and fitted values are mean forecast. Otherwise, these points can be considered the median of the forecast densities.}
\item{lower}{Lower bounds for the parameters (alpha, beta, gamma, phi)}
\item{upper}{Upper bounds for the parameters (alpha, beta, gamma, phi)}
\item{opt.crit}{Optimization criterion. One of "mse" (Mean Square Error),
    "amse" (Average MSE over first \code{nmse} forecast horizons), "sigma"
    (Standard deviation of residuals), "mae" (Mean of absolute residuals), or "lik" (Log-likelihood, the default).}
\item{nmse}{Number of steps for average multistep MSE (1<=\code{nmse}<=30).}
\item{bounds}{Type of parameter space to impose: \code{"usual" }
indicates all parameters must lie between specified lower and
upper bounds; \code{"admissible"} indicates parameters must lie in the
admissible space; \code{"both"} (default) takes the intersection of these regions.}
\item{ic}{Information criterion to be used in model selection.}
\item{restrict}{If \code{TRUE} (default), the models with infinite variance will not be allowed.}
\item{allow.multiplicative.trend}{If \code{TRUE}, models with multiplicative trend are allowed when searching for a model. Otherwise, the model space excludes them. This argument is ignored if a multiplicative trend model is explicitly requested (e.g., using \code{model="MMN"}).}
\item{use.initial.values}{If \code{TRUE} and \code{model} is of class \code{"ets"}, then the initial values in the model are also not re-estimated.}
\item{...}{Other undocumented arguments.}
}

\description{Returns ets model applied to \code{y}.
}
\details{Based on the classification of methods as described in Hyndman et al (2008).

The methodology is fully automatic. The only required argument for ets is the time series. The model
is chosen automatically if not specified. This methodology performed extremely well on the
M3-competition data. (See Hyndman, et al, 2002, below.)
}


\value{An object of class "\code{ets}".

The generic accessor functions \code{fitted.values} and \code{residuals} extract useful features of
the value returned by \code{ets} and associated functions.}



\references{Hyndman, R.J., Koehler, A.B., Snyder,
R.D., and Grose, S. (2002) "A state space framework for automatic
forecasting using exponential smoothing methods",
\emph{International J. Forecasting}, \bold{18}(3), 439--454.

Hyndman, R.J., Akram, Md., and Archibald, B. (2008) "The
admissible parameter space for exponential smoothing models".
\emph{Annals of Statistical Mathematics}, \bold{60}(2),
407--426.

Hyndman, R.J., Koehler, A.B., Ord, J.K., and Snyder, R.D. (2008)
\emph{Forecasting with exponential smoothing: the state space approach},
Springer-Verlag. \url{http://www.exponentialsmoothing.net}.
}



\seealso{\code{\link[stats]{HoltWinters}}, \code{\link{rwf}}, \code{\link{Arima}}.}

\author{Rob J Hyndman}
\examples{fit <- ets(USAccDeaths)
plot(forecast(fit))
}
\keyword{ts}
