\name{STNN}
\alias{STNN}
\title{
Spatio-Temporal Neural Network
}

\description{
The STNN function fits a Time-Delay Spatio-Temporal Neural Network model for
space-time time series data.
}

\usage{
STNN(data,lag, weight0, weight1,hs, h)
}
\arguments{
  \item{data}{
Spatio-temporal time series (ts) data.}
\item{lag}{
Number of time lag(s).
}
\item{weight0}{
Zero-order weight matrix.
}
\item{weight1}{
First-order weight matrix.
}
\item{hs}{
Number of hidden layer(s).
}
\item{h}{
The forecast horizon.
}
}
\details{
This function enables you to apply the Time-delay Spatio-Temporal model for
analyzing space-time series data.
}
\value{
  \item{Model Summary}{Summary of the fitted STNN}
  \item{Fitted values }{Fitted values of STNN}
   \item{Forecasted values}{h step ahead forecasted values employing STNN}
}
\references{
Saha, A., Singh, K.N., Ray, M. et al. A hybrid spatio-temporal modelling: an
application to space-time rainfall forecasting. Theor Appl Climatol 142,
1271–1282 (2020).

}
\author{
Mrinmoy Ray, Rajeev Ranjan Kumar, Kanchan Sinha, K. N. Singh
}

\seealso{
ANN
}

\examples{
ts.sim1 <- 50+arima.sim(list(order = c(1,1,0), ar = 0.7), n = 100)
ts.sim2<-70+arima.sim(list(order = c(1,1,0), ar = 0.8), n = 100)
weight0=diag(1, 2, 2)
weight1=matrix(c(0, 1, 1, 0), nrow= 2, ncol = 2, byrow = TRUE)
zz=as.matrix(cbind(ts.sim1,ts.sim2))
data=zz%*%weight0+zz%*%weight1
lag=1
hs=2
h=5
STNN(data,lag,weight0,weight1,hs,h)
}
\keyword{STNN}
