\name{accrual.n.plot}
\alias{accrual.n.plot}
\title{
Plot for Prediction of Accrual in Fixed Time Frame
}
\description{
Produce a plot and output for prediction of the number of participants will be recruited in a fixed time frame.
}
\usage{
accrual.n.plot(n, TT, P, m, tm, Tp, Method)
}
\value{
A list of prediction and corresponding plot on the number of participants that will be recruited in a fixed time frame.
}
\arguments{
  \item{n}{
Target sample size
}
  \item{TT}{
Target completion time
}
  \item{P}{
The prior certainty, range 0-1; For Accelerated Prior, P = 1-m/n
}
  \item{m}{
Sample observed to date
}
  \item{tm}{
Time to date
}
  \item{Tp}{
The specific time that want to predict the recruitment
}
  \item{Method}{
Informative Prior, Accelerated Prior, Hedging Prior
}
}


\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.n.plot(n=300, TT=36, P=0.5, m=100, tm=10, Tp=36, Method="Informative Prior")
accrual.n.plot(n=300, TT=36, m=100, tm=10, Tp=36, Method="Accelerated Prior")
accrual.n.plot(n=300, TT=36, m=100, tm=10, Tp=36, Method="Hedging Prior")
}

\keyword{plot}
\keyword{Bayesian}
