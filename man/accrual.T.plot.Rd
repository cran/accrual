\name{accrual.T.plot}
\alias{accrual.T.plot}
\title{
Plot for Prediction of Time
}
\description{
Produce a plot and output for prediction of time frame for a certain number of participants.
}
\usage{
accrual.T.plot(n, TT, P, m, tm, np, Method)
}
\value{
A list of prediction and corresponding plot on time frame that needed for reaching a certain number of participants.
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
  \item{np}{
The specific number of participants want to be predicted
}
  \item{Method}{
Informative Prior, Accelerated Prior, Hedging Prior
}
}

\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.T.plot(n=300, TT=36, P=0.5, m=100, tm=10, np=300, Method="Informative Prior")
accrual.T.plot(n=300, TT=36, m=100, tm=10, np=300, Method="Accelerated Prior")
accrual.T.plot(n=300, TT=36, m=100, tm=10, np=300, Method="Hedging Prior")
}

\keyword{plot}
\keyword{Bayesian}
