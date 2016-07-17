\name{accrual.n.hedging}
\alias{accrual.n.hedging}
\title{
Prediction of Accrual with Hedging Prior in Fixed Time Frame
}
\description{
Produce an output for prediction of the number of subjects can be recruited in a fixed time frame with Hedging Prior.
}
\usage{
accrual.n.hedging(n, T, m, tm, Tp)
}

\arguments{
  \item{n}{
Target sample size
}
  \item{T}{
Target completion time
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
}

\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
 accrual.n.hedging(n=300, T=36, m=100, tm=10, Tp=36)[[1]]
}

\keyword{accrual}
\keyword{Bayesian}
