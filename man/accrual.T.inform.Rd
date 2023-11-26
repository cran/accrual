\name{accrual.T.inform}
\alias{accrual.T.inform}
\title{
Prediction of Time with Informative Prior
}
\description{
Prediction of time frame with Informative Prior for a certain number of participants.
}
\usage{
accrual.T.inform(n, TT, P, m, tm, np)
}
\value{
A list of prediction on time frame that needed for reaching a certain number of participants with Informative Prior.
}
\arguments{
  \item{n}{
Target sample size
}
  \item{TT}{
Target completion time
}
  \item{P}{
The prior certainty, range 0-1
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
}

\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.T.inform(n=300, TT=36, P=0.5, m=100, tm=10, np=300)[[1]]
}

\keyword{accrual}
\keyword{Bayesian}
