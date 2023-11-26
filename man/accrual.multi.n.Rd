\name{accrual.multi.n}
\alias{accrual.multi.n}
\title{
Prediction of Multi-center Accrual with Informative Prior in Fixed Time Frame
}
\description{
Produce an output for prediction of the number of total participants will be recruited in a fixed time frame with Informative Prior for a multi-center trial.
}
\usage{
accrual.multi.n(n,TT,P,J,Tm,Tsj,m,Tpred,all)
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
  \item{J}{
The number of sites
}
  \item{Tm}{
Time to date
}
  \item{Tsj}{
The start date for each site
}
  \item{m}{
Sample observed to date for each site
}
  \item{Tpred}{
The specific time that want to predict the recruitment
}
  \item{all}{
Using all the sites (True/False)
}
}
\value{
For a multi-center trial, a list of prediction on the number of total participants that will be recruited in a fixed time frame with Informative Prior.
}
\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.multi.n(n=300,TT=36,P=0.5,J=10,Tm=10,Tsj=c(0,0,0,0,0,0,0,0,0,0),
m=c(9,10,10,10,11,11,11,12,12,12),Tpred=36,all=TRUE)[[1]]
}

\keyword{accrual}
\keyword{Bayesian}
