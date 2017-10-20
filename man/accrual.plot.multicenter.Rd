\name{accrual.plot.multicenter}
\alias{accrual.plot.multicenter}
\title{
Plot for Prediction of Multicenter Accrual in Fixed Time Frame
}
\description{
Produce a plot and output for prediction of the number of subjects for a multicenter trial can be recruited in a fixed time frame. 
}
\usage{
accrual.plot.multicenter(n,T,P,J,Tm,Tsj,m,all)
}

\arguments{
  \item{n}{
Target sample size
}
  \item{T}{
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
  \item{all}{
Using all the sites (True/False)
}
}

\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski
}

\examples{
accrual.plot.multicenter(n=300,T=36,P=0.5,J=10,Tm=10,Tsj=c(0,0,0,0,0,0,0,0,0,0),
m=c(9,10,10,10,11,11,11,12,12,12),all=TRUE)
}

\keyword{plot}
\keyword{Bayesian}