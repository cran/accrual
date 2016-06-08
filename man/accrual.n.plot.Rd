\name{accrual.n.plot}
\alias{accrual.n.plot}
\title{Prediction of Accrual in Fixed Time Frame
}
\description{
Produce a plot and output for prediction of the number of subjects can be recruited in a fixed time frame
}
\usage{
accrual.n.plot(n, T, P, m, tm,Tp)
}

\arguments{
  \item{n}{
Total sample size
}
  \item{T}{
Total planned time
}
  \item{P}{
Research's confidence in the accrual process, range 0-1
}
  \item{m}{
Subject been recruited
}
  \item{tm}{
Time to now
}

 \item{Tp}{
The specific time that want to predict the recruitment 
}

}


\author{
Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo , Rama Raghavan, Byron J. Gajewski
}

\examples{
 accrual.n.plot(n=300, T=36, P=0.5, m=100, tm=10, Tp=36) 
}
\keyword{accrual}
\keyword{Byesian}
