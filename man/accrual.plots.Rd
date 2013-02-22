\name{accrual.plots}
\alias{accrual.plots}
\title{
Dignostic Plots
}
\description{
The diagnostic panel shows four figures that help to understand the data distribution.
The figure on the top left is the exponential quantile plot, which checks whether the distribution of waiting times is exponential. 
The top right figure shows the histogram of the waiting times, with the red line is the theoretical exponential distribution.
The figure of waiting time verse cumulative accrual time is shown on the bottom left.
The figure of total accrual verse cumulative accrual time is shown on the bottom right. }
\usage{
accrual.plots(w)
}
\arguments{
  \item{w }{The accrual data set.
}
}
\author{
Yu Jiang, Steve Simon, Matthew S. Mayo , Rama Raghavan, Byron J. Gajewski
}
\examples{
accrual.plots(accrual.data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ exponetial }
\keyword{ Diagnostic }
