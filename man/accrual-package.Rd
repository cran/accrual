\name{accrual-package}
\alias{accrual-package}
\alias{accrual}
\docType{package}
\title{
Bayesian Accrual Prediction
}
\description{
Description: Subject recruitment for medical research is challenging. Slow patient accrual leads to delay in research. Accrual monitoring during the process of recruitment is critical. Researchers need reliable tools to manage the accrual rate. We developed a Bayesian method that integrates researcher's experience on previous trials and data from the current study,providing reliable prediction on accrual rate for clinical studies. In this R package, we present functions for Bayesian accrual prediction which can be easily used by statisticiana and clinical researchers.
}
\details{
\tabular{ll}{
Package: \tab accrual\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2016-06-18\cr
License:\tab  GPL-2\cr
}
There are major eight funtions in the package. The accrual.gui function provides the gui version.
}
\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski

Maintainer:Junhao Liu <jliu4@kumc.edu>
}
\references{
[1] Gajewski BJ, Simon SD, Carlson SE (2008). Predicting accrual in clinical trials with Bayesian posterior predictive distributions. Stat Med. 27(13):2328-40.

[2] Jiang, Y., Simon, S., Mayo, M. S., & Gajewski, B. J. (2015). Modeling and validating Bayesian accrual models on clinical data and simulations using adaptive priors. Statistics in medicine, 34(4), 613-629.
}
\keyword{accrual}
\keyword{Bayesian}
\examples{
accrual.n.inform(n=300, T=36, P=0.5, m=100, tm=10, Tp=36)
accrual.n.plot(n=300, T=36, P=0.5, m=100, tm=10, Tp=36, Method="Informative Prior")
accrual.T.plot(n=300, T=36, P=0.5, m=100, tm=10, np=300, Method="Informative Prior")
accrual.gui()
}