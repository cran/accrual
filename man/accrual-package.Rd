\name{accrual-package}
\alias{accrual-package}
\alias{accrual}
\docType{package}
\title{
Bayesian Accrual Prediction
}
\description{
Participant recruitment for medical research is challenging. Slow accrual leads to delays in research. Accrual monitoring during the process of recruitment is critical. Researchers need reliable tools to manage the accrual rate. We developed a Bayesian method that integrates the researcher's experience with previous trials and data from the current study, providing reliable predictions on accrual rate for clinical studies. In this R package, we present functions for Bayesian accrual prediction which can be easily used by statisticians and clinical researchers.
}
\details{
\tabular{ll}{
Package: \tab accrual\cr
Type: \tab Package\cr
Version: \tab 1.4\cr
Date: \tab 2023-11-23\cr
License:\tab  GPL-2\cr
}
There are major eight functions in the package. The accrual.gui function provides the gui version.
}
\author{
Junhao Liu, Yu Jiang, Cen Wu, Steve Simon, Matthew S. Mayo, Rama Raghavan, Byron J. Gajewski

Maintainer: Junhao Liu <liujunhao2008@gmail.com>
}
\references{
[1] Byron J. Gajewski, Stephen D. Simon, Susan E. Carlson (2008). Predicting accrual in clinical trials with Bayesian posterior predictive distributions. Stat Med. 27(13):2328-40.

[2] Yu Jiang, Steve Simon, Matthew S. Mayo, Byron J. Gajewski (2015). Modeling and validating Bayesian accrual models on clinical data and simulations using adaptive priors. Statistics in medicine, 34(4), 613-629.
}
\keyword{accrual}
\keyword{Bayesian}
\examples{
accrual.n.inform(n=300, T=36, P=0.5, m=100, tm=10, Tp=36)
accrual.n.plot(n=300, T=36, P=0.5, m=100, tm=10, Tp=36, Method="Informative Prior")
accrual.T.plot(n=300, T=36, P=0.5, m=100, tm=10, np=300, Method="Informative Prior")
accrual.gui()
}
