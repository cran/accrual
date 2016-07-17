accrual.T.inform <-
function(n,T,P,m,tm,np) {
  qB=qbeta(c(0.025,0.5,0.975), np-m, n*P+m)
  rB=rbeta(10000,np-m, n*P+m)
  qTOT=(T*P+tm)*qB/(1-qB)+tm
  rTOT=(T*P+tm)*rB/(1-rB)+tm
  return(list(qTOT,rTOT))
}
