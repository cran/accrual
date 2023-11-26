accrual.n.inform <-
function(n,TT,P,m,tm,Tp) {
  prob=(TT*P+tm)/(TT*P+Tp)
  r=n*P+m
  pred.n.Tp=qnbinom(c(0.025,0.5,0.975),r,prob)+m
  dist.n.Tp=rnbinom(10000,r,prob)+m
  return(list(pred.n.Tp,dist.n.Tp))
}
