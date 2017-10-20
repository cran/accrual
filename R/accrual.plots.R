accrual.plots <-
function(w){
  par(mfrow=c(2,2))
  qqexp(w,line=TRUE)
  hist(w, freq = FALSE,xlab="Waiting time",main="" )
  lines(dexp(min(w):max(w),1/mean(w)),col = "red", lwd = 2)
  CumTime=cumsum(w)
  plot(CumTime,w,type="l",xlab="Cumulative time (months)",ylab="Waiting Time")
  sub=1:length(w)
  plot(CumTime,sub,type="l",xlab="Cumulative time (months)",ylab="Number of Subjects Recruited")
  
}
