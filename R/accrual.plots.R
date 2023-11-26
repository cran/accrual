accrual.plots <-
function(w){

  # Save current graphical parameters
  oldpar <- par(no.readonly = TRUE)
  # set up on.exit to restore graphical parameters when the function exits
  on.exit(par(oldpar))

  par(mfrow=c(2,2))
  qqexp(w,line=TRUE)
  hist(w, freq = FALSE,xlab="Waiting time",main="" )
  lines(dexp(min(w):max(w),1/mean(w)),col = "red", lwd = 2)
  CumTime=cumsum(w)
  plot(CumTime,w,type="l",xlab="Cumulative time (Months)",ylab="Waiting Time")
  sub=1:length(w)
  plot(CumTime,sub,type="l",xlab="Cumulative time (Months)",ylab="Number of Participants Recruited")
}
