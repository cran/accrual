library('fgui')# For R gui
library('tcltk')# For R gui
library('SMPracticals')# for qqexp

# n is the target sample size
# T is the target completion time
# P is the prior certainty (0 <= P <= 1)
# m is the sample observed to date
# tm is the time to date
# B is the number of simulated accrual times
# nmax is the upper limit on the sample size axis of the graph
# set P to zero for a non-informative prior
# set m and tm to zero if no data has been accumulated yet.
# How the data generated 

accrual.data<-c(5,  3,  1,  1,  0,  2,  3,  6,  3,  4,  0,  5,  2,  9,  2,  4 , 1, 8,  3,  4,  0,  3 , 6,  2,  3,
 9,  8,  8,  8,  9,  0,  7, 11,  1,  1,  2,  7,  3,  6,  1,  3,  2,  0,  6,  0,  1,  2,  7, 10,  6,
 4,  2, 23,  2,  5,  4,  0,  6,  5,  2,  1,  5,  7,  0,  3,  1,  3,  4,  3,  3, 11,  7,  2,  5,  0,
3, 10,  5,  3,  1,  2,  0,  0,  1,  3,  4,  6,  9,  4,  0,  4,  3, 10,  2,  1,  3,  1,  6,  3,  0,
3,  2,  2,  2, 1,  3,  7,  0,  4,  2,  3,  3,  7,  4,  0,  3,  5,  1,  2,  0, 10, 10,  2,  1,  5,
4,  0,  2,  0,  7, 15,  5,  4,  6,  4,  5,  2,  4,  0,  2,  1,  5,  0,  2,  3,  1,  3,  0,  0, 12,
 2,  0,  1,  0,  0, 10,  3,  3,  1, 12,  2,  2,  3,  4,  9,  5,  7,  0,  7,  1,  1,  1,  0, 1,  9,
5,  1,  2,  2,  5,  5,  1,  0,  2,  4,  2,  4,  2,  1,  4,  0,  0,  3,  4,  7,  0,  3, 17,  3,  6,
4,  1,  0,  2,  2,  1,  0,  1,  1,  3,  4, 13, 18,  2, 2,  1,  6,  3, 11,  2,  1,  5,  6,  9,  3,
5,  3,  4,  9,  4,  1,  7,  1,  1,  4,  2,  2,  5,  0,  1,  0,  1,  5, 10,  1,  8,  2,  7,  2,  3,
8,  2,  0,  2,  1, 17,  1,  0,  3,  5,  1,  7,  1,  2,  4,  2,  2,  8,  1,  4,  1,  3,  2, 11,  2,
0,  1,  1,  2,  1,  4,  1,  1,  3,  6,  1,  5,  4,  1,  0,  3,  1,  9,  7,  2,  1,  3,  0,  4,  5)


## function for accrual plot
accrual.n <- function(n,T,P,m,tm,nmax=2*n) {
  B=1000
  sample.paths=0
  k <- n*P+m
  V <- T*P+tm
  theta <- 1/rgamma(B,shape=k,rate=V)
  simulated.duration <- matrix(NA,nrow=nmax-m,ncol=B)

  for (i in 1:B) {
    wait <- rexp(nmax-m,1/theta[i])
    simulated.duration[,i] <- tm+cumsum(wait)
  }

  tlist <- seq(tm,T,length=100)
  accrual.count <- function(x,t) {m+sum(x<=t)}
  simulated.accrual <- matrix(NA,nrow=100,ncol=B)

  for (i in 1:100) {
    time <- tlist[i]
    simulated.accrual[i,] <- apply(simulated.duration,2,
      accrual.count,t=time)
  }

  lcl <- apply(simulated.accrual,1,quantile,probs=0.025)
  mid <- apply(simulated.accrual,1,quantile,probs=0.500)
  ucl <- apply(simulated.accrual,1,quantile,probs=0.975)



  simulated.duration <- matrix(NA,nrow=n-m,ncol=B)

  for (i in 1:B) {
    wait <- rexp(n-m,1/theta[i])
    simulated.duration[,i] <- tm+cumsum(wait)
  }
  lclD <- round(quantile(simulated.duration[n-m,], probs=0.025),digits=1)
  midD <- round(quantile(simulated.duration[n-m,], probs=0.500),digits=1)
  uclD <- round(quantile(simulated.duration[n-m,], probs=0.975),digits=1)

  lclT <- round(quantile(simulated.accrual[100,],probs=0.025))
  midT <- round(quantile(simulated.accrual[100,],probs=0.5))
  uclT <- round(quantile(simulated.accrual[100,],probs=0.975))
 

  layout(matrix(c(2,2,2,1),nrow=1))
  par(mar=c(4.1,0.1,2.1,0.1))
  accrual.hist <- cut(simulated.accrual[100,],
    seq(0,nmax,length=40))
  barplot(table(accrual.hist),horiz=TRUE,
    axes=FALSE,xlab=" ",ylab=" ",space=0,
    col="white",names.arg=rep(" ",39),
    )

  par(mar=c(4.1,4.1,2.1,0.1))
  plot(c(-0.5,T),c(0,nmax),xlab="Time",
    ylab="Number of patients",type="n",xaxt = 'n')
  axis(1, at=seq(0,T, 6)) 
   
  legenda=paste("Total subjects needed:", n)
  legendb=paste("Espected total months to finish:",T)
  legendd=paste("Time to now:", tm)
  legendc=paste("Subjects recruited:",m)
  legende=paste("Subjects in", T, "months:",midT,"(",lclT,",",uclT,")" )
  legendf=paste("Time for",n, "subjects:",midD,"(",lclD,",",uclD,")" )


  legend(-0.5, nmax, legend=c("Input Information:",
         legenda,legendb,legendc,legendd,"------------------------","Summary of results:",legende,legendf))




  polygon(c(tlist,rev(tlist)),c(lcl,rev(ucl)),
    density=-1,col="gray",border=NA)
  lines(tlist,mid,col="white")
  segments(0,0,tm,m)
  lines(rep(n,100),col="red")


  while (sample.paths>0) {
    lines(tlist,simulated.accrual[,sample.paths])
    sample.paths <- sample.paths-1
  } 

  return(list(paste("2.5%=",lclT), paste("50%=",midT),paste("97.5%=",uclT)))

}



accrual.T<- function(n,T,P,m,tm,Tmax=2*T){
  B=1000
  sample.paths=0
  k <- n*P+m
  V <- T*P+tm
  theta <- 1/rgamma(B,shape=k,rate=V)
 
  simulated.duration <- matrix(NA,nrow=n-m,ncol=B)

  for (i in 1:B) {
    wait <- rexp(n-m,1/theta[i])
    simulated.duration[,i] <- tm+cumsum(wait)
  }


  tlist <- seq(tm,T,length=100)
  accrual.count <- function(x,t) {m+sum(x<=t)}
  simulated.accrual <- matrix(NA,nrow=100,ncol=B)

  for (i in 1:100) {
    time <- tlist[i]
    simulated.accrual[i,] <- apply(simulated.duration,2,
      accrual.count,t=time)
  }

  lcl <- apply(simulated.duration,1,quantile,probs=0.025)
  mid <- apply(simulated.duration,1,quantile,probs=0.500)
  ucl <- apply(simulated.duration,1,quantile,probs=0.975)

  

  for (i in 1:B) {
    wait <- rexp(n-m,1/theta[i])
    simulated.duration[,i] <- tm+cumsum(wait)
  }
  lclD <- round(quantile(simulated.duration[n-m,], probs=0.025),digits=1)
  midD <- round(quantile(simulated.duration[n-m,], probs=0.500),digits=1)
  uclD <- round(quantile(simulated.duration[n-m,], probs=0.975),digits=1)

  lclT <- round(quantile(simulated.accrual[100,],probs=0.025))
  midT <- round(quantile(simulated.accrual[100,],probs=0.5))
  uclT <- round(quantile(simulated.accrual[100,],probs=0.975))

  layout(matrix(c(1,2,2,2)))
  par(mar=c(0.1,4.1,0.1,0.1))
  duration.hist <- cut(simulated.duration[n-m,],
    seq(0,Tmax,length=40))
  barplot(table(duration.hist),horiz=FALSE,
    axes=FALSE,xlab=" ",ylab=" ",space=0,
    col="white",names.arg=rep(" ",39))

  par(mar=c(4.1,4.1,0.1,0.1))
  plot(c(0,Tmax),c(0,n),xlab="Time (Months)",
    ylab="Number of patients",xaxt = 'n',type="n")
  axis(1, at=seq(0,Tmax, 6)) 
  

  
  legenda=paste("Total subjects needed:", n)
  legendb=paste("Espected total months to finish:",T)
  legendd=paste("Subjects recruited:",m)
  legendc=paste("Time to now (month):", tm)
  legende=paste("Subjects in", T, "months:",midT,"(",round(lclT),",",round(uclT),")" )
  legendf=paste("Time for",n, "subjects:", round(midD,digits=1),"(",round(lclD,digits=1),",",round(uclD,digits=1),")" )



  legend(T, n/3, legend=c("Input Information:",
         legenda,legendb,legendc,legendd,"------------------------","Summary of results:",legendf,legende))


  polygon(c(lcl,rev(ucl)),c((m+1):n,n:(m+1)),
    density=-1,col="gray",border=NA)
  lines(mid,(m+1):n,col="white")
  abline(v = T, col = "red") 
  segments(0,0,tm,m)

  while (sample.paths>0) {
    lines(simulated.duration[,sample.paths],(m+1):n)
    sample.paths <- sample.paths-1
  } 
 
   return(list(paste("2.5%=",lclD), paste("50%=",midD),paste("97.5%=",uclD)))

}

## Function for diagonistic plot
  accrual.plots=function(w){
  par(mfrow=c(2,2))
  qqexp(w,line=TRUE)
  hist(w, freq = FALSE,xlab="Waiting time",main="" )
  lines(dexp(min(w):max(w),1/mean(w)),col = "red", lwd = 2)
  CumTime=cumsum(w)
  plot(CumTime,w,type="l",xlab="Cumulative time (days)",ylab="Waiting Time")
  sub=1:length(w)
  plot(CumTime,sub,type="l",xlab="Cumulative time (days)",ylab="No of Subjects Recruited")

}



accrual.gui <- function(){
## Gui for Acurral analysis                        ######
accrualtimecallbackplot<- function(Total_smaple_size,Targeted_finish_time_in_months,Your_confidence,
                       Subject_recruited,Total_months_after_started) {
 n=Total_smaple_size
 T=Targeted_finish_time_in_months
 P=Your_confidence
 m=Subject_recruited
 tm=Total_months_after_started
 return <-accrual.n(n,T,P, m,tm,nmax=n*2)
}


durationcallbackplot<- function(Total_smaple_size,Targeted_finish_time_in_months,Your_confidence,
                       Subject_recruited,Total_months_after_started) {
 n=Total_smaple_size
 T=Targeted_finish_time_in_months
 P=Your_confidence
 m=Subject_recruited
 tm=Total_months_after_started
 return <-accrual.T(n,T,P, m,tm)
}

diagexpgui <- function(w,Header) {
  if (Header==TRUE){ w <- as.matrix( read.csv(w),header=TRUE)}
  if (Header==FALSE){w <- as.matrix( read.csv(w),header=FALSE)}
  return(accrual.plots(w))
  
}

diagguiCallback <- function( arg ) {
  if( arg=="w" ) {
    datanames1 <- names( read.csv( guiGetValue("w") ) )
    print( datanames1 )
    guiSet( "datanames1", datanames1 )
    }
  
  }



mgui(accrualtimecallbackplot,title=c("Menu","How many patients will you recruit?"),
     argSlider=list(Your_confidence=c(0,1,0.05)))

mgui(durationcallbackplot,title=c("Menu","How long to reach the targeted sample size?"),
     argSlider=list(Your_confidence=c(0,1,0.05)))

mgui(diagexpgui,title=c("Menu","Dignostic Plots"),
     argFilename=list(w=NULL), callback=diagguiCallback, argOption=list(Header=c("TRUE","FALSE")) )


}






