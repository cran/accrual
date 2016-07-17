accrual.gui <-
function(){
  ## Gui for Acurral analysis                        ######
  accrualcallbackplot<- function(Targeted_sample_size,Targeted_finish_time_in_months,Your_confidence,
                                 Total_subjects_recruited_to_date,Time_to_date_in_months) {
    n=Targeted_sample_size
    T=Targeted_finish_time_in_months
    P=Your_confidence
    m=Total_subjects_recruited_to_date
    tm=Time_to_date_in_months
    return <-accrual.n.plot(n,T,P,m,tm,T,Method="Informative Prior")
  }
  
  accrualcallbackplotadaptive<- function(Targeted_sample_size,Targeted_finish_time_in_months,Method,
                                         Total_subjects_recruited_to_date,Time_to_date_in_months){
    n=Targeted_sample_size
    T=Targeted_finish_time_in_months
    m=Total_subjects_recruited_to_date
    tm=Time_to_date_in_months
    Method=Method
    return <-accrual.n.plot(n,T,P=NULL,m,tm,T,Method)
  }
  
  
  durationcallbackplot<- function(Targeted_sample_size,Targeted_finish_time_in_months,Your_confidence,
                                  Total_subjects_recruited_to_date,Time_to_date_in_months) {
    n=Targeted_sample_size
    T=Targeted_finish_time_in_months
    P=Your_confidence
    m=Total_subjects_recruited_to_date
    tm=Time_to_date_in_months
    return <-accrual.T.plot(n,T,P,m,tm,n,Method="Informative Prior")
  }
  
  durationcallbackplotadaptive<- function(Targeted_sample_size,Targeted_finish_time_in_months,Method,
                                          Total_subjects_recruited_to_date,Time_to_date_in_months){
    n=Targeted_sample_size
    T=Targeted_finish_time_in_months
    m=Total_subjects_recruited_to_date
    tm=Time_to_date_in_months
    Method=Method
    return <-accrual.T.plot(n,T,P=NULL,m,tm,n,Method)
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
  
  
  
  mgui(accrualcallbackplot,title=c("Subjective Prior","How many subjects will you recruit?"),
       argSlider=list(Your_confidence=c(0,1,0.05)))
  
  
  mgui(durationcallbackplot,title=c("Subjective Prior","How long will it take to reach the targeted sample size?"),
       argSlider=list(Your_confidence=c(0,1,0.05)))
  
  mgui(accrualcallbackplotadaptive,title=c("Adaptive Prior","How many subjects will you recruit?"),
       argOption=list(Method=c("Accelerated Prior","Hedging Prior")) )
  
  mgui(durationcallbackplotadaptive,title=c("Adaptive Prior","How long will it take to reach the targeted sample size?"),
       argOption=list(Method=c("Accelerated Prior","Hedging Prior")) )
  
  mgui(diagexpgui,title=c("Diagnostic","Diagnostic Plots"),
       argFilename=list(w=NULL), callback=diagguiCallback, argOption=list(Header=c("TRUE","FALSE")) )
}
