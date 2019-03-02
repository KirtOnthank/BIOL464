#' Trims BIOPAC AcqKnowledge files to only include heart rate
#' 
#' AcqKnowledge exports all the voltage data from the electrodes when recording a heart rate, which is often more than what we want. This function is used to trim the data to only include heart rate.
#' @param x An object containing data file from AcqKnowledge which has already had time added by addtime function
#' @return An object containining only time and heart rate at each heart beat is returned
#' @export

onlybpm <-
  function(x){
    time1=x[1:length(x[,1])-1,4]
    time2=x[2:length(x[,1]),4]
    out=x[c(0,time2-time1)!=0,c(1,4)]
    colnames(out)=c("time","bpm")
    return(out)
  }