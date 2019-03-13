#' Determine heart rate
#' 
#' Find heart rate from a ecg in BIOPAC AcqKnowledge file format.
#' @param x A vector of voltages of an ecg from AcqKnowledge
#' @param t Specifies how long the entire file is in seconds. Default is 30 seconds.
#' @return a single integer, the average heart rate in bpm, will be returned
#' @export

findbpm=function(x,t=30,bounds=c(0,10000)){
  temp1=length(x)*9
  x=x-mean(x)
  padded=c(x, seq(0, 0, length=temp1))
  little=(bounds/(60/t))*10
  spec=spectrum(padded)$spec
  spec.little=spec[little[1]:little[2]]
  bpm=((which.max(spec.little)+little[1])/10)*(60/t)
  return(paste(bpm, "Beats Per Minute"))
}

