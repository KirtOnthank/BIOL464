#' Add time to BIOPAC AcqKnowledge files
#' 
#' Adds a field for time that has been dropped when AcqKnowledge exports a .TXT file
#' @param x An object containing data file from AcqKnowledge
#' @param time Specifies how long the entire file is in seconds. Default is 60 seconds.
#' @return The original object is returned with a new field for time
#' @export
addtime <-
  function(x,time=60){
    temp=seq(from=0,to=time,length.out=length(x[,1]))
    return(cbind(temp,x))
  }
