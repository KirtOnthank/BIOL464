#' Read Data Files Exported from BioPac MP160
#' 
#' Will read the output file from the MP160 logger software into R as a dataframe.
#' @param file File to be read
#' @return Data will be read in, a column for time is added. Header information will be discarded.
#' @export

read.mp160=
  function(file){
  con=file(file)
  open(con)
  thing=readLines(con,n=15)
  close(con)
  tim=thing[2]
  chan=nchar(thing[grep("CH",thing)])/4
  chan.list=substr(thing[grep("CH",thing)],1,3)
  if (chan>1){
    for (i in 2:chan){
      chan.list[i]=substr(thing[grep("CH",thing)],(i-1)*4+1,(i-1)*4+3)
    }
  }
  start=grep("CH",thing)+2
  dura=as.numeric(gsub('(\\d+\\.\\d+) msec/sample','\\1',tim))/1000
  volt=read.table(file,skip=start)
  volt.time=seq(from=0,to=(length(volt[,1])-1)*dura,by=dura)
  volt=cbind(volt.time,volt)
  colnames(volt)=c("time",chan.list)
  return(volt)
}
