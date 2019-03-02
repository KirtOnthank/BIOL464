#' Reduces a object in size to be more easily handled by slower computers
#' 
#' Many of the AcKnowledge export files can be very large and hard for slower computers to handle.  This function will reduce the size of objects to make them easier for slower computers to handle.  Rows of the input object are removed evenly through the entire object to acheive the specified reduction.
#' @param x An object containing data file from AcqKnowledge which has already had time added by addtime function
#' @param per The percentage of the original object you would like the resulting object to be.  For example, using per=10 will result in an object that is 10\% the size of the original.
#' @return An object reduced to the specified percentage of the original object
#' @export

shrink <-
  function(x, per=50){
    out=x[seq(from=1,to=nrow(x),length.out=round(nrow(x)*(per/100))),]
    return(out)
  }