#' This function calculate the distance Manhattan
#'
#' @param Reals data frame with real data
#' @param Simulateds, data frame with simulated data
#' @return Distance Manhattan between \code{Reals} and \code{Simulateds}.
#' @export
Manhattan <- function(Reals, Simulateds){
  if((dim(Reals)[1] == dim(Simulateds)[1]) && (dim(Reals)[2] == dim(Simulateds)[2])){
    manhattan <- mean(colSums(abs(Reals-Simulateds)))
    return(manhattan)
  }else{
    return("Dimention of data frames is different")
  }
}
