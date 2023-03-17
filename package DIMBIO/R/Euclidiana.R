#' This function calculate the distance euclidiana
#'
#' @param Reals data frame with real data
#' @param Simulateds, data frame with simulated data
#' @return Distance euclidiana between \code{Reals} and \code{Simulateds}.
#' @export
Euclidiana <- function(Reals, Simulateds){
  if((dim(Reals)[1] == dim(Simulateds)[1]) && (dim(Reals)[2] == dim(Simulateds)[2])){
    euclidiana <- mean(sqrt(colSums((Reals-Simulateds)^2)))
    return(euclidiana)
  }else{
    return("Dimention of data frames is different")
  }
}



