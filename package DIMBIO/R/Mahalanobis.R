#' This function calculate the distance Mahalabonis
#'
#' @param Reals data frame with real data
#' @param Simulateds, data frame with simulated data
#' @return Distance Mahalabonis between \code{Reals} and \code{Simulateds}.
#' @export

Mahalanobis <- function(Reals, Simulateds){

  if((dim(Reals)[1] == dim(Simulateds)[1]) && (dim(Reals)[2] == dim(Simulateds)[2])){
    lon <- dim(Reals)[2]

    proReals <- c(colMeans(Reals))

    restaReals <- sweep(Reals,2,proReals)
    proSimulateds <- c(colMeans(Simulateds))
    restaSimulateds <- sweep(Simulateds,2,proSimulateds)
    for(i in 1:dim(restaReals)[2]){
      for(j in 1:dim(restaReals)[2]){
        if(i == j) {
          if(i == 1 & j == 1){
            aux <- var(restaReals[i])
            ve <- aux
          }else{
            aux <- var(restaReals[i])
            ve <- cbind(ve,aux)
          }
        }else{
          aux <- cov(restaReals[i], restaReals[j])
          ve <- cbind(ve, aux)
        }
      }
    }

    exp_var_cov <- matrix(ve,nrow=lon,byrow=T)
    for(i in 1:dim(restaSimulateds)[2]){
      for(j in 1:dim(restaSimulateds)[2]){
        if(i == j) {
          if(i == 1 & j == 1){
            aux <- var(restaSimulateds[i])
            ve <- aux
          }else{
            aux <- var(restaSimulateds[i])
            ve <- cbind(ve,aux)
          }
        }else{
          aux <- cov(restaSimulateds[i], restaSimulateds[j])
          ve <- cbind(ve, aux)
        }
      }
    }

    sim_var_cov <- matrix(ve,nrow=lon,byrow=T)
    pro_exp_sim <- (exp_var_cov + sim_var_cov)/2
    mat_inv <- solve(pro_exp_sim)
    subpro <- matrix(proReals - proSimulateds)
    mahalanobis <- sqrt((t(subpro) %*% mat_inv) %*% subpro)
    return(as.numeric(mahalanobis))
  }else{
    return("Dimention of data frames is different")
  }

}
