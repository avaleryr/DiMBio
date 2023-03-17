#' This function calculate statistics Univariant
#'
#' @param Reals data frame with real data
#' @param Simulateds, data frame with simulated data
#' @param names_variables variable names vector, optional
#' @return R^2, pearson, %RMSE, t-student, NS, MSEP, RMSEP, MAE, %MAE, U, MC, SC, RC, MV, AIC, bayes entre \code{Reals} and \code{Simulateds}.
#' @export
#'
Univariantes <- function(Reals, Simulateds, names_variables = NULL){
  if((dim(Reals)[1] == dim(Simulateds)[1]) && (dim(Reals)[2] == dim(Simulateds)[2])){
    if(!is.null(names_variables)){
      if(length(names_variables) != dim(Reals)[2]){
        return("Vector size of names_variables must equal the number of variables")
      }
    }
    lon <- dim(Reals)[1]
    n_variable <- dim(Reals)[2]

    ################################### Estadisticos Univariantes ###########################

    for (i in 1:n_variable) {
      if(i==1){
        ve <- sqrt(sum((Reals[[i]] - Simulateds[[i]])^2)/lon)*100/mean(Reals[[i]]) #%RMSE
        ve <- cbind(ve, 1 - (sum((Reals[[i]] - Simulateds[[i]])^2)/sum((Reals[[i]]-mean(Reals[[i]]))^2)))#NS
        ve <- cbind(ve, msep <- (sum((Reals[[i]] - Simulateds[[i]])^2))/lon) #MSEP
        ve <- cbind(ve, sqrt(msep)) #RMSEP
        ve <- cbind(ve, (sum(abs(Reals[[i]] - Simulateds[[i]])))/lon) # MAE
        ve <- cbind(ve, (sum((abs(Reals[[i]] - Simulateds[[i]]))/(abs(Reals[[i]]))))*(100/lon))# %MAE
        ve <- cbind(ve, sqrt(sum((Reals[[i]] - Simulateds[[i]])^2)/lon) / (sqrt(sum((Reals[[i]])^2)/lon) + sqrt(sum((Simulateds[[i]])^2)/lon)))#U
        ve <- cbind(ve, (cor(Reals[[i]], Simulateds[[i]]))^2);   #r^2
        ve <- cbind(ve, pearson <- cor(Reals[[i]], Simulateds[[i]])) #pearson
        ve <- cbind(ve, MV <- (lon / 2) * (log1p(sum((Reals[[i]]-Simulateds[[i]])^2)/lon))) # MV
        ve <- cbind(ve, (-2 * MV)+2)#AIC
        ve <- cbind(ve,((mean(Reals[[i]]) - mean(Simulateds[[i]]))^2)/(sum((Reals[[i]]-Simulateds[[i]])^2)/lon))#MC
        ve <- cbind(ve,((sd(Simulateds[[i]])-(pearson*sd(Reals[[i]])))^2)/(sum((Reals[[i]]-Simulateds[[i]])^2)/lon))#SC
        ve <- cbind(ve,((1 - (pearson)^2)*((var(Reals[[i]]))*((lon-1)/lon)))/(sum((Reals[[i]]-Simulateds[[i]])^2)/lon))#RC
        ve <- cbind(ve, (sum(Reals[[i]] - Simulateds[[i]])/lon)/((sqrt((sum(((Reals[[i]]-Simulateds[[i]])-(sum(Reals[[i]] - Simulateds[[i]])/lon))^2))/(lon-1)))/lon)) # t_student
      }
      else{
        ve <- cbind(ve, sqrt(sum((Reals[[i]]-Simulateds[[i]])^2)/lon)*100/mean(Reals[[i]]))#%RMSE
        ve <- cbind(ve, 1 - (sum((Reals[[i]]-Simulateds[[i]])^2)/sum((Reals[[i]]-mean(Reals[[i]]))^2)))#NS
        ve <- cbind(ve, msep <- (sum((Reals[[i]]-Simulateds[[i]])^2))/lon) #MSEP
        ve <- cbind(ve, sqrt(msep)) #RMSEP
        ve <- cbind(ve, (sum(abs(Reals[[i]] - Simulateds[[i]])))/lon) # MAE
        ve <- cbind(ve, (sum((abs(Reals[[i]]-Simulateds[[i]]))/(abs(Reals[[i]]))))*(100/lon))# %MAE
        ve <- cbind(ve, sqrt(sum((Reals[[i]]-Simulateds[[i]])^2)/lon) / (sqrt(sum((Reals[[i]])^2)/lon) + sqrt(sum((Simulateds[[i]])^2)/lon)))#U
        ve <- cbind(ve, ((cor(Reals[[i]], Simulateds[[i]]))^2))#r^2
        ve <- cbind(ve, pearson <- cor(Reals[[i]], Simulateds[[i]])) #pearson
        ve <- cbind(ve, MV <- (lon / 2) * (log1p(sum((Reals[[i]]-Simulateds[[i]])^2)/lon))) # MV
        ve <- cbind(ve, (-2 * MV)+2)#AIC
        ve <- cbind(ve, ((mean(Reals[[i]])-mean(Simulateds[[i]]))^2)/(sum((Reals[[i]]-Simulateds[[i]])^2)/lon))#MC
        ve <- cbind(ve, ((sd(Simulateds[[i]])-(pearson*sd(Reals[[i]])))^2)/(sum((Reals[[i]]-Simulateds[[i]])^2)/lon))#SC
        ve <- cbind(ve, ((1 - (pearson)^2)*((var(Reals[[i]]))*((lon-1)/lon)))/(sum((Reals[[i]]-Simulateds[[i]])^2)/lon))#RC
        ve <- cbind(ve, (sum(Reals[[i]] - Simulateds[[i]])/lon)/((sqrt((sum(((Reals[[i]]-Simulateds[[i]])-(sum(Reals[[i]] - Simulateds[[i]])/lon))^2))/(lon-1)))/lon)) # t_student

      }

    }

    table_uni <- data.frame(matrix(ve,nrow=n_variable,ncol=15,byrow=T))
    colnames(table_uni ) <- c("%RMSE", "NS","MSEP","RMSEP","MAE","%MAE","U","R^2","pearson","MV","AIC","MC","SC","RC","t-student")
    if(!is.null(names_variables))
      rownames(table_uni ) <- names_variables

    table_uni$bayes <-(table_uni$MV/sum(table_uni$MV))
    return(data.frame(table_uni))
  }else{
    return("Dimention of data frames is different")
  }

}
