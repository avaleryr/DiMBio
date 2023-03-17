#Cargar datos
datos <- read.table("C:/Users/Ronny/Documents/tesis/ejecucion/Datos.txt",header=TRUE)
#datos

#Datos Reales
Reales <- data.frame(datos$CBM, datos$NBM, datos$ResAcum, datos$Nmin)
#Reales

#Datos Simulados
Simulados <- data.frame(datos$CBMs, datos$NBMs, datos$ResAcums, datos$Nmins)
#Simulados

#promedio datos reales
proReales <- colMeans(Reales)
#proReales

#promedio datos Simulados
proSimulados <- colMeans(Simulados)
#proSimulados


#########################################################
#########################################################
#////////
#Log de Reales
logReales <- log10(Reales)
#logReales

#promedio de log de datos Reales
prologReales <- c(colMeans(logReales))
#prologReales

#logReales - prologReales
restaRealeslog <- sweep(logReales,2,prologReales)
#restaRealeslog

#///////
#Log de Simulados
logSimulados <- log10(Simulados)
#logSimulados

#promedio de log de datos Simulados
prologSimulados <- c(colMeans(logSimulados))
#prologSimulados

#logReales - prologSimulados
restaSimuladoslog <- sweep(logSimulados,2,prologSimulados)
#restaSimuladoslog

#Experimetal Varianza and Covarianza
for(i in 1:dim(restaRealeslog)[2]){
  for(j in 1:dim(restaRealeslog)[2]){
    if(i == j) {
      if(i == 1 & j == 1){
        aux <- var(restaRealeslog[i])
        ve <- aux
      }else{
        aux <- var(restaRealeslog[i])
        ve <- cbind(ve,aux)
      }
    }else{
      aux <- cov(restaRealeslog[i], restaRealeslog[j])
      ve <- cbind(ve, aux)
    }
  }
}

exp_var_cov <- matrix(ve,nrow=4,byrow=T)
#exp_var_cov

#Simulado Varianza and Covarianza
for(i in 1:dim(restaSimuladoslog)[2]){
  for(j in 1:dim(restaSimuladoslog)[2]){
    if(i == j) {
      if(i == 1 & j == 1){
        aux <- var(restaSimuladoslog[i])
        ve <- aux
      }else{
        aux <- var(restaSimuladoslog[i])
        ve <- cbind(ve,aux)
      }
    }else{
      aux <- cov(restaSimuladoslog[i], restaSimuladoslog[j])
      ve <- cbind(ve, aux)
    }
  }
}

sim_var_cov <- matrix(ve,nrow=4,byrow=T)
#sim_var_cov

#Promedio Experimental Simulado
pro_exp_sim <- (exp_var_cov + sim_var_cov)/2
#pro_exp_sim

#matrix Inversa
mat_inv <- solve(pro_exp_sim)
#mat_inv

#Resta de promedios Experimental, Simulado
subpro <- matrix(prologReales - prologSimulados)
#subpro

#Primer paso de la multiplicacion
mahalanobis <- sqrt((t(subpro) %*% mat_inv) %*% subpro)
as.numeric(mahalanobis)

