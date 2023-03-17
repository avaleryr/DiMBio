#Cargar datos
datos <- read.table("C:/Users/Ronny/Documents/tesis/ejecucion/Datos.txt",header=TRUE)
#datos

#Datos Reales
Reales <- data.frame(datos$CBM, datos$NBM, datos$ResAcum, datos$Nmin)
#Reales

#Datos Simulados
Simulados <- data.frame(datos$CBMs, datos$NBMs, datos$ResAcums, datos$Nmins)
#Simulados

#////////
#Log de Reales
logReales <- log10(Reales)
#logRealess

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

a <- mean(sqrt(colSums((Reales-Simulados)^2)))
a

b <- mean(sqrt(colSums((logReales-logSimulados)^2)))
b

