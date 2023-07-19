
library(DIMBIO)
library(readxl)

datos <- read_excel("Data.xlsx")

reales<-data.frame(datos$CBM, datos$NBM, datos$ResAcum, datos$Nmin)


simulados <-data.frame(datos$CBMs, datos$NBMs, datos$ResAcums, datos$Nmins)


logReales<-log10(reales)
logReales
logSimulados<-log10(simulados)
logSimulados

Manhattan(logReales, logSimulados)

Euclidiana(logReales, logSimulados)
Mahalanobis(logReales, logSimulados)
nombres <-c("CBM", "NBM", "ResAcum", "Nmin")
Univariantes(reales, simulados, nombres)
