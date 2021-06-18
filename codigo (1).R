
library(readxl)
library(digest)
library(haven)
library(dplyr)
library(VIM)
library(naniar)
library(smoothmest)
library(fastDummies)

#Ejericio 1
#Importo el fichero y eval?o y corrijo los formatos que no me cuadran
DF <- read_excel("SBAnational.xlsx")
str(DF)
names(DF)

#corrijo formatos
#formatos de num?rico a integer
DF[,c(10,11,12,14,15)] <- apply(DF[,c(10,11,12,14,15)], 2, as.integer)

#de lo que sea a factor
#factor: Los factores son la forma en la que R guarda las variables categoricas 
for (i in c(4,5,7,8,13,16,17,18,19,24)) {DF[,i]<-as_factor(DF[,i])}

#el identificador no puede ser un n?mero xque en ning?n caso debemos operar con ?l
DF[,1] <- as.character(DF$LoanNr_ChkDgt)
DF$LoanNr_ChkDgt
#de POSIXct a Date
DF[,c(9,20,21)] <- apply(DF[,c(9,20,21)], 2, as.Date)
#las columnas 22, 23, 25, 26 y 27 son num?ricas y se quedan como num?ricas
#las columnas 2, 3 y 6 son character y las mantengo como character

#Ejercicio 2
#anonimizaci?n
DF <- as.data.frame(mutate(DF, Name=sapply(Name, digest, c("md5"))))
str(DF)
#la variable a anonimizar es Name, que el identificador de la empresa que pide el pr?stamo
#escojo este m?todo por de los que hemos estudiados es el que el formato
#de anonimizaci?n es el m?s ?ptimo (aunque no el m?s seguro)
#adem?s, cuido que me sustituya completamente la variable a anonimizar
#puesto que en otro caso estar?a pseudoanonimizando

#data discovering
#quito duplicados
DF <- DF[!duplicated(DF), ] #no hay
DF
#empiezo por los dominios de las variables
#los identificadores no los tengo que analizar: columnas 1, 2, 3 y 6
#variables factores
tabla_frecuencias <- function(vector) {
aux <- as.data.frame(table(vector))
return(aux)
}
frec_State <- tabla_frecuencias(DF$State)
frec_Zip <- tabla_frecuencias(DF$Zip)
frec_BankState <- tabla_frecuencias(DF$BankState)
frec_NAICS <- tabla_frecuencias(DF$NAICS)
frec_NewExist <- tabla_frecuencias(DF$NewExist)
frec_FranchiseCode <- tabla_frecuencias(DF$FranchiseCode)
frec_UrbanRural <- tabla_frecuencias(DF$UrbanRural)
frec_RevLineCr <- tabla_frecuencias(DF$RevLineCr)
frec_LowDoc <- tabla_frecuencias(DF$LowDoc)
frec_Status <- tabla_frecuencias(DF$MIS_Status)

#las variables para las que no tengo mapeo, no puedo revisarlas
#corrijo las variables FranchiseCode y LowDoc xque no tienen 
# bien informados algunos valores
#los que puedo cambiar, los cambios. Los que no, los elimino
DF$RevLineCr[DF$RevLineCr == "1"] <- "Y"
DF$RevLineCr[DF$RevLineCr == "0"] <- "N"
DF$LowDoc[DF$LowDoc == "1"] <- "Y"
DF$LowDoc[DF$LowDoc == "0"] <- "N"
DF$Zip[DF$Zip == "0"] <- ""
DF$NAICS[DF$NAICS == "0"] <- ""

DF <- DF[(DF$LowDoc=="Y" | DF$LowDoc=="N") & (DF$RevLineCr=="Y" | DF$RevLineCr=="N") & (DF$NewExist=="1" | DF$NewExist=="2") ,]

#variables continuas
numeric <- as.data.frame(select_if(DF, is.numeric))
desc_numeric <- as.data.frame(summary(numeric))

#auencias
ausencias <- as.data.frame(miss_var_summary(DF))
ausencias_casos <- miss_case_table(DF)
#la variable que m?s ausencias presenta es ChgOffDate, pero no la puedo 
#eliminar xque son ausencias ficticias, son pr?stamos sin impago

#por ?ltimo, visualizo los outliers para las variables num?ricas
#salvo el a?o de la concesi?n del pr?stamo, que 
#a pesar de ser integer, con analizar el rango es suficiente
boxplot(numeric[-c(1)], las = 2)
#como se ve claro que hay rango de valores distintos
#divido las visualizaciones
boxplot(numeric[c(1,2,3,4)], las = 2)
boxplot(numeric[c(5,7,8,9)], las = 2)
boxplot(numeric[c(14)], las = 2)
#el outlier que presenta la variable ChgOffPrinGr tiene
#sentido y se complementa con las ausencias de ChgOffDate
#xque solo hay un impago
#de lo dem?s, me llama la atenci?n una empresa en la variable
#GrAppv, pero la reviso es una empresa grande en general

#Ejercicio 4: imputaciones
#imputaciones valores ausentes variables num?ricas
DF <- kNN(DF, variable=colnames(DF[c(22,23,27,25,26)]), dist_var=colnames(DF[c(12,13,14,15,17,18,19)]))
miss_var_summary(DF[c(22,23,27,25,26)])

#Ejercicio 5: acotaciones
#acoto variables continuas
acotacion <- function(var) {
  var <- ifelse(var<(mean(var)-3*sd(var)),(mean(var)-3*sd(var)),ifelse(var>(mean(var)+3*sd(var)), (mean(var)+3*sd(var)), var))
  return(var)}

DF[,c(22,23,25,26,27)] <- apply (DF[,c(22,23,25,26,27)], 1, acotacion)
summary(DF[,c(22,23,25,26,27)])

#Ejercicio 6
#anonimizaci?n consulta
epsilon <- 0.1
max <- max(DF[c(23)])
min <- min(DF[c(23)])
n <- nrow(DF)
gs <- (max-min)/n


consulta <- DF %>%
  group_by(Zip) %>%
  summarize(mean(BalanceGross))

consulta_anonimizada <- cbind(consulta[,1], rdoublex(16503, consulta[,2], gs/epsilon))

#Ejercicio 7
#genero dummies
any_na(DF[,17])
levels(DF[,17])
#los missings los imputo a valores 0 que equivale a desconocido
DF$UrbanRural[is.na(DF[17])==TRUE] <- "0"
dummies <- dummy_cols(DF[,17])[-c(1,2)]
names(dummies) <- c("Urban", "Rural")
DF <- cbind(DF[-c(17)], dummies)
#solo hacen falta 2 variables dummies para no perder informaci?n
