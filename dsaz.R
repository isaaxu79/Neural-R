library(readxl)
library(dplyr)
library(tidyr) 
library(data.table)
library(ggplot2)
library(writexl)
library(neuralnet)
require(gridExtra) 
#
## Lectura de datos
#
prueba <- read.table("prueba_con.csv",header=TRUE, sep=",")
#temperatura <- read_excel("Climatux.xlsx")
entrena <- read.table('medidas_meses_entrenamiento.csv',header=TRUE, sep=",")


#Normalizacion


#entrenamiento de la red

prueba

# MODELO
# -----------------------------------------------------
modelo.nn <- neuralnet(exito~nave+alimento+mes+tasa+tipo_alimento, #variables a implementar
                       data          = entrena,
                       hidden        = c(7,5), #  especifica una primera capa oculta con 7 neuronas y una segunda capa oculta con 5 neuronas. 
                       threshold     = 0.0001,   #  indica que las iteraciones se detendran cuando el "Cambio" del error sea menor a 5% entre una iteracion de optimizacion y otra. Este "Cambio" es calculado como la derivada parcial de la funcion de error respecto a los pesos.
                       algorithm     = "rprop+", # refiere al algoritmo "Resilient Backpropagation", que actualiza los pesos considerando únicamente el signo del cambio, es decir, si el cambio del error es en aumento (+) o disminución (-) entre una iteración y otra.
                       act.fct="logistic", 
                       linear.output=F
                       ) #Fin

#rn = neuralnet(exito~nave+alimento+mes+tasa+tipo_alimento, data=entrena, hidden=c(10,5), act.fct="logistic", linear.output=F)

#modelo.nn



# PREDICCION
# -----------------------------------------------------
rnprediction = compute(modelo.nn, prueba)
#rnprediction = compute(modelo.nn,within(prueba,rm(tasa)))

#desnormalizacion



a = rnprediction$net.result
sino = ifelse(a>0.5,1,0)
prueba$exito = sino
prueba$exito2=0
prueba[prueba$tasa <= prueba$tasa_esperada,]$exito2 = 1

#Grafica

#RED
plot(modelo.nn)

prueba

p1 <- ggplot(prueba, aes(x=factor(mes), y=factor(nave), group=exito, colour=factor(exito))) + geom_point() + labs(title="Cumpliento de la tasa de crecimiento por nave y por mes",
                                                                                                            y="Nave",x="Meses",color="Nave")
p2 <- ggplot(prueba, aes(x=factor(mes), y=factor(nave), group=exito2, colour=factor(exito2))) + geom_point() + labs(title="Prediccion del cumpliento de la tasa de crecimiento nave y por mes",
                                                                                                            y="Nave",x="Meses",color="Nave")
grid.arrange(p1, p2, ncol=2)