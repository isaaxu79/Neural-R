library(MASS); library(neuralnet); library(ggplot2)
# DATOS
# -----------------------------------------------------
cat("\014")  
datos    <- read.table("prueba_con.csv",header=TRUE, sep=",")
datos$exito=0
datos[datos$tasa <= datos$tasa_esperada,]$exito = 1
n        <- nrow(datos)
muestra  <- sample(n, n * .75)
train    <- datos[muestra, ]
test     <- datos[-muestra, ]

train    <- train[c(2,3,5,6,8,10)]
datos    <- datos[c(2,3,5,6,8,10)]

# NORMALIZACION DE VARIABLES
# -----------------------------------------------------
maxs      <- apply(train, 2, max)
mins      <- apply(train, 2, min)
ax <- maxs-mins
datos_nrm <- as.data.frame(scale(datos, center=mins, scale=ax))
train_nrm <- datos_nrm[muestra, ]
test_nrm  <- datos_nrm[-muestra, ]
#test_nrm$exito  <- 0
test_nrm <- subset( test_nrm, select = -c(exito) ) #Elimina el campo exito



# MODELO
# -----------------------------------------------------
modelo.nn <- neuralnet(exito~nave+alimento+mes+tasa+tipo_alimento,
                       data          = train_nrm,
                       hidden        = c(7,5), # ver Notas para detalle 
                       threshold     = 0.0001,   # ver Notas para detalle
                       algorithm     = "rprop+", # refiere al algoritmo "Resilient Backpropagation", que actualiza los pesos considerando únicamente el signo del cambio, es decir, si el cambio del error es en aumento (+) o disminución (-) entre una iteración y otra.
                       act.fct="logistic", 
                       linear.output=F)


# PREDICCION
# -----------------------------------------------------
pr.nn   <- compute(modelo.nn,test_nrm)

# se transoforma el valor escalar al valor nominal original
medv.predict <- pr.nn$net.result*(max(datos$exito)-min(datos$exito))+min(datos$exito)
sino = ifelse(medv.predict>0.5,1,0)
medv.real    <- (test_nrm$exito)*(max(datos$exito)-min(datos$exito))+min(datos$exito)

View(sino)
View(medv.real)

# SUMA DE ERROR CUADRATICO
# -----------------------------------------------------
(se.nn <- sum((medv.real - sino)^2)/nrow(test_nrm))


#GRAFICOS
# -----------------------------------------------------
# Errores
qplot(x=medv.real, y=sino, geom=c("point","smooth"), method="lm", 
      main=paste("Real Vs Prediccion. Summa de Error Cuadratico=", round(se.nn,2)))
# Red
plot(modelo.nn)