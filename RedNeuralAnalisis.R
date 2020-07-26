# LIBRERIAS Y DATOS
# -----------------------------------------------------
library(MASS); library(neuralnet); library(ggplot2); library(gridExtra);

datos    <- read.table("prueba_con.csv",header=TRUE, sep=",")
datos$exito=0
datos[datos$tasa <= datos$tasa_esperada,]$exito = 1
datos[datos$tipo_alimento==1,]$tipo_alimento = 1.92
datos[datos$tipo_alimento==2,]$tipo_alimento = 1.75
datos[datos$tipo_alimento==3,]$tipo_alimento = 1.80

datos    <- datos[c(2,3,5,6,8,10)]
n        <- nrow(datos)
muestra  <- sample(n, n * .70)
train    <- datos[muestra, ]
test     <- datos[-muestra, ]

# NORMALIZACION DE VARIABLES
# -----------------------------------------------------
maxs      <- apply(train, 2, max)
mins      <- apply(train, 2, min)
datos_nrm <- as.data.frame(scale(datos, center = mins, scale = maxs - mins))
train_nrm <- datos_nrm[muestra, ]
test_nrm  <- datos_nrm[-muestra, ]

# FORMULA
# -----------------------------------------------------
nms  <- names(train_nrm)
frml <- as.formula(paste("exito ~", paste(nms[!nms %in% "exito"], collapse = " + ")))


# MODELO
# -----------------------------------------------------
modelo.nn <- neuralnet(frml,
                       data          = train_nrm,
                       hidden        = c(7,5), # ver Notas para detalle 
                       threshold     = 0.001,   # ver Notas para detalle
                       algorithm     = "rprop+")

View(modelo.nn$result.matrix)
# PREDICCION
# -----------------------------------------------------
pr.nn   <- compute(modelo.nn,within(test_nrm,rm(exito),rm(tasa))) # rm elimina la columna seleccionada

# se transoforma el valor escalar al valor nominal original
medv.predict <- pr.nn$net.result*(max(datos$exito)-min(datos$exito))+min(datos$exito)
sino = ifelse(medv.predict>0.5,1,0)
medv.real    <- (test_nrm$exito)*(max(datos$exito)-min(datos$exito))+min(datos$exito)

das <- data.frame(prediccion=sino, real=medv.real)
test$prediccion = sino

contV <- 0
contf <- 0

for(x in 1:length(sino)){
  print(medv.real[x])
  if(sino[x] == medv.real[x]){
    contV = contV+1
  }else {
    contf = contf+1
  }
}
contV = (contV*100)/26
print("porcetaje de exito:")
print(contV)

plot(modelo.nn)
p1 <- ggplot(test, aes(x=factor(mes), y=factor(nave), group=exito, colour=factor(exito))) + geom_point() + labs(title="Cumpliento esperado de la tasa de crecimiento por nave y por mes",
                                                                                                                  y="Nave",x="Meses",color="Nave")
p2 <- ggplot(test, aes(x=factor(mes), y=factor(nave), group=prediccion, colour=factor(prediccion))) + geom_point() + labs(title="Prediccion del cumpliento de la tasa de crecimiento nave y por mes",
                                                                                                                    y="Nave",x="Meses",color="Nave")
grid.arrange(p1, p2, ncol=2)