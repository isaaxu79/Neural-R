# LIBRERIAS Y DATOS
# -----------------------------------------------------
library(MASS); library(neuralnet); library(ggplot2); library(gridExtra);

datos    <- read.table("prueba_con.csv",header=TRUE, sep=",")
datos$exito=0
datos[datos$tasa <= datos$tasa_esperada,]$exito = 1
datos    <- datos[c(2,3,5,6,10)]
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
                       threshold     = 0.0029,   # ver Notas para detalle
                       act.fct="logistic")


# PREDICCION
# -----------------------------------------------------
pr.nn   <- compute(modelo.nn,within(test_nrm,rm(exito))) # rm elimina la columna seleccionada

# se transoforma el valor escalar al valor nominal original
medv.predict <- pr.nn$net.result*(max(datos$exito)-min(datos$exito))+min(datos$exito)
sino = ifelse(medv.predict>0.5,1,0)
medv.real    <- (test_nrm$exito)*(max(datos$exito)-min(datos$exito))+min(datos$exito)

das <- data.frame(prediccion=sino, real=medv.real)
test$prediccion = sino


plot(modelo.nn)
p1 <- ggplot(test, aes(x=factor(mes), y=factor(nave), group=exito, colour=factor(exito))) + geom_point() + labs(title="Cumpliento esperado de la tasa de crecimiento por nave y por mes",
                                                                                                                  y="Nave",x="Meses",color="Nave")
p2 <- ggplot(test, aes(x=factor(mes), y=factor(nave), group=prediccion, colour=factor(prediccion))) + geom_point() + labs(title="Prediccion del cumpliento de la tasa de crecimiento nave y por mes",
                                                                                                                    y="Nave",x="Meses",color="Nave")
grid.arrange(p1, p2, ncol=2)