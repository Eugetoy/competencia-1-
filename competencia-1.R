# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
install.packages( "data.table", dependencies= TRUE )
library("data.table")
install.packages('rpart.plot')
library(rpart.plot)
install.packages ('rpart')
install.packages("ggplot2")
install.packages("dplyr")
library(rpart)
library(ggplot2)
library(dplyr)

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Euge\\OneDrive\\Escritorio\\EspecilidadDM\\economiayfinanzas")
# Poner sus semillas
semillas <- c(469363,502133,621923,704017,839369)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset01 <- dataset[foto_mes == 202101]
dataset03 = dataset[foto_mes == 202103]


# Numero de nulos en variable Visa_fechaalta resto todo 0 los null
print(sum(is.na(dataset$Visa_fechaalta)))     # con ceros
print(sum(is.na(dataset$Visa_mlimitecompra))) #7894  con la media
print(sum(is.na(dataset$Master_mlimitecompra)))#16847 con la media 

# Imputamos los nulos de nuestra variable con ceros s puede probar con -1 con mediana, u otra cosa
#dataset[, Visa_fechaalta := ifelse(is.na(Visa_fechaalta), 
#                                    0,
#                                    Visa_fechaalta)]

#estadísticas de ambas variables
summary(dataset$Visa_fechaalta)

# CAlculo la media de $Master_mlimitecompra y Visa_mlimitecompra
mean_Master_mlimitecompra = mean(dataset$Master_mlimitecompra, na.rm = T)

mean_Visa_mlimitecompra = mean(dataset$Visa_mlimitecompra, na.rm = T)

mean_Visa_fechaalta = mean(dataset$Visa_fechaalta, na.rm = T)


# Imputamos los nulos de nuestra variable con la media

dataset[, Master_mlimitecompra := ifelse(is.na(Master_mlimitecompra), 
                                      mean_Master_mlimitecompra,
                                      Master_mlimitecompra)] 

dataset[, Visa_mlimitecompra := ifelse(is.na(Visa_mlimitecompra), 
                                       mean_Visa_mlimitecompra,
                                       Visa_mlimitecompra)] 

dataset[, Visa_fechaalta := ifelse(is.na(Visa_fechaalta), 
                                   mean_Visa_fechaalta,
                                   Visa_fechaalta)] 

#-----------------------------
#Feauter engineering
#--------------------------

dataset[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
dataset[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]

dataset[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
dataset[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]

dataset[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
dataset[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]

dataset[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
dataset[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]

## ---------------------------
## Step 5: Outliers
## ---------------------------

# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dataset, aes(x=ccomisiones_otras )) + geom_boxplot()
ggplot(dataset, aes(x=ccomisiones_mantenimiento)) + geom_boxplot()

## ---------------------------
## Step 6: Outliers - Luchando 
## ---------------------------

# Reduzcamos la enorme disperción usando un logaritmo
dataset[, ctrx_quarter := log(ctrx_quarter + 1)]
summary(dataset$ctrx_quarter)    

dataset[, active_quarter := log(active_quarter  + 1)]
summary(dataset$active_quarter)   

## ---------------------------
## Step 7: Outliers - Una más y no jodemos más 
## ---------------------------

library(dplyr)
dataset[, ctrx_quarter := ntile(ctrx_quarter, 10)]
dataset[, mprestamos_personales := ntile(mprestamos_personales, 10)]
dataset[, mcuentas_saldo := ntile(mcuentas_saldo, 10)]
dataset[, mactivos_margen := ntile(mactivos_margen, 10)]
dataset[, mcaja_ahorro := ntile(mcaja_ahorro, 10)]
dataset[, mcuenta_corriente := ntile(mcuenta_corriente, 10)]
dataset[, mcomisiones_mantenimiento := ntile(mcomisiones_mantenimiento, 10)]
dataset[, mrentabilidad := ntile(mrentabilidad, 10)]
dataset[, mpasivos_margen := ntile(mpasivos_margen, 10)]
dataset[, Visa_mlimitecompra := ntile(Visa_mlimitecompra, 10)]
dataset[, mrentabilidad_annual := ntile(mrentabilidad_annual, 10)]
dataset[, Master_mlimitecompra := ntile(Master_mlimitecompra, 10)]


# Supongamos que tenemos una lista de variables a las que queremos transformar
## NO ME SALIO 
mis_variables <- c("ctrx_quarter",
                   "mprestamos_personales",
                   "mcuentas_saldo",
                   "mactivos_margen",
                   "mcaja_ahorro",
                   "mcuenta_corriente",
                   "ccomisiones_otras","cprestamos_personales",
                   "mcomisiones_mantenimiento","mrentabilidad", "mpasivos_margen",   
                   "Visa_mlimitecompra", "mrentabilidad_annual", 
                   "Master_mlimitecompra","mplazo_fijo_dolares")

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dataset[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
}

## ---------------------------
## Step 10: Embeddings (Caseros)
## ---------------------------

# Hagamos interactuar algunas variables para ver si conseguimos alguna mejor
# forma automática de generar nuevas variables multiplicando. 
nuevas <- c()
for (var1 in mis_variables) {
  for (var2 in mis_variables) {
    if (var1 != var2) {
      nueva <- paste(var1, var2, sep = "___")
      dataset[, (nueva) := get(var1) * get(var2)]
      nuevas <- c(nuevas, nueva)
    }
  }
}

mis_variables_3 <- c(nuevas, mis_variables) 

campos2 <- paste(mis_variables_3, collapse = " + ")
#3formula2 <- paste0( "clase_binaria ~ ", campos2 )


#-----------------------------------------------
#Optimización Bayesiana, sólo para tener los hiperparáemtros 
#------------------------------------------------------

install.packages("smoof")
library(smoof)
install.packages('mlrMBO')
library(mlrMBO)
install.packages('rgenoud')
library(rgenoud)

semillas <- c(469363,502133,621923,704017,839369)

# Nos quedamos solo con el 202101
dataset01 <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset01[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dataset01[, clase_ternaria := NULL]

ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}

modelo_rpart <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  
  ganancia(test_prediccion[, "evento"], test$clase_binaria) / 0.3
}

experimento_rpart <- function(ds, semillas, cp = 0, ms = 20, mb = 1, md = 10){
  gan = c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    r <- modelo_rpart(train , test,   
                      cp = cp, ms = ms, mb = mb, md = md)
    gan = c(gan,r)}
  mean(gan)
}

num_random = sample(0:1, replace= FALSE)
set.seed(semillas[1])

#invoco la función. Agrego minsplit por el número entre 0-1
#FINAL OPT BAYESIANA
obj_fun_md_ms <- function(x) {
  experimento_rpart(dataset01, semillas
                    , md = x$maxdepth
                    , ms = x$minsplit, 
                    mb= floor(x$minsplit*num_random),
                    cp = x$cp ) #minbucket floor q sea numerico
}
#x es la tabla $ la variable

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 4L, upper = 20L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 200L),
    makeIntegerParam("minbucket",  lower = 0L, upper = 1L),
    makeIntegerParam("cp",  lower = -1, upper = 1L)
    
    # makeNumericParam <- para parámetros continuos
  ),
  noisy = TRUE, # lo desbloqueo
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 20L)# Cantidad de iteraciones50
# iters cantidad de iteraciones  
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  #opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)



#----------------------------------------
## Optimización hiperparámetros en general

#dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset 
dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

param  <- list("cp"= -0.5,
               "minsplit"=  123,
               "minbucket"= 440,
               "maxdepth"= 12
)

#genero el modelo
modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param,
                        cp = 0,
                        )

#Aplico el modelo a los datos dapply pidiendo que me devuelva probabildades
prediccion  <- predict( modelo, dapply, type = "prob")

prob_baja2  <- prediccion[, "BAJA+2"]
Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )  # 0.025 es la probabilidad de corte

entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, "Predicted"=Predicted)  )


#creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/K101_011.csv",
        sep=  "," )

