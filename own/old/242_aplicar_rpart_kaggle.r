rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui debe cambiar los parametros por los que desea probar

param_basicos  <- list( "cp"=          -0.424328867,  #complejidad minima
                        "minsplit"=   2283,     #minima cantidad de registros en un nodo para hacer el split
                        "minbucket"=  1041,     #minima cantidad de registros en una hoja
                        "maxdepth"=     18 )    #profundidad máxima del arbol

setwd("A:/20. Formación Academica/02. Mineria de Datos/01. Cursos/08. Aplicaciones de Minería de Datos a Economía y Finanzas/MMD_Curso8_Labo")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./dataset/paquete_premium_202011.csv")

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart("clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data = dtrain,
                 xval=0,
                 control=  param_basicos )

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#Ahora aplico al modelo  a los datos de 202101  y genero la salida para kaggle

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./dataset/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, dapply , type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/60
dapply[ , Predicted  := as.numeric(prob_baja2 > 1/60) ]

#genero un dataset con las dos columnas que me interesan
entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./exp/", showWarnings = FALSE  )
dir.create( "./exp/KA2022/", showWarnings = FALSE  )

fwrite( entrega, 
        file= "./exp/KA2022/K242_001.csv", 
        sep= "," )
