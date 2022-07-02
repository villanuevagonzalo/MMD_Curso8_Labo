#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")

setwd("A:/20. Formación Academica/02. Mineria de Datos/01. Cursos/08. Aplicaciones de Minería de Datos a Economía y Finanzas/MMD_Curso8_Labo")  #Establezco el Working Directory

 #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./datasets/paquete_premium_202011.csv")


#uso esta semilla para los canaritos
set.seed(430007)

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 1:100 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


#Primero  veo como quedan mis arboles
modelo  <- rpart(formula= "clase_ternaria ~ . ",
                 data= dataset[,],
                 model= TRUE,
                 xval= 0,
                 cp= -1, 
                 minsplit= 0, 
                 minbucket= 500, 
                 maxdepth= 6)

#creo la carepta donde guardo el resultado
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/ST5210/", showWarnings = FALSE )

setwd("A:/20. Formación Academica/02. Mineria de Datos/01. Cursos/08. Aplicaciones de Minería de Datos a Economía y Finanzas/MMD_Curso8_Labo/exp/ST5210")  #Establezco el Working Directory DEL EXPERIMENTO

#genero la imagen del arbol
pdf( file= "arbol_canaritos100.pdf", width=20, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
