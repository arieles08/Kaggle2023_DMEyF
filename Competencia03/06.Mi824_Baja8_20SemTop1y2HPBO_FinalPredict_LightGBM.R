# limpio la memoria
#rm(list = ls()) # remove all objects
#gc() # garbage collection

require("data.table")
require("lightgbm")

#cambiar lineas: 15, 34,  71, 72, 88, 91, 95, 96, y el nombre 10.1 -> por el que corresponde
#-----------------------------------------------------------------------------------------------------------
#configuración:
source <- "~/buckets/b1/"

data <- "competencia03_FEhist_v2_reduced_pca.csv.gz"

folder_in <- 'BO_Mod06_fhistv2pca' #donde esta la salida de la Bayesiana

folder_out <- 'Semillerio7_Baja4'


#--------------Carga de Parametros------------------------------------
#Elijo meses de entrenamiento y testeo:
# meses donde se entrena el modelo

training <- c(201908, 201909, 201910, 201911, 201912,
              202001, 202002, 202011, 202012, 
              202101, 202102, 202103, 202104, 202105, 202106, 202107)
future <- c(202109) # meses donde se aplica el modelo


#Cantidad de Semillas a utilizar (1 a 20):
k <- 20


#Elijo del Top10 HiperParam con cuales voy a entrenar:
seleccion_topHP <- c(1,3)

#Cantidad N del TopN de Parametros a seleccionar de la BO (Optimización Bayesiana):
topN <- length(seleccion_topHP)


# creo la carpeta donde va a guardar las predicciones
setwd(source) 
dir.create(paste0("./exp/", folder_out, "/"), showWarnings = FALSE)


#---------- Elijo 20 Semillas para correr el mismo modelo -----------------------------------------------------------------------------
# queda guardada en **semillas** y se puede llamar a cada uno como `semillas[1]` ó `semillas[2]`...

#leo la salida de la optimizaciob bayesiana
setwd(source)
nombre  <- "datasets/Semillas1a20_2023.txt"

semillas  <- fread( paste0(source,nombre))
semillas  <- as.numeric(unlist(semillas))


#------------inicia----------------------------------------------------------------------------------------
#------------ Carga el Dataset -----------------------------------------------------------------------------
# Aqui se debe poner la carpeta de la materia de SU computadora local (el Working Directory)
setwd(source) 

#cargo el dataset
dataset <- fread(paste0("./datasets/", data)) 

#check
print(dim(dataset))

#----------------Modifica Dataset: agrega clase binaria y Ganancia-------------------
# Genero CLASE_BINARIA: clase01
dataset[, clase01 := ifelse(clase_gral %in% c("BAJA+8", "BAJA+7", "BAJA+6","BAJA+5","BAJA+4","BAJA+3","BAJA+2", "BAJA+1"), 1L, 0L)]

# Agrego campo de Ganancia_INDIVIDUAL -> es INDEPENDIENTE de la predicción -> conociendo la clase_ternaria real -> ya se puede saber
dataset[  , ganancia :=  ifelse( clase_ternaria == "BAJA+2", 273000, -7000 ) ]

#check
#table(dataset$foto_mes, dataset$clase01 )

#---------- Levanta salida de Optimización Bayesiana: para elegir lo TopN Parametros -----------------------------------------------------------------------------
# queda guardada en top_parametros y se puede llamar a cada uno como top_parametros$top1 ó top_parametros$top2...

#leo la salida de la optimizaciob bayesiana
ruta  <- paste0(source,"exp/",folder_in, "/" ) #"~/buckets/b1/exp/BO_Mod06_fhistv2pca/"
nombre  <- "BO_log.txt"

tb_log  <- fread( paste0(ruta,nombre))

#ordeno por Ganancia --> tener el TopN con mejor ganancia arriba
setorder( tb_log, -ganancia )

#Genero una lista con los Top10 mejores parametros de la BO: 
top_parametros <- list()

for (i in 1:10){
  #elijo ahora por ejemplo el Top1:
  parametros <- as.list(copy(tb_log[i]))
  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL
  
  # Guarda los parámetros en la lista
  top_parametros[[paste0('top', i)]] <- parametros
}

#Me quedo solo con los seleccionados:
top_parametros <- top_parametros[seleccion_topHP]

#como llamo ahora el top1 (podria llamar el top2, top3...top10):
#opcion 1: top_parametros$top1
#opcion 2: top_parametros[[1]]

#---------- Entrenamiento del LightGBM -----------------------------------------------------------------------------
# parametros$seed <- semillas[1] semillas[2] ...
# param = top_parametros$top1 ó top_parametros$top2...

#--------------Preparación previa-------------------------------------
#Genero el Dataset de TRAIN:
# establezco donde entreno:
dataset[, train := 0L]
dataset[foto_mes %in% training, train := 1L]

# establesco donde predigo:
dtest <- dataset[foto_mes %in% future]

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria","clase01","add_mesn","add_last_mes","add_lastn","clase_gral_n","clase_gral", "ganancia"))

# dejo los datos de TRAIN en el formato que necesita LightGBM para Entrenar
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

#---------Recorro TopN parámetos de BO y N Semillas: Entrenamiento -> Predicción -> Feature Importance -> lista_predicciones-------------------------------------

#directorio de trabajo: la carpeta creada en exp
setwd(paste0("./exp/", folder_out, "/"))

#VOY A TENER QUE: generar la prediccion en un dataset que tenga los campos basicos -> aca le agregue ganancia cuando en realidad no necesitamos
tb_prediccion  <- dtest[  , list( numero_de_cliente, foto_mes, clase_ternaria, ganancia ) ]

for( i in 1:topN ){
  #Elijo los parametros del Modelo de la BO que usamos:  
  parametros <- top_parametros[[i]] #-> aca la semilla es la 1ra: 997751 -> despues es necesario cambiarla a la que queremos
  
  for( semilla in semillas[1:k] ){
    #Elijo Semilla y Parametros:
    parametros$seed  <- semilla #-> se cambia la semilla
    
    #ENTRENO el Modelo: genero el modelo con datos de TRAIN
    set.seed( parametros$seed )
    message("Entrenando el modelo")
    timestamp()
    modelo  <- lightgbm( data= dtrain,
                         param=  parametros,
                         verbose= -100 )
    timestamp()
    
    #Aplico el modelo a los datos nuevos
    message("Prediciendo")
    prediccion <- predict(modelo,
                          data.matrix(dtest[, campos_buenos, with = FALSE] ) )
    
    #agrego columnas con la probabilidad de cada predicción de c/semilla --> esto lo tengo que testear a ver si la prediccion que genera es la que le corresponde a ese
    tb_prediccion[ , paste0('prob_top',i,'_', semilla) := prediccion ]
    
    #Semillerio: calculo la probabilidad promedio:
    if(semilla == semillas[k]){
      #Selecciono las columnas con las probabilidades predichas de cada semilla:
      columnas_prob <- colnames(tb_prediccion) 
      columnas_prob <- columnas_prob[grep(paste0('prob_top', i), columnas_prob)]
      
      # Hace ENSAMBLE: Promedio 
      promedio <- apply(tb_prediccion[,..columnas_prob], 1, mean)
      
      #agrego:
      tb_prediccion[, paste0('promedio_top',i) := promedio] # PROMEDIO
    }
    
    #Genero el .txt con el feature importance del modelo de la primera semilla 
    if(semilla == semillas[1]) {
      tb_importancia <- as.data.table(lgb.importance(modelo))
      FIname <- paste0('10.0.Top',i,'_sem',semillas[k],'_FImpo.txt')
      fwrite(tb_importancia,
             file = FIname,
             sep = "\t")   }
    
    #borro y limpio la memoria para la vuelta siguiente del for
    rm( modelo)
    rm(promedio)
    
    timestamp()
    message(paste0("fin semilla ", semilla))
  }
  
  #borro y limpio memoria para el siguiente for
  rm( parametros)
  gc()
  
  message(paste0("fin Top ", i))
}

#EntreCruzamiento: calculo la probabilidad promedio de todas las semillas de DIFERENTES HIPERPARAMETROS:
#Selecciono las columnas con las probabilidades predichas de cada semilla:
columnas_prob <- colnames(tb_prediccion) 
columnas_prob <- columnas_prob[grep('prob_', columnas_prob)]

# Hace ENSAMBLE: Promedio 
promedio <- apply(tb_prediccion[,..columnas_prob], 1, mean)

#agrego:
tb_prediccion[, promedio_todos := promedio] # PROMEDIO


# Guarda la ganancia acumulada en un archivo CSV
fwrite(tb_prediccion, 
       file = 'predicciones_todas.csv', 
       sep = ",") 


#-------------SALIDAS PARA KAGGLE (SI SE PREDICE EN 202109)------------------

if(future == 202109){
  tb_entrega <- tb_prediccion
  
  #Selecciono las columnas con las probabilidades predichas de cada semilla:
  columnas_prob <- colnames(tb_entrega) 
  columnas_prob <- columnas_prob[columnas_prob %like% "(prob)"]
  cols_entrega <- c("numero_de_cliente", "foto_mes", columnas_prob)
  tb_entrega <- tb_prediccion[,..cols_entrega]
  
  # Hace ENSAMBLE: de 2 maneras -> 1) Promedio  y 2) Quantiles
  promedio <- apply(tb_entrega[,..columnas_prob], 1, mean)
  quantil <- apply(tb_entrega[,..columnas_prob], 1, quantile, probs = 0.75, na.rm = TRUE)
  
  tb_entrega[, promedio_todas := promedio] # PROMEDIO
  tb_entrega[, quantil75_todas := quantil] # QUANTIL 75
  
  # Ordena por probabilidad descendente
  setorder(tb_entrega, -promedio_todas)
  
  # Guarda el archivo final con las nuevas columnas
  fwrite(tb_entrega, 
         file = "prediccion_ensamble_kaggle.csv", 
         sep = ",")
  
  
  # Genera archivos con los cortes para Kaggle (promedio)
  cortes <- seq(8000, 15000, by = 500)
  for (envios in cortes) {
    tb_entrega[, Predicted := 0L]
    tb_entrega[1:envios, Predicted := 1L]
    
    fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
           file = paste0(folder_out, "_promedio_", envios, ".csv"),
           sep = ","  ) }
  
  
  # Ordena por probabilidad descendente para el quantil 75
  setorder(tb_entrega, -quantil75_todas)
  
  # Genera archivos con los cortes para Kaggle (quantil 75)
  cortes <- seq(8000, 15000, by = 500)
  for (envios in cortes) {
    tb_entrega[, Predicted := 0L]
    tb_entrega[1:envios, Predicted := 1L]
    
    fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
           file = paste0(folder_out, "_quantil75_", envios, ".csv"),
           sep = ","  ) }
  
  cat("\n\nLa generación de los archivos para Kaggle ha terminado\n")
}


#-----------------------------------------------------------------------------------------------------------------
#-------------------- CÁLCULO de GANANCIA (SI SE PREDICE EN O ANTES DE 202107) -----------------------------------
#-----------------------------------------------------------------------------------------------------------------

if(future <= 202107){
  
  # Crea un nuevo data.table para almacenar los resultados -> Unico valor de Ganancia (promedio de la meseta) para cada modelo
  resultados <- data.table(hyperparam = integer(), seed = integer(), ganancia = numeric(), pos_optima = integer(), semillerio = integer())
  
  
  for (i in 1:topN) {
    message(paste0("inicia calculo ganancias Top ", i))
    timestamp()
    
    semillerio <- 0 #es semillerio?
    
    for (semilla in semillas[1:k]) {
      # Ordena por probabilidad descendente -> uso order porque es de data.table y asi funciona (setorder: NO funciona)
      tb_prediccion <- tb_prediccion[order(-tb_prediccion[[paste0('prob_top', i, '_', semilla)]])]
      
      #Agrega la posición ordenada o rank:
      tb_prediccion[ , paste0('pos_top',i,'_', semilla) := .I ]
      
      # Calcula la ganancia acumulada
      tb_prediccion[, gan_acum := cumsum(ganancia)]
      
      # Encuentra el máximo de la ganancia suavizada y su posicion (punto de corte optimo y su único valor de ganancia)
      max_gan <- max(tb_prediccion[, gan_acum])
      pos_optima <- which.max(tb_prediccion[, gan_acum] == max_gan)
      
      # Almacena los resultados en el nuevo data.table
      resultados <- rbind(resultados, data.table(hyperparam = i, seed = semilla, ganancia = max_gan, pos_optima = pos_optima, semillerio = semillerio))
      
      
      #limpia memoria:
      rm( max_gan)
      rm(pos_optima)
      tb_prediccion[, gan_acum := NULL]
      gc()
      
    }
    
    semillerio <- 1 #es semillerio?
    semilla <- k
    # Ordena por probabilidad descendente -> uso order porque es de data.table y asi funciona (setorder: NO funciona)
    tb_prediccion <- tb_prediccion[order(-tb_prediccion[[paste0('promedio_top', i)]])]
    
    #Agrega la posición ordenada o rank:
    tb_prediccion[ , paste0('pos_top', i) := .I ]
    
    # Calcula la ganancia acumulada
    tb_prediccion[, gan_acum := cumsum(ganancia)]
    
    # Encuentra el máximo de la ganancia suavizada y su posicion (punto de corte optimo y su único valor de ganancia)
    max_gan <- max(tb_prediccion[, gan_acum])
    pos_optima <- which.max(tb_prediccion[, gan_acum] == max_gan)
    
    # Almacena los resultados en el nuevo data.table
    resultados <- rbind(resultados, data.table(hyperparam = i, seed = semilla, ganancia = max_gan, pos_optima = pos_optima, semillerio = semillerio))    
    
    if(i>1){
      semillerio <- 11 #es semillerio?
      semilla <- (k*i)
      # Ordena por probabilidad descendente -> uso order porque es de data.table y asi funciona (setorder: NO funciona)
      tb_prediccion <- tb_prediccion[order(-tb_prediccion[[paste0('promedio','_todos')]])]
      
      #Agrega la posición ordenada o rank:
      tb_prediccion[ , pos_promedio := .I ]
      
      # Calcula la ganancia acumulada
      tb_prediccion[, gan_acum := cumsum(ganancia)]
      
      # Encuentra el máximo de la ganancia suavizada y su posicion (punto de corte optimo y su único valor de ganancia)
      max_gan <- max(tb_prediccion[, gan_acum])
      pos_optima <- which.max(tb_prediccion[, gan_acum] == max_gan)
      
      # Almacena los resultados en el nuevo data.table
      resultados <- rbind(resultados, data.table(hyperparam = i, seed = semilla, ganancia = max_gan, pos_optima = pos_optima, semillerio = semillerio))
    }
    
    timestamp()
    message(paste0("fin calculo de ganancias Top ", i))   
  }
  
  # Genera CSV con los resultados de única Gananancia para c/u de los
  fwrite(resultados, 
         file = 'Resultados_GanMax.csv', 
         sep = ",")
  
  # Genera CSV con los resultados de única Gananancia para c/u de los
  fwrite(tb_prediccion, 
         file = 'ProbPredicha_individuales_y_ensamble.csv', 
         sep = ",")   
}
