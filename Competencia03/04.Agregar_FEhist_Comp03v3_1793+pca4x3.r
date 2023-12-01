#---------------------------------------------------------------------------------------------------------------
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

install.packages("SYNCSA")
#install.packages("umap")
#install.packages("umapr")

require("data.table")
require("lightgbm")
require("SYNCSA") #pca

#require("umapr")
#library("umap")
#library("FactoMineR")
#require("zoo")
#require("Rcpp")

#---INICIO: Carga dataset y verifica---------------------------------------------------------------------------
#Directorio:
setwd("~/buckets/b1") 

#cargo el dataset
dataset <- fread("./datasets/competencia03_FE_drift.csv.gz")
#dataset <- fread("./datasets/competencia03_FEhist_v2_reduced.csv.gz")#1793  #--> este se levanto en un segundo paso, para agregar PCA, porque daba tarda y dio errores. 
                                                                             #--> solo despues de muchos intentos y muchisimas RAM me corrio PCA en el dataset reducido con canaritos.      

dim(dataset) #4893028     214

#---Ordeno y Selecciono las columnas---------------------------------------------------------------------------
#ordeno el dataset para aplicar luego los fixes
setorder( dataset, numero_de_cliente, foto_mes )

#Columnas Seleccionadas para LAGs:
campos_a_quitar  <- colnames(dataset)
campos_a_quitar  <- campos_a_quitar[campos_a_quitar %like% "^(add|clase)"]
cols_lagueables  <- copy(  setdiff( colnames(dataset), c("numero_de_cliente", "foto_mes", campos_a_quitar)) )

length(cols_lagueables)#206



#-----LAGs y DELTA_LAGS ------------------------------------------------------

#LAGs y DELTAS_LAGs Individuales:

lag1 <- TRUE
lag2 <- FALSE
lag3 <- TRUE
lag4 <- FALSE
lag6 <- TRUE
lag9 <- FALSE
lag12 <- TRUE
dim(dataset) #4893028   832 -> +618


if (lag1) {
  dataset[ , paste0("lag1_", cols_lagueables) := shift(.SD, 1, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0("delta1_", vcol ) := get(vcol) - get(paste0("lag1_", vcol))]  }
}

if (lag2) {
  dataset[ , paste0("lag2_", cols_lagueables) := shift(.SD, 2, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  #for (vcol in cols_lagueables) {
  #  dataset[ , paste0("delta2_", vcol ) := get(vcol) - get(paste0("lag2_", vcol))]  }
}

if (lag3) {
  dataset[ , paste0("lag3_", cols_lagueables) := shift(.SD, 3, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0("delta3_", vcol ) := get(vcol) - get(paste0("lag3_", vcol))]  }
}

if (lag4) {
  dataset[ , paste0("lag4_", cols_lagueables) := shift(.SD, 4, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  #for (vcol in cols_lagueables) {
  #  dataset[ , paste0("delta4_", vcol ) := get(vcol) - get(paste0("lag4_", vcol))]  }
}

if (lag6) {
  dataset[ , paste0("lag6_", cols_lagueables) := shift(.SD, 6, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  #for (vcol in cols_lagueables) {
  #  dataset[ , paste0("delta6_", vcol ) := get(vcol) - get(paste0("lag6_", vcol))]  }
}

if (lag9) {
  dataset[ , paste0("lag9_", cols_lagueables) := shift(.SD, 9, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  #for (vcol in cols_lagueables) {
  #  dataset[ , paste0("delta9_", vcol ) := get(vcol) - get(paste0("lag9_", vcol))]  }
}

if (lag12) {
  dataset[ , paste0("lag12_", cols_lagueables) := shift(.SD, 12, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  for (vcol in cols_lagueables) {
    dataset[ , paste0("delta12_", vcol ) := get(vcol) - get(paste0("lag12_", vcol))]  }
}

dim(dataset)#  4893028    1656
#------ PROMEDIO VENTANA ULTIMOS N MESES -----------------------------------
avg4  <- TRUE
avg8  <- FALSE
avg12  <- TRUE
avg18  <- FALSE


if (avg4) {
  dataset[ , paste0("avg4_", cols_lagueables) := frollmean(.SD, n = 4, align = "right", na.rm = TRUE, hasNA = TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]}

if (avg8) {
  dataset[ , paste0("avg8_", cols_lagueables) := frollmean(.SD, n = 8, align = "right", na.rm = TRUE, hasNA = TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]}

if (avg12) {
  dataset[ , paste0("avg12_", cols_lagueables) := frollmean(.SD, n = 12, align = "right", na.rm = TRUE, hasNA = TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]}

if (avg18) {
  dataset[ , paste0("avg18_", cols_lagueables) := frollmean(.SD, n = 18, align = "right", na.rm = TRUE, hasNA = TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]}

dim(dataset)#    1656 -> +824

#------ PROMEDIO VENTANAS CORRIDAS -----------------------------------
avg4a8  <- TRUE
avg4a12  <- FALSE
avg6a12  <- TRUE
avg8a12  <- FALSE
avg4a18  <- TRUE

if (avg4a8) {
  dataset[ , paste0("avg4a8_", cols_lagueables) := frollmean(shift(.SD, 4, NA, "lag"), n = 4, align = "right", na.rm = TRUE, hasNA = TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]}

if (avg4a12) {
  dataset[ , paste0("avg4a12_", cols_lagueables) := frollmean(shift(.SD, 4, NA, "lag"), n = 8, align = "right", na.rm = TRUE, hasNA = TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]}

if (avg6a12) {
  dataset[ , paste0("avg6a12_", cols_lagueables) := frollmean(shift(.SD, 6, NA, "lag"), n = 6, align = "right", na.rm = TRUE, hasNA = TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]} 

if (avg8a12) {
  dataset[ , paste0("avg6a12_", cols_lagueables) := frollmean(shift(.SD, 8, NA, "lag"), n = 4, align = "right", na.rm = TRUE, hasNA = TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]}


if (avg4a18) {
  dataset[ , paste0("avg4a18_", cols_lagueables) := frollmean(shift(.SD, 12, NA, "lag"), n = 6, align = "right", na.rm = TRUE, hasNA = TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]} 




#------ RATIOS Y DISTANCIAS  -----------------------------------

#-------INDICADORES DE TENDENCIA ---------------------
#1) indicadores de tendencia: valor actual / promedio_ventana (ratioavg)
ratioavg4  <- TRUE #requiere avg4 previamente
ratioavg8  <- FALSE #requiere avg8 previamente
ratioavg12 <- FALSE #requiere avg12 previamente

if (avg4 & ratioavg4) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("ratioavg4_", vcol ) := ifelse(get(vcol) == 0, 0, ifelse( get(paste0("avg4_", vcol)) == 0 , NA, get(vcol) / get(paste0("avg4_", vcol)) ) )] }
} 

if (avg8 & ratioavg8) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("ratioavg8_", vcol ) := ifelse(get(vcol) == 0, 0, ifelse( get(paste0("avg8_", vcol)) == 0 , NA, get(vcol) / get(paste0("avg8_", vcol)) ) )]}
} 

if (avg12 & ratioavg12) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("ratioavg12_", vcol ) := ifelse(get(vcol) == 0, 0, ifelse( get(paste0("avg12_", vcol)) == 0 , NA, get(vcol) / get(paste0("avg12_", vcol)) ) )]}
} 


#2) indicadores de tendencia general: promedio_ventana_corta / promedio_ventana_larga (ratioavg)
ratioavg4_12  <- TRUE #requiere avg4 y avg12 previamente
ratioavg4_18  <- FALSE #requiere avg4 y avg18 previamente

if (avg4 & avg12 & ratioavg4_12) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("ratioavg4_12_", vcol ) := ifelse(get(paste0("avg4_", vcol)) == 0, 0, ifelse( get(paste0("avg12_", vcol)) == 0 , NA, get(paste0("avg4_", vcol)) / get(paste0("avg12_", vcol)) ) )] }
}

if (avg4 & avg18 & ratioavg4_18) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("ratioavg4_18_", vcol ) := ifelse(get(paste0("avg4_", vcol)) == 0, 0, ifelse( get(paste0("avg18_", vcol)) == 0 , NA, get(paste0("avg4_", vcol)) / get(paste0("avg18_", vcol)) ) )] }
}


#-------INDICADORES DE VARIACION (SIMIL DELTA) ---------------------

#Actual - AvgN Ultimos:
var_avg4 <- TRUE      #requiere avg4 previamente
var_avg4a12  <- FALSE #requiere avg4a12 previamente
var_avg4a18  <- FALSE #requiere avg12a18 previamente

if (avg4 & var_avg4) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("var_avg4_", vcol ) := get(vcol) - get(paste0("avg4_", vcol))]  }
}

if (avg4a12 & var_avg4a12) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("var_avg4a12_", vcol ) := get(vcol) - get(paste0("avg4a12_", vcol))]  }
}

if (avg4a18 & var_avg4a18) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("var_avg4a18_", vcol ) := get(vcol) - get(paste0("avg4a18_", vcol))]  }
}   


#Avg Cercano - Avg Lejanos:
avg4_avg4a8 <- TRUE      #requiere avg4 previamente
avg4_avg6a12 <- TRUE
avg4_avg4a18 <- FALSE


if (avg4 & avg4_avg4a8) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("avg4_avg4a8_", vcol ) := get(paste0("avg4_", vcol)) - get(paste0("avg6a12_", vcol))]  }
}

if (avg4a12 & avg4_avg6a12) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("avg4_avg6a12_", vcol ) := get(paste0("avg4_", vcol)) - get(paste0("avg6a12_", vcol))]  }
}

if (avg4a18 & avg4_avg4a18) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("avg4_avg4a18", vcol ) := get(paste0("avg4_", vcol)) - get(paste0("avg4a18_", vcol))]  }
} 


#-------RATIOS DE VARIACIONES ---------------------

#a que porcentaje estamos del maximo de los ultimos meses
r_var_avg12  <- TRUE #requiere perc75_4a18 previamente
r_avg4_avg6a12 <- TRUE #requiere avg4 y perc75_4a18 previamente

if (r_var_avg12) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("r_var_avg12_", vcol ) := ifelse(get(vcol) == 0, 0,ifelse( get(paste0("avg12_", vcol))==0, NA, get(vcol) / get(paste0("avg12_", vcol)) ) )]  }
}

if (r_avg4_avg6a12) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("r_avg4_avg6a12_", vcol ) := ifelse(get(paste0("avg4_", vcol)) == 0, 0,ifelse( get(paste0("avg6a12_", vcol))==0, NA, get(paste0("avg4_", vcol)) / get(paste0("avg6a12_", vcol)) ) )]  }
}


dim(dataset) #4893028    5364

#-------DELTAS NORMALIZADOS ---------------------
delta1_avg4  <- TRUE #requiere perc75_4a18 previamente
delta3_avg4 <- TRUE #requiere avg4 y perc75_4a18 previamente
delta12_avg12 <- TRUE #requiere avg4 y perc75_4a18 previamente

if (delta1_avg4 ) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("delta1_avg4_", vcol ) := ifelse(get(paste0("delta1_", vcol)) == 0, 0,ifelse( get(paste0("avg4_", vcol))==0, NA, get(paste0("delta1_", vcol)) / get(paste0("avg4_", vcol)) ) )]  }
}

if (delta3_avg4 ) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("delta3_avg4_", vcol ) := ifelse(get(paste0("delta3_", vcol)) == 0, 0,ifelse( get(paste0("avg4_", vcol))==0, NA, get(paste0("delta3_", vcol)) / get(paste0("avg4_", vcol)) ) )]  }
}

if (delta12_avg12 ) {
  for (vcol in cols_lagueables) {
    dataset[ , paste0("delta12_avg12_", vcol ) := ifelse(get(paste0("delta12_", vcol)) == 0, 0,ifelse( get(paste0("avg12_", vcol))==0, NA, get(paste0("delta12_", vcol)) / get(paste0("avg12_", vcol)) ) )]  }
}

dim(dataset) #4893028    
dataset_original <- copy(dataset)


#---EXPORTA DATABSE GENERADA A .CSV---------
setwd("~/buckets/b1/datasets/")
dim(dataset)

#EXPORTAMOS a un .CSV --> jamas se guardó: se colgo en el proceso de guardarla en el bucket
#fwrite( dataset_original,
#        file= "competencia03_FEhist_v2_full.csv.gz",
#        sep= "," )

#-------------------Canaritos-----------------------------------------------------------
#Elimina del dataset las variables que estan por debajo de la capa geologica de canaritos
#se llama varias veces, luego de agregar muchas variables nuevas, para ir reduciendo la cantidad de variables
# y así hacer lugar a nuevas variables importantes

#------------------fganancia_lgbm_meseta------------------------------------------------------------
VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")
  
  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 78000, -2000 ) ) )
  
  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta
  
  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tamaño 500
  
  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )
  
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}

#----------canaritos-----------------------
GVEZ <- 1 

CanaritosAsesinos  <- function( canaritos_ratio=0.15 )
{
  gc()
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]
  
  for( i  in 1:(ncol(dataset)*canaritos_ratio))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]
  
  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","add_mesn","add_last_mes","add_lastn","clase_gral_n","clase_gral","clase01", "foto_mes" ) )
  
  azar  <- runif( nrow(dataset) )
  dataset[ , entrenamiento := foto_mes>= 202103 &  foto_mes<= 202105  & ( clase01==1 | azar < 0.10 ) ]
  
  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
  )
  
  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202107, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202107, clase01],
                          weight=  dataset[ foto_mes==202107, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
  )
  
  
  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 999983,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.065, 
                 feature_fraction= 1.0,   #lo seteo en 1 para que las primeras variables del dataset no se vean opacadas
                 min_data_in_leaf= 260,
                 num_leaves= 60,
                 early_stopping_rounds= 200 )
  
  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )
  
  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]
  
  fwrite( tb_importancia, 
          file= paste0( "impo_", GVEZ ,".txt"),
          sep= "\t" )
  
  GVEZ  <<- GVEZ + 1
  
  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) + 2*sd(pos) ]  #Atencion corto en la mediana mas DOS desvios!!
  
  col_utiles  <- tb_importancia[ pos < umbral & !( Feature %like% "canarito"),  Feature ]
  col_utiles  <-  unique( c( col_utiles,  c("numero_de_cliente","foto_mes","clase_ternaria","add_mesn","add_last_mes","add_lastn","clase_gral_n","clase_gral","mes") ) )
  col_inutiles  <- setdiff( colnames(dataset), col_utiles )
  
  dataset[  ,  (col_inutiles) := NULL ]
  
}

#APLICA CANARITOS:
canaritos <- TRUE

if( canaritos ){
  ncol( dataset )
  CanaritosAsesinos( canaritos_ratio = 0.15 )
  ncol( dataset )
}

dim(dataset)
#--------------------------- Agrega Variables con PCA ---------------------------
#ACLARACIÓN: (estaba puesto ANTES de canaritos, pero NO funciono, en tantas varaibles siempre habia NA o algo que lo rompia) 
#             solo 4 de los 10 grupos de variables corrieron --> aplicado sobre dataset reducido de 1793 variables

#Seleccion grupos de variables a integrar:
cols  <- colnames(dataset)
colsrentabilidad <- copy(cols[cols %like% "(rentabilidad)"]) #156
colscuentas <- copy(cols[cols %like% "(cuenta|caja)"]) #546
colsprestamo <-  copy(cols[cols %like% "(prestamo)"]) #208
colsinversion <-  copy(cols[cols %like% "(inversion|plazo_fijo)"]) #364
colspayroll <-  copy(cols[cols %like% "(payroll)"]) #234
colsdebitos <-  copy(cols[cols %like% "(debito|pagode|pagomis)"]) #338
colscomisiones <-  copy(cols[cols %like% "(comision)"]) #208
colsmovimiento <-  copy(cols[cols %like% "(transferencia|extraccion|deposit|atm)"]) #416
colsvisa <-  copy(cols[cols %like% "(visa)"]) #234
colsmaster <-  copy(cols[cols %like% "(master)"]) #234

#------------------PCA-----------------------------:
# Función para aplicar PCA de SYNCSA y obtener las primeras 3 componentes principales
apply_pca <- function(cols, prefix){
  pca_result <- pca(dataset[, ..cols])
  dataset[,paste0(prefix,1) := pca_result$individuals[,1]]
  dataset[,paste0(prefix,2) := pca_result$individuals[,2]]
  dataset[,paste0(prefix,3) := pca_result$individuals[,3]]
}


# Aplicar PCA para cada conjunto de variables
apply_pca(colsrentabilidad, "rentabilidad_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )



apply_pca(colscuentas, "cuentas_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )


apply_pca(colsprestamo, "prestamo_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )


apply_pca(colsinversion, "inversion_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )


apply_pca(colspayroll, "payroll_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )


apply_pca(colsdebitos, "debitos_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )


apply_pca(colscomisiones, "comisiones_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )


apply_pca(colsmovimiento, "movimiento_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )


apply_pca(colsvisa, "visa_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )


apply_pca(colsmaster, "master_pca")
dim(dataset)
#exportamos un archivos con las variables PCA creadas para incluirlas facil luego.
colspca <- colnames(dataset)
colspca <- colspca[colspca %like% "(pca)"]
colspca <- c("numero_de_cliente", "foto_mes", colspca ) 
setwd("~/buckets/b1/datasets/")
fwrite( dataset[, ..colspca],
        file= "pca_FEvar.csv",
        sep= "," )

#---EXPORTA DATABSE GENERADA A .CSV---------
setwd("~/buckets/b1/datasets/")
dim(dataset)



#EXPORTAMOS a un .CSV
fwrite( dataset,
        file= "competencia03_FEhist_v2_reduced_pca.csv.gz",
        sep= "," )
