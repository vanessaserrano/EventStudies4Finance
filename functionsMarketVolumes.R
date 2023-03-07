#FUNCIONES SOPORTE PARA EMPEZAR EL ANALISIS
#setwd("C:/Users/pcasals/Desktop/TFG/Todas")

#directorioDatos = "C:/Users/pcasals/Desktop/TFG/ItalyNetherlandsSpain"
#documentoEventos= "Fechas_ok.txt"
#documentoEventos="datos_eventos.txt"

AnalisisDirectorio <- function(directorioDatos,documentoEventos){
  
  # setwd(directorioDatos)
  
  EMPRESAS_MUESTRA <- read.table(documentoEventos,sep="\t",header=T,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  colnames(EMPRESAS_MUESTRA) <- c("COMPANY","DATE","MARKET")
  EMPRESAS_MUESTRA <- data.frame(EMPRESAS=unique(EMPRESAS_MUESTRA[,1]))
  
  EMPRESAS_DIRECTORIO <- c(unique(list.files(directorioDatos,pattern = ".txt")))
  
  RESULTADO <- data.frame(EMPRESA=EMPRESAS_MUESTRA$EMPRESAS, DIRECTORIO =NA)
  #Empresas que estan en la muestra y no en el directorio
  for(i in 1:length(EMPRESAS_MUESTRA$EMPRESAS)){
    if(paste(as.character(EMPRESAS_MUESTRA$EMPRESAS[i]),'.txt',sep='') %in% EMPRESAS_DIRECTORIO){
      RESULTADO$DIRECTORIO[i] <- "SI"
    } else{
      RESULTADO$DIRECTORIO[i] <- "NO"
    }
  }
}

#Ejemplo:
#AnalisisDirectorio(directorioDatos = "C:/Users/pcasals/Desktop/TFG/ItalyNetherlandsSpain", documentoEventos= "Fechas_ok.txt")


#FUNCIONES PARA EL ANALISIS DE LA MATRIZ INICIAL DE EVENTOS
#PARA ANALISIS DOC RENTABILIDAD SE MIRA:
#--> LA VENTANA DE DIAS ESTABLECIDA SOBRE DIAS DE COTIZACION NO NATURALES
#--> EXISTENCIA DE SUFICIENTES DATOS PARA LA VENTANA DE ESTIMACION PREVIA AL EVENTO
#--> QUE NO HAYA CONTAMINACION ENTRE EVENTOS EN BASE A DIAS DE COTIZACION

# SE DEVUELVE: LA MATRIZ DE EVENTOS CON:
#--> COLUMNA DE FECHA DIA 0 (SE NOTIFICA SI SE HA CAMBIADO CON EL OBJETIVO QUE COINCIDA EN DIA DE COTIZACIÃ“N DE
# EMPRESA Y MERCADO)
#--> FECHA LIE
#-->FECHA LSV
#CONCLUSION DEL EVENTO (OK, ERRONEO O NO ANALIZABLE POR FALTA DE DATOS)
#COMENTARIOS ADICIONALES

#datos='Fechas_ok.txt'

# Attention format is hard-coded (sometimes)
ANALISIS_DOC_VOLUMEN <- function(datos,LSPE1=95,LSPE2=95,format="%d/%m/%Y",directorio){
  
  datos <- read.table(datos,sep="\t",header=T,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  datos[,2] <- as.Date(datos[,2],format='%d/%m/%Y')
  datos <- datos[order(datos[,1],datos[,2],decreasing=F),]
  datos$Fecha_evento_def <- as.Date(NA,format=format)
  datos$Fecha_LSPE2 <- as.Date(NA,format=format)
  datos$Fecha_LSPE1 <- as.Date(NA,format=format)
  datos$ControlEvento <- NA
  datos$Comentarios <- NA
  
  i=1
  
  #Cargamos los datos para la primera empresa
  empresa <- datos[i,1]
  datos_empresa <- read.table(paste(directorio,'/',as.character(empresa),'.txt',sep = ''), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(datos_empresa) <- c("Date","PX_LAST","PX_VOLUME")
  datos_empresa <- datos_empresa[!is.na(datos_empresa[,"PX_LAST"]),]
  datos_empresa <- datos_empresa[!is.na(datos_empresa[,"PX_VOLUME"]),]

  datos_empresa<- datos_empresa[!format(datos_empresa[,"Date"],format='%d/%m')==format(as.Date("01/01/2000",format='%d/%m/%Y'),'%d/%m'),]
  datos_empresa<- datos_empresa[!format(datos_empresa[,"Date"],format='%d/%m')==format(as.Date("25/12/2000",format='%d/%m/%Y'),'%d/%m'),]
  datos_empresa$Date <- as.Date(datos_empresa$Date,format='%d/%m/%Y')

  for(i in 1:nrow(datos)){
    if(i>=2 && datos[i,1] != datos[i-1,1]){  #este if sirve para cargar solo los datos cuando no se hayan cargado anteriormente (aumentar eficiencia del cÃ³digo)
      empresa <- datos[i,1]
      datos_empresa <- read.table(paste(directorio,'/',as.character(empresa),'.txt',sep = ''), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
      colnames(datos_empresa) <- c("Date","PX_LAST","PX_VOLUME")
      datos_empresa <- datos_empresa[!is.na(datos_empresa[,"PX_LAST"]),]
      datos_empresa <- datos_empresa[!is.na(datos_empresa[,"PX_VOLUME"]),]
      datos_empresa<- datos_empresa[format(datos_empresa[,"Date"],format='%d/%m')!=format(as.Date("01/01/2000",format='%d/%m/%Y'),'%d/%m'),]
      datos_empresa<- datos_empresa[format(datos_empresa[,"Date"],format='%d/%m')!=format(as.Date("25/12/2000",format='%d/%m/%Y'),'%d/%m'),]
      datos_empresa$Date <- as.Date(datos_empresa$Date,format='%d/%m/%Y')
    }
    
    #Comprobamos que la fecha evento puede ser esa, sino sumamos dias hasta encontrar una en comun entre mercado y empresa
    event_day <- as.Date(datos[i,2],format='%d/%m/%Y')
    fecha_buscar <- event_day

    while(is_empty(which(datos_empresa$Date == as.Date(fecha_buscar,format='%d/%m/%Y')))==T){
      fecha_buscar <- format(as.Date(fecha_buscar,format = '%d/%m/%Y') + 1, format='%d/%m/%Y')
    }
    
    if(event_day!=fecha_buscar){
      datos$Comentarios[i] <- paste("Fecha del evento de ",empresa,"modificada, pasa de ",
                                    event_day," a ",fecha_buscar)
      event_day <- fecha_buscar
    } else {
      datos$Comentarios[i]<- "--"
    }
    datos$Fecha_evento_def[i]<-as.Date(event_day,format=format)
    
    # Calculo la fila de la matriz datos_empresa donde se encuentra el dia del evento
    
    fila_evento_empresa <- which(datos_empresa$Date == as.Date(event_day,format=format))
    if(fila_evento_empresa < abs(LSPE1)){
      datos$ControlEvento[i]="ANULADA POR FALTA DE DATOS"
      datos$Fecha_LSPE2[i] <- datos_empresa[fila_evento_empresa + LSPE2,"Date"]
    } else {
      datos$Fecha_LSPE2[i] <- datos_empresa[fila_evento_empresa + LSPE2,"Date"]
      datos$Fecha_LSPE1[i] <- datos_empresa[fila_evento_empresa - LSPE1,"Date"]
    }
  }  # Final del primer FOR para establecer fecha_evento_def LSV y LIE

  i=2
  
  if(is.na(datos$ControlEvento[1])==T){
    datos$ControlEvento[1]="OK"}
  
  if(nrow(datos)>1) {
    for(i in 2:nrow(datos)){
      if(is.na(datos$ControlEvento[i]) == T) {
        if(datos[i,1]==datos[i-1,1] && (datos$Fecha_LSPE1[i] - datos$Fecha_LSPE2[i-1])< 0){
          #La empresa es la misma que la fila anterior
          datos$ControlEvento[i]="ERRONEO"
        } else {
          datos$ControlEvento[i]="OK"
        }
      }
    }#final del segundo FOR para determinar finalmente quÃ© eventos son analizables
  }
  
  return(datos)
}

#Ejemplo:
#head(ANALISIS_DOC_VOLUMEN("Fechas_24052018(2).txt"),10)
#FUNCIONES PARA CALCULO
#CALCULOS POR PROMEDIO
#LIPE1: Limite inferior ventana estimacion 1 (periodo previo al evento)
#LSPE1: Limite superior ventana estimacion 1 (periodo previo al evento)
#LIPE2: Limite inferior ventana estimacion 2 (periodo previo al evento)
#LSPE2: Limite superior ventana estimacion 2 (periodo previo al evento)
#LVE: Limite ventana evento

VOLUMEN_MEDIA <- function(datos, fecha_evento, LSPE1 = 95,LIPE1=21, LSPE2=95,LIPE2=21,LVE=10, format = '%d/%m/%Y'){
  colnames(datos) <- c("Date","PX_LAST","PX_VOLUME")
  datos$Date <- as.Date(datos$Date, format = format)
  event_day <- as.Date(fecha_evento, format = format)
  event_day_data <- which(datos$Date == event_day)
  window_mean1 <- mean(datos$PX_VOLUME[(event_day_data - LSPE1):(event_day_data - LIPE1)], na.rm = TRUE)
  window_mean2<- mean(datos$PX_VOLUME[(event_day_data + LIPE2):(event_day_data + LSPE2)], na.rm = TRUE)
  window_mean<-(window_mean1+window_mean2)/2

  # Volum <- ifelse(is.na(window_mean),NA,datos$PX_VOLUME[(event_day_data - LSPE1):(event_day_data + LSPE2)] / window_mean)
  if(is.na(window_mean)){
    Volum = NA
  } else {
    Volum =datos$PX_VOLUME[(event_day_data - LSPE1):(event_day_data + LSPE2)] / window_mean
  }

  AV <- data.frame(Date = paste("Day", -LSPE1:+LSPE2, sep=""),
                   AAVolume = Volum)
  
  # AV <- na.omit(AV)
  #Nos conviene que devuelva los resultados no de forma cronolÃ³gica sino con el siguiente patrÃ³n
  # -95--> -21
  # 21--> 95
  #-10--> 10
  
  RESULTADOS <- data.frame(Date=c(paste("Day", -LSPE1:-LIPE1, sep=""),
                                  paste("Day", LIPE2:LSPE2, sep=""),
                                  paste("Day", -LVE:LVE, sep="")))
  
  RESULTADOS$AV <- merge(x=RESULTADOS, y=AV, by = "Date", all.x = TRUE,sort=F)[,2]
  
  return(RESULTADOS)
  
}

ACUMULATIVA_VOLUMEN_MEDIA <- function(datos,LSPE1 = 95,LIPE1=21, LSPE2=95,LIPE2=21,LVE=10,format="%d/%m/%Y",directorio){
  
  #analisis_array<-read.table(analysis_array,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",
  #                          header=T,dec=".")
  
  analisis_array <- ANALISIS_DOC_VOLUMEN(datos,LSPE1=LSPE1,LSPE2=LSPE2,format=format,directorio = directorio)
  #Saco de la tabla los eventos que no sean analizables por posibles efectos confundentes
  analisis_array <- analisis_array[which(analisis_array$ControlEvento=="OK"),]
  
  tabla_resultados<-data.frame(Date=c(paste("Day", -LSPE1:-LIPE1, sep=""),
                                      paste("Day", LIPE2:LSPE2, sep=""),
                                      paste("Day", -LVE:LVE, sep="")))
  
  
  i<-1
  b<-2
  
  for(i in 1:nrow(analisis_array)){
    tabla_resultados[,b]<-VOLUMEN_MEDIA(datos=read.table(paste(directorio,'/',as.character(analisis_array[i,1]),'.txt',sep=""), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec="."),
                                        fecha_evento=analisis_array[i,"Fecha_evento_def"],
                                        LSPE1 = LSPE1,LIPE1=LIPE1, LSPE2=LSPE2,LIPE2=LIPE2,LVE=LVE)[,2]
    b<-b+1
  }
  
  colnames(tabla_resultados) <- c("Day", paste(analisis_array[,1], as.character(analisis_array[,"Fecha_evento_def"]), sep="_"))
  return(tabla_resultados)
  
}
#EJEMPLO
#head(ACUMULATIVA_VOLUMEN_MEDIA("Fechas_24052018(2).txt"),10)
#MODELO DE MERCADO
#RENTABILIDAD MERCADO

#mercado="FTSEItaliaALLSHARE"
#empresa="A2A.txt"
#fecha_evento="09/11/2015"

#head(RENTABILIDAD_MERCADO(empresa="A2A.txt",fecha_evento="09/11/2015",mercado="FTSEItaliaALLSHARE"),15)
#CATALANA_MERCAT <- RENTABILIDAD_MERCADO(empresa="CatalanaOccidente.txt", mercado="IGBM", fecha_evento="26/04/2012",datos_mercados="datos_mercados2.txt")

RENTABILIDAD_MERCADO <- function(empresa, mercado, fecha_evento, LIE = 170,datos_mercados="datos_mercados.txt", 
                                 format = '%d/%m/%Y',ventana_pre_evento=20,LVE=10,directorio){
  datos_empresa <- read.table(paste(directorio,'/',as.character(empresa),'.txt',sep=""), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(datos_empresa) <- c("Date","PX_LAST","PX_VOLUME")
  #carga los valores de cotizacion de todos los mercados para todas las fechas
  datos_todos_mercados <- read.table(datos_mercados,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="", header=T,dec=".",stringsAsFactors = FALSE)
  colnames(datos_todos_mercados)[1] <- "Date"
  DATOSRENT<<-datos_todos_mercados
  
  #Se eliminan los valores NA para la columna de precios
  datos_empresa <- datos_empresa[!is.na(datos_empresa[,"PX_LAST"]),]

  #Se eliminan los valores de 1 enero y 25 diciembre si los hay, para todos los aÃ±os.
  datos_empresa <- datos_empresa[!format(datos_empresa[,"Date"],format='%d/%m')==format(as.Date("01/01/2000",format='%d/%m/%Y'),'%d/%m'),]
  datos_empresa <- datos_empresa[!format(datos_empresa[,"Date"],format='%d/%m')==format(as.Date("25/12/2000",format='%d/%m/%Y'),'%d/%m'),]
  #Se eliminan los valores NA para la columna de mercado correspondiente
  datos_todos_mercados <- datos_todos_mercados[!is.na(datos_todos_mercados[,mercado]),]
  #Se eliminan los valores de 1 enero y 25 diciembre si los hay, para todos los aÃ±os.
  datos_todos_mercados <- datos_todos_mercados[!format(datos_todos_mercados[,"Date"],format='%d/%m')==format(as.Date("01/01/2000",format='%d/%m/%Y'),'%d/%m'),]
  datos_todos_mercados <- datos_todos_mercados[!format(datos_todos_mercados[,"Date"],format='%d/%m')==format(as.Date("25/12/2000",format='%d/%m/%Y'),'%d/%m'),]
  
  ##PREPARACION DATOS DEL MERCADO
  
  #escogemos los datos del mercado que nos interesa
  datos_mercado <- data.frame(Date = datos_todos_mercados$Date, PX_LAST = datos_todos_mercados[,mercado])
  #pasamos todas las fechas al mismo formato
  datos_mercado$Date <- as.Date(datos_mercado$Date, format = format)
  #calculo de la rentabilidad
  datos_mercado$Rentabilidad[2:nrow(datos_mercado)] <- log(datos_mercado$PX_LAST[2:nrow(datos_mercado)] / datos_mercado$PX_LAST[1:(nrow(datos_mercado)-1)])
  
  ##PREPARACION DATOS DE LA EMPRESA
  #calculo de los valores de rentabilidad para la empresa
  datos_empresa$Rentabilidad[2:nrow(datos_empresa)] <- log(datos_empresa$PX_LAST[2:nrow(datos_empresa)] / datos_empresa$PX_LAST[1:(nrow(datos_empresa)-1)]) 
  #pasamos las fechas al mismo formato
  #todas las fechas de los datos de empresa
  datos_empresa$Date <- as.Date(datos_empresa$Date, format = format)
  
  #Eliminamos todas las filas del mercado que NO est?n en la empresa...
  if(dim(datos_mercado[-c(match(as.Date(as.Date(setdiff(datos_mercado$Date[1:length(datos_mercado$Date)],
                                                        datos_empresa$Date),origin="1970-01-01"),format=format),datos_mercado$Date)),])[1]!=0){
    
                            datos_mercado <- datos_mercado[-c(match(as.Date(as.Date(setdiff(datos_mercado$Date[1:length(datos_mercado$Date)],
                                             datos_empresa$Date),origin="1970-01-01"),format=format),datos_mercado$Date)),]
                            }
  
  #Se determina la fila que corresponde con la fecha del evento para la matriz de datos del mercado
  fila_evento_mercado <- which(datos_mercado$Date == as.Date(fecha_evento,format=format))
  
  #se genera la LIE de datos que interesa
  ventana_datos_mercado <- data.frame(Date = datos_mercado$Date[(fila_evento_mercado - LIE):(fila_evento_mercado - ventana_pre_evento)],
                                    Rentabilidad_mercado=datos_mercado$Rentabilidad[(fila_evento_mercado - LIE):(fila_evento_mercado - ventana_pre_evento)])
  
  
  #Fecha final e inicial del periodo de estimacion que marca el mercado!!!
  fecha_LSPE_mercado <- ventana_datos_mercado$Date[1]
  fecha_LIPE_mercado <- ventana_datos_mercado$Date[nrow(ventana_datos_mercado)]
  
  #Busco en qu? filas se encuentran dichas fechas para el documento de la empresa
  
  fila_LSPE_empresa <- which(datos_empresa$Date == as.Date(fecha_LSPE_mercado,format=format))
  fila_LIPE_empresa <- which(datos_empresa$Date == as.Date(fecha_LIPE_mercado,format=format))
  
  #matriz con los valores de rentabilidad de la empresa y la fecha para la ventana de datos entrada, de la ventana
  #se descuentan los LVE dias mas cercanos al dia del evento
  
  ventana_datos_empresa <- data.frame(Date=datos_empresa$Date[fila_LSPE_empresa:fila_LIPE_empresa],
                                      Rentabilidad_empresa=datos_empresa$Rentabilidad[fila_LSPE_empresa:fila_LIPE_empresa])
  
  ##PREPARACION DE LOS DATOS PARA APLICAR EL MODELO DE AJUSTE
  
  #necesitamos tener un data.frame comÃºn con los datos de rentabilidad del mercado y la empresa con una misma columna de fecha,
  #para ello se aplica un merge
  #solo se han cogido valores de rentabilidad en los cuales ambas ventanas tuvieran datos, en el caso que uno de ellos fuera NA
  #se desestima la fecha y no se tiene en cuenta para el cÃ¡lculo.
  
  ventana_estimacion <- merge(ventana_datos_mercado,ventana_datos_empresa,by="Date")
  
  ##CALCULO DEL MODELO ESTIMACION MERCADO
  
  #se aplica una ajuste de tipo lineal que enfrenta los valores de cotizacion del mercado con los de la empresa que analizamos
  ajuste <- lm(Rentabilidad_empresa ~ 0 + Rentabilidad_mercado, 
               data = ventana_estimacion)
  AJUSTEMK <<- ajuste
  #Generamos una nueva matriz de valores con los que luego haremos la estimacion mediante el ajuste, en este caso nuestro ajuste
  #esta en funcion de los valores de cotizacion de mercado y por tanto sera lo que pondremos en el vector. No tan solo para la LIE
  #previa al evento sino tambien los valores para la fecha del evento y LVE dÃ�as posterior
  
  fila_start_estimacion <- which(datos_mercado$Date == datos_mercado$Date[fila_evento_mercado - LIE])
  fila_end_estimacion <- which(datos_mercado$Date == datos_mercado$Date[fila_evento_mercado + LVE])
  
  dfNuevos <- data.frame(Date=datos_mercado[fila_start_estimacion : fila_end_estimacion,"Date"],
                         Rentabilidad_mercado = datos_mercado[fila_start_estimacion : fila_end_estimacion,"Rentabilidad"])
  
  #Elimino los valores de fecha que estan en dfNuevos pero no en la empresa y darian problemas al hacer estimaci?n
  #dfNuevos <- dfNuevos[-c(match(as.Date(as.Date(setdiff(dfNuevos$Date[1:(length(dfNuevos$Date)-ventana_pre_evento-LVE)],ventana_datos_empresa$Date),origin="1970-01-01"),format=format),dfNuevos$Date)),]
  
  #Hacemos este merge para asegurarnos que cuadraran los dias al hacer la resta entre la rentabilidad estimada y la real
  dfNuevos$Rent_empresa <- merge(dfNuevos,datos_empresa,by="Date")[,"Rentabilidad"]
  #aplico los coeficientes del modelo para estimar los valores de lo que deberÃ�a ser la rentabilidad de la empresa
  dfNuevos$Estimacion_rent_empresa <- predict(ajuste,dfNuevos)
  # dfNuevos$Estimacion_rent_empresa <- coefficients(ajuste)[1] + coefficients(ajuste)[2] * dfNuevos$Rentabilidad_mercado
  #Se genera la matriz resultado con los dias expresados "en relativo" y los valores de anormalidad que corresponden a la resta entre
  #los reales menos los estimados por el modelo
  
  output <- data.frame(Date = paste("Day", -LIE:LVE, sep=""),
                       AAR =  dfNuevos$Rent_empresa - dfNuevos$Estimacion_rent_empresa)
  
  return(output)
  
}
#CIE <- RENTABILIDAD_MERCADO(empresa="CIE.txt",mercado="IGBM",fecha_evento="09/05/2016")
#ACUMULATIVA RENTABILIDAD MERCADO

ACUMULATIVA_RENTABILIDAD_MERCADO <- function(analysis_array,LIE=170,LVE=10, ventana_pre_evento=20, datos_mercados="datos_mercados.txt"
                                             ,format='%d/%m/%Y',directorio){
  #Aplicamos la funcion de eventos sucesivos para detectar aquellos eventos que pueden dar
  #problemas por EFECTOS CONFUNDENTES
  analisis_array <- analisisRentabilidad(datos=analysis_array,datos_mercados=datos_mercados, format=format,LIE=LIE,LVE=LVE,directorio)
  analisis_array <- analisis_array[which(analisis_array$ControlEvento=="OK"),]
  analisis_array <- analisis_array[!is.na(analisis_array$Fecha_LIE),]
  analisis_array <- analisis_array[!is.na(analisis_array$Fecha_LVE),]

  tabla_resultados<-data.frame(Day=paste("Day", -LIE:LVE, sep=""))
  i<-1
  b<-2
  
  for(i in 1:nrow(analisis_array)){
    
    tabla_resultados[,b] <- RENTABILIDAD_MERCADO(empresa=as.character(analisis_array[i,1]),
                                                 mercado=as.character(analisis_array[i,3]),
                                                 fecha_evento=analisis_array[i,"Fecha_evento_def"],
                                                 LIE=LIE,LVE=LVE,ventana_pre_evento=ventana_pre_evento,
                                                 format=format,datos_mercados,directorio)[,2]
    
    b <- b + 1
    
  }
  
  colnames(tabla_resultados) <- c("Day", paste(analisis_array[,1], analisis_array[,2],analisis_array[,3],sep="||"))
  return(tabla_resultados)
}

