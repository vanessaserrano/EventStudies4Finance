# Antigua función leerTabla
rentabilidadEmpresa <- function(nombreEmpresa, directorio){
  # datos <-read.table(paste(directorio,'/',nombreEmpresa,'.txt',sep=''), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  datos <-read.table(paste(directorio,'/',nombreEmpresa,'.txt',sep=''), na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(datos)<-c("Date","PX_LAST","PX_VOLUME")
  datos$Date <- as.Date(datos$Date, format="%d/%m/%Y")
  datos$Rentabilidad[2:nrow(datos)] <- log(datos$PX_LAST[2:nrow(datos)] / datos$PX_LAST[1:(nrow(datos)-1)])  
  colnames(datos)<-c("Date","PX_LAST","PX_VOLUME",nombreEmpresa)
  return(datos)
}

# Antigua función rentabilidad
rentabilidadMercado <- function(archivoDatosMercado, columna){ #Calcula la rentabilidad de una columna dada y devuelve dos columnas: Fecha y Rentabilidad
  #archivoDatosMercado="datos_mercados.txt"
  #columna="IGBM"
  datos <-read.table(archivoDatosMercado, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(datos)[1] <- "Date"
  #Elimino los valores que son NA
  datos <- datos[!is.na(datos[,eval(columna)]),]
  datos$Date <- as.Date(datos$Date, format="%d/%m/%Y")
  datos$Rentabilidad[2:nrow(datos)] <- log(as.numeric(datos[2:nrow(datos),columna]) / as.numeric(datos[1:(nrow(datos)-1),columna]))  
  return(datos[,c("Date","Rentabilidad")])
}

comprobacionesFactores <- function (documentoEventos,
                                    directorioDatos,
                                    datos_muestra="datos_muestra.txt",
                                    datos_mercados="datos_mercados.txt"){
  setwd(directorioDatos)
  # DATOS_EVENTOS <- read.table(documentoEventos,sep="\t",header=T,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  DATOS_EVENTOS <- read.table(documentoEventos,sep="\t",header=T,na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  colnames(DATOS_EVENTOS) <- c("COMPANY","DATE","MARKET")
  # DATOS_MUESTRA <- read.table(datos_muestra,sep="\t",header=T,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  DATOS_MUESTRA <- read.table(datos_muestra,sep="\t",header=T,na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  colnames(DATOS_MUESTRA) <- c("COMPANY","TICKER","MARKET",'YEAR',"STARTING_PRICE_YEAR(t-1)","CLOSING_PRICE_YEAR(t-1)","MARKET_CAP(t-1)",
                               "TOTAL_EQUITY(t-1)","REVENUES(t-1)","COGS(t-1)","SG&A(t-1)","INTEREST_EXPENSE(t-1)","ASSETS(t-1)","ASSETS(t-2)")
  # DATOS_MERCADOS <- read.table(datos_mercados,sep="\t",header=T,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  DATOS_MERCADOS <- read.table(datos_mercados,sep="\t",header=T,na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  colnames(DATOS_MERCADOS)[1] <- "Date"
  EMPRESAS_DIRECTORIO <- data.frame(EMPRESAS = unique(list.files(directorioDatos,pattern = ".txt")))
  RESULTADO <- data.frame(EMPRESA=unique(DATOS_EVENTOS$COMPANY), EN_DATOS_MUESTRA=NA, EN_DIRECTORIO =NA)
  
  #Comprobación de que todas las empresas de la muestra tienen datos en DATOS_MUESTRA y por tanto pueden formar parte de algun conjunto
  EMPRESAS <- unique(DATOS_EVENTOS$COMPANY)
  
  #ANALISIS 1: Empresas que NO estan en el documento de datos de muestra
  for(i in 1:length(EMPRESAS)){
    if(EMPRESAS[i] %in% DATOS_MUESTRA$COMPANY){
      RESULTADO$EN_DATOS_MUESTRA[i] <- "SI"
    } else{
      RESULTADO$EN_DATOS_MUESTRA[i] <- "NO"
    }}
  
  #Comprobación de que todas las empresas tienen su documento asociado en el directorio:
  #ANALISIS 2: Empresas que NO estan en el directorio
  for(i in 1:length(EMPRESAS)){
    if(as.character(EMPRESAS[i])  %in% EMPRESAS_DIRECTORIO$EMPRESAS){
      RESULTADO$EN_DIRECTORIO[i] <- "SI"
    } else{
      RESULTADO$EN_DIRECTORIO[i] <- "NO"
    }}
  
  # ELIMINAR????
  # MERCADOS_DISPONIBLES <- colnames(DATOS_MERCADOS)[2:ncol(DATOS_MERCADOS)]
  # MERCADOS_ANALISIS <- unique(DATOS_EVENTOS$MARKET)
  
  return(RESULTADO)
}

# Antigua función ANALISIS_DOC_RENTABILIDAD
analisisRentabilidad <- function(datos,datos_mercados="datos_mercados.txt", format="%d/%m/%Y",LIE=170,LVE=10,directorio){
  
  
  ##CARGA DE DATOS DE MERCADO -----------------------------------------------------------------------------------
  #Lectura de datos del mercado
  
  # datos_todos_mercados <- read.table(datos_mercados,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  datos_todos_mercados <- read.table(datos_mercados,na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = FALSE)
  colnames(datos_todos_mercados)[1] <- "Date"
  #Con este for se pretende limpiar los datos de cada columna de mercado y crear un objeto R para cada mercado
  #df_inter es una variable intermedia que vamos pisando para coger cada mercado y al final de cada iteracion la asignamos a un objeto R con el nombre del mercado
  #El objetivo es tener diferentes objetos de R creados en diferentes iteraciones con los datos de cada mercado, para ser mas eficientes y no ir reevaluando el 
  # archivo de texto de los diferentes mercados cada evaluaci?n de empresa (limpiando NA...)
  for(i in 2:ncol(datos_todos_mercados)){
    mercado = colnames(datos_todos_mercados)[i]
    #Cargamos los datos de esta iteraci?n al objeto intermedio df_inter
    df_inter <- data.frame(Date = datos_todos_mercados[,1], mercado = datos_todos_mercados[,i])
    #Sacamos todos los dias de cotizaci?n que tienen valor NA
    df_inter <- df_inter[!is.na(df_inter[,2]),]
    #Sacamos todas las cotizaciones de dia de navidad o 1 de enero
    df_inter <- df_inter[format(df_inter[,"Date"],format='%d/%m')!="01/01",]
    df_inter <- df_inter[format(df_inter[,"Date"],format='%d/%m')!="25/12",]
    #Uniformamos las fechas al formato establecido
    df_inter$Date <- as.Date(df_inter$Date,format='%d/%m/%Y')
    #Asignamos el objeto intermedio df_inter a un objeto de R con el nombre del mercado en cuesti?n
    assign(eval(mercado),df_inter)
  }
  #En el fondo queremos quedarnos solo con los objetos de los diferentes mercados, por tanto
  rm(df_inter)#Elimino el objeto de datos intermedio
  rm(datos_todos_mercados)#Elimino el objeto de datos_todos_mercados ya que tengo todos los mercados cargados en diferentes objetos
  
  ##CARGA DE DATOS DE EVENTOS Y EMPRESAS ------------------------------------------------------------------------------------
  # datos <- read.table(datos,sep="\t",header=T,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  datos <- read.table(datos,sep="\t",header=T,na.strings=c("","#N/A","N/A","NULL","-","NA"),quote="",stringsAsFactors = FALSE,dec=".")
  #Uniformamos el formato de fechas de toda la columna de eventos
  datos[,2] <- as.Date(datos[,2],format='%d/%m/%Y')
  #Ordenamos la matriz por orden alfab?tico de empresa y posteriormente por fecha de evento
  datos <- datos[order(datos[,1],datos[,2],decreasing=F),]
  #Generamos las columnas de fecha_evento_def,limites de ventanas de estimaci?n (LIE y LIE), ControlEvento y Comentarios
  datos$Fecha_evento_def <- as.Date(NA,format=format)
  datos$Fecha_LIE <- as.Date(NA,format=format)
  datos$Fecha_LVE <- as.Date(NA,format=format)
  datos$ControlEvento <- NA
  datos$Comentarios <- NA
  
  
  #HACEMOS UN SEGUNDO FOR PARA DETERMINAR PARA CADA EMPRESA
  #--> FECHA EVENTO DEFINITIVA
  #--> FECHA LIE
  #--> FECHA LVE
  
  #Cargamos los datos para la primera empresa y luego seguir? la iteraci?n a partir de la segunda, el tema es que la primera empresa tiene evento OK seguro, 
  #no tiene con quien compara la fecha anteriormente y fallar por contaminaci?n de evento
  
  empresa <- as.character(datos[1,1]) #Primera empresa de la matriz
  mercado <- datos[1,3] #Mercado de la primera empresa de la matriz
  #Carga,limpieza y adecuaci?n de los datos de la empresa
  # datos_empresa <- read.table(paste(directorio,'/',as.character(empresa),'.txt',sep = ''), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  datos_empresa <- read.table(paste(directorio,'/',as.character(empresa),'.txt',sep = ''), na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(datos_empresa) <- c("Date","PX_LAST","PX_VOLUME")
  datos_empresa <- datos_empresa[!is.na(datos_empresa[,"PX_LAST"]),]
  datos_empresa <- datos_empresa[format(datos_empresa[,"Date"],format='%d/%m')!="01/01",]
  datos_empresa <- datos_empresa[format(datos_empresa[,"Date"],format='%d/%m')!="25/12",]
  datos_empresa$Date <- as.Date(datos_empresa$Date,format='%d/%m/%Y')
  
  
  for(i in 1:nrow(datos)){
    if(i>=2 && datos[i,1] != datos[i-1,1]){  #este if sirve para cargar solo los datos de empresas que no se hayan cargado, sean distintas a la anterior fila...
      empresa <- datos[i,1]
      mercado <- datos[i,3]
      # datos_empresa <- read.table(paste(directorio,'/',as.character(empresa),'.txt',sep = ''), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
      datos_empresa <- read.table(paste(directorio,'/',as.character(empresa),'.txt',sep = ''), na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
      colnames(datos_empresa) <- c("Date","PX_LAST","PX_VOLUME")
      datos_empresa <- datos_empresa[!is.na(datos_empresa[,"PX_LAST"]),]
      datos_empresa <- datos_empresa[format(datos_empresa[,"Date"],format='%d/%m')!="01/01",]
      datos_empresa <- datos_empresa[format(datos_empresa[,"Date"],format='%d/%m')!="25/12",]
      datos_empresa$Date <- as.Date(datos_empresa$Date,format='%d/%m/%Y')
    }
    
    df_aux <- eval(parse(text=mercado)) #data frame con el mercado en cuestion para este evento
    
    #Comprobamos que la fecha evento puede ser esa, sino sumamos dias hasta encontrar una en comun entre mercado y empresa
    event_day = as.Date(datos[i,2],format='%d/%m/%Y')
    fecha_buscar <- event_day
    while(is_empty(which(datos_empresa$Date == as.Date(fecha_buscar,format='%d/%m/%Y')))==T
          || is_empty(which(df_aux$Date == as.Date(fecha_buscar,format='%d/%m/%Y')))==T){
      fecha_buscar <- as.Date(fecha_buscar,format = '%d/%m/%Y')+1
    }
    #A?adimos una nota en consola o comentario en la matriz en caso de tener que modificar fecha evento...
    if(!event_day==fecha_buscar){
      datos$Comentarios[i] <- paste("Fecha del evento de ",empresa,"modificada, pasa de ",event_day," a ",fecha_buscar)#Comentario en la columna "Comentarios" de la matriz
      event_day=fecha_buscar
    }else{datos$Comentarios[i]<- "--"}
    datos$Fecha_evento_def[i]<-as.Date(event_day,format=format) #incoroporamos la fecha final en campo fecha_definitiva...
    
    #Calculo la fila de la matriz datos_empresa donde se encuentra el dia del evento
    
    fila_evento_empresa <- which(datos_empresa$Date == as.Date(event_day,format=format))
    
    if(fila_evento_empresa < LIE){ #No hay suficientes datos para hacer la estimaci?n...
      
      datos$ControlEvento[i]="ANULADA POR FALTA DE DATOS"
      datos$Fecha_LVE[i] <- datos_empresa[fila_evento_empresa + LVE,"Date"]
      
    }else{ #Asignamos las fechas de LVE y LIE en base a fechas de cotizaci?n de la matriz 
      
      datos$Fecha_LVE[i] <- datos_empresa[fila_evento_empresa + LVE,"Date"]
      datos$Fecha_LIE[i] <- datos_empresa[fila_evento_empresa - LIE,"Date"]
    }
  }
  
  #Final del primer FOR para establecer fecha_evento_def LVE y LIE
  
  if(is.na(datos$ControlEvento[1])==T){datos$ControlEvento[1]="OK"}
  
  for(i in 2:nrow(datos)){
    if(is.na(datos$ControlEvento[i]) == T){
      if(datos[i,1]==datos[i-1,1] && (datos$Fecha_LIE[i] - datos$Fecha_LVE[i-1])< 0){#La empresa es la misma que la fila anterior
        datos$ControlEvento[i]="ERRONEO"
      }else{datos$ControlEvento[i]="OK"}}}#final del segundo FOR para determinar finalmente quÃ© eventos son analizables
  return(datos)#Para que nos devuelva la matriz con los eventos y las conclusiones del anÃ¡lisis
}

EMPRESAS_SMB <- function(datos_eventos,datos_muestra,fecha_evento,MARKET,porc_empr_SMB=0.5){
  #Año del evento
  YEAR <- format(as.Date(fecha_evento,format='%d/%m/%Y'),'%Y')
  #Lectura de los datos asociados a datos_muestra... (documento de texto)
  # datos_muestra<-read.table(datos_muestra, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  datos_muestra<-read.table(datos_muestra, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  colnames(datos_muestra) <- c("COMPANY","TICKER","MARKET",'YEAR',"STARTING_PRICE_YEAR(t-1)","CLOSING_PRICE_YEAR(t-1)","MARKET_CAP(t-1)",
                               "TOTAL_EQUITY(t-1)","REVENUES(t-1)","COGS(t-1)","SG&A(t-1)","INTEREST_EXPENSE(t-1)","ASSETS(t-1)","ASSETS(t-2)")
  #Lectura de los datos del documento eventos
  # datos_eventos <- read.table(datos_eventos, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  datos_eventos <- read.table(datos_eventos, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  colnames(datos_eventos) <- c("COMPANY","DATE","MARKET")
  #Filtraje de las empresas que han tenido eventos en el año del evento que estudiamos
  datos_eventos <- datos_eventos[format(as.Date(datos_eventos$DATE,format='%d/%m/%Y'),'%Y') == YEAR,] 
  #Selección solo de las empresas que tienen datos y de las que pertenecen al mercado y el año que se solicita
  datos_muestra <- datos_muestra[datos_muestra$YEAR==(as.numeric(YEAR)),] #Eliminamos todos los que no sean del año contable anterior al año del evento
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"MARKET_CAP(t-1)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[datos_muestra$MARKET==MARKET,] #Eliminamos todos los que no sean de ese mercado
  # ELIMINAR (USAR TODAS LAS EMPRESAS PARA CALCULAR FACTORES)
  # Cogemos solo las empresas que han tenido eventos en el mismo año que el evento que analizamos
  # datos_muestra <- datos_muestra[datos_muestra$COMPANY %in% datos_eventos$COMPANY,]
  #Calculo de la matriz de EMPRESAs con valores de capitalización
  CRIT_CAP <- data.frame(EMPRESA=datos_muestra$COMPANY,CAP=datos_muestra$MARKET_CAP)
  #Ordenación del dataframe en descendente
  CRIT_CAP <- CRIT_CAP[order(CRIT_CAP$CAP,decreasing=T),]
  #Calculo de las filas que debo coger de la muestra teniendo en cuenta el porcentaje de filas (porc_empr)
  #coge el entero superior de la multiplicación del % x num de filas total de la matriz de muestra
  filas_selec <- floor(nrow(CRIT_CAP)*porc_empr_SMB)
  #Total de filas de la matriz de la muestra
  filas_total <- nrow(CRIT_CAP)
  #Generación del data frame con las EMPRESAs seleccionadas de la muestra para representar el conjunto SMALL y BIG por criterio
  #de capitalización
  CRIT_CAP$SMB <- c(rep("BIG",filas_selec),rep("SMALL",filas_total-filas_selec))
  EMPRESAS_SMB_ext <<- CRIT_CAP
  return(CRIT_CAP)
}

EMPRESAS_HML <- function(datos_muestra="datos_muestra.txt",datos_eventos="datos_eventos.txt",fecha_evento,MARKET,porc_empr_HML=0.3){
  #Año del evento
  YEAR <- format(as.Date(fecha_evento,format='%d/%m/%Y'),'%Y')
  #Lectura de los datos asociados a datos_muestra... (documento de texto)
  # datos_muestra<-read.table(datos_muestra, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  datos_muestra<-read.table(datos_muestra, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  colnames(datos_muestra) <- c("COMPANY","TICKER","MARKET",'YEAR',"STARTING_PRICE_YEAR(t-1)","CLOSING_PRICE_YEAR(t-1)","MARKET_CAP(t-1)",
                               "TOTAL_EQUITY(t-1)","REVENUES(t-1)","COGS(t-1)","SG&A(t-1)","INTEREST_EXPENSE(t-1)","ASSETS(t-1)","ASSETS(t-2)")
  #Lectura de los datos del documento eventos
  # datos_eventos <- read.table(datos_eventos, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  datos_eventos <- read.table(datos_eventos, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  colnames(datos_eventos) <- c("COMPANY","DATE","MARKET")
  #Filtraje de las empresas que han tenido eventos en el año del evento que estudiamos
  datos_eventos <- datos_eventos[format(as.Date(datos_eventos$DATE,format='%d/%m/%Y'),'%Y') == YEAR,] 
  #Selección solo de las empresas que tienen datos y de las que pertenecen al mercado y el año que se solicita
  datos_muestra <- datos_muestra[datos_muestra$YEAR==(as.numeric(YEAR)),] #Eliminamos todos los que no sean del año contable anterior al año del evento
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"MARKET_CAP(t-1)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"TOTAL_EQUITY(t-1)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[datos_muestra$MARKET==MARKET,] #Eliminamos todos los que no sean de ese mercado
  datos_muestra <- datos_muestra[datos_muestra[,"MARKET_CAP(t-1)"]!=0,] #Eliminamos todos los que tengan MARKET_CAP igual a 0
  datos_muestra <- datos_muestra[datos_muestra[,"TOTAL_EQUITY(t-1)"]>=0,] #Eliminamos todos los que tengan TOTAL_EQUITY negativo
  # ELIMINAR (USAR TODAS LAS EMPRESAS PARA CALCULAR FACTORES)
  # Cogemos solo las empresas que han tenido eventos en el mismo año que el evento que analizamos
  # datos_muestra <- datos_muestra[datos_muestra$COMPANY %in% datos_eventos$COMPANY,]
  #Calculo de índice PatrimonioNeto/Capitalización
  datos_muestra$NET_EQUITYCAP <- datos_muestra$"TOTAL_EQUITY(t-1)"/datos_muestra$"MARKET_CAP(t-1)" 
  #Calculo de la matriz de EMPRESAs con valores de capitalización
  CRIT_NET_EQUITYCAP <- data.frame(EMPRESA=datos_muestra$COMPANY,NET_EQUITYCAP=datos_muestra$NET_EQUITYCAP)
  #Ordenación del dataframe en descendente
  CRIT_NET_EQUITYCAP <- CRIT_NET_EQUITYCAP[order(CRIT_NET_EQUITYCAP$NET_EQUITYCAP,decreasing=F),]
  #Generación del data frame con las EMPRESAs seleccionadas de la muestra para representar el conjunto SMALL y BIG por criterio
  #de capitalización
  CRIT_NET_EQUITYCAP$HML <- ifelse(CRIT_NET_EQUITYCAP$NET_EQUITYCAP<=quantile(CRIT_NET_EQUITYCAP$NET_EQUITYCAP,porc_empr_HML,na.rm=TRUE), "LOW", 
           ifelse(CRIT_NET_EQUITYCAP$NET_EQUITYCAP<=quantile(CRIT_NET_EQUITYCAP$NET_EQUITYCAP,1-porc_empr_HML,na.rm=TRUE),"MEDIUM","HIGH"))
  EMPRESAS_HML_ext <<- CRIT_NET_EQUITYCAP
  return(CRIT_NET_EQUITYCAP)
}

EMPRESAS_RMW <- function(fecha_evento, MARKET,datos_muestra="datos_muestra.txt", datos_eventos="datos_eventos.txt",porc_empr_RMW=0.3){
  #Año del evento
  YEAR <- format(as.Date(fecha_evento,format='%d/%m/%Y'),'%Y')
  #Lectura de los datos asociados a datos_muestra... (documento de texto)
  # datos_muestra<-read.table(datos_muestra, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  datos_muestra<-read.table(datos_muestra, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  colnames(datos_muestra) <- c("COMPANY","TICKER","MARKET",'YEAR',"STARTING_PRICE_YEAR(t-1)","CLOSING_PRICE_YEAR(t-1)","MARKET_CAP(t-1)",
                               "TOTAL_EQUITY(t-1)","REVENUES(t-1)","COGS(t-1)","SG&A(t-1)","INTEREST_EXPENSE(t-1)","ASSETS(t-1)","ASSETS(t-2)")
  #Lectura de los datos del documento eventos
  # datos_eventos <- read.table(datos_eventos, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  datos_eventos <- read.table(datos_eventos, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  colnames(datos_eventos) <- c("COMPANY","DATE","MARKET")
  #Filtraje de las empresas que han tenido eventos en el año del evento que estudiamos
  datos_eventos <- datos_eventos[format(as.Date(datos_eventos$DATE,format='%d/%m/%Y'),'%Y') == YEAR,] 
  #Selección solo de las empresas que tienen datos y de las que pertenecen al mercado y el año que se solicita
  datos_muestra <- datos_muestra[datos_muestra$YEAR==(as.numeric(YEAR)),] #Eliminamos todos los que no sean del año contable anterior al año del evento
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"REVENUES(t-1)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"COGS(t-1)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"INTEREST_EXPENSE(t-1)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"SG&A(t-1)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"TOTAL_EQUITY(t-1)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[datos_muestra$MARKET==MARKET,] #Eliminamos todos los que no sean de ese mercado
  datos_muestra <- datos_muestra[datos_muestra[,"TOTAL_EQUITY(t-1)"]>0,] #Eliminamos todos los que tengan TOTAL_EQUITY negativo o cero
  # ELIMINAR (USAR TODAS LAS EMPRESAS PARA CALCULAR FACTORES)
  # Cogemos solo las empresas que han tenido eventos en el mismo año que el evento que analizamos
  # datos_muestra <- datos_muestra[datos_muestra$COMPANY %in% datos_eventos$COMPANY,]
  #Calculo de Margen Operativo
  datos_muestra$OP <- (datos_muestra$`REVENUES(t-1)` - datos_muestra$`COGS(t-1)` + datos_muestra$`INTEREST_EXPENSE(t-1)` - datos_muestra$`SG&A(t-1)`)/datos_muestra$`TOTAL_EQUITY(t-1)`
  #Calculo de la matriz de EMPRESAs con valores de crit. margen operativo
  CRIT_OP <- data.frame(EMPRESA=datos_muestra$COMPANY,OP=datos_muestra$OP )
  #Ordenación del dataframe en descendente
  CRIT_OP <- CRIT_OP[order(CRIT_OP$OP,decreasing=F),]
  CRIT_OP$RMW <- ifelse(CRIT_OP$OP<=quantile(CRIT_OP$OP,porc_empr_RMW,na.rm=TRUE), "WEAK", 
         ifelse(CRIT_OP$OP<=quantile(CRIT_OP$OP,1-porc_empr_RMW,na.rm=TRUE),"MEDIUM","ROBUST"))
  EMPRESAS_RMW_ext <<- CRIT_OP
  return(CRIT_OP)
}

EMPRESAS_CMA <- function(fecha_evento, MARKET, datos_muestra="datos_muestra.txt", datos_eventos="datos_eventos.txt",porc_empr_CMA=0.3){
  #Año del evento
  YEAR <- format(as.Date(fecha_evento,format='%d/%m/%Y'),'%Y')
  #Lectura de los datos asociados a datos_muestra... (documento de texto)
  # datos_muestra<-read.table(datos_muestra, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  datos_muestra<-read.table(datos_muestra, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  colnames(datos_muestra) <- c("COMPANY","TICKER","MARKET",'YEAR',"STARTING_PRICE_YEAR(t-1)","CLOSING_PRICE_YEAR(t-1)","MARKET_CAP(t-1)",
                               "TOTAL_EQUITY(t-1)","REVENUES(t-1)","COGS(t-1)","SG&A(t-1)","INTEREST_EXPENSE(t-1)","ASSETS(t-1)","ASSETS(t-2)")
  #Lectura de los datos del documento eventos
  # datos_eventos <- read.table(datos_eventos, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  datos_eventos <- read.table(datos_eventos, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
  colnames(datos_eventos) <- c("COMPANY","DATE","MARKET")
  #Filtraje de las empresas que han tenido eventos en el año del evento que estudiamos
  datos_eventos <- datos_eventos[format(as.Date(datos_eventos$DATE,format='%d/%m/%Y'),'%Y') == YEAR,] 
  #Selección solo de las empresas que tienen datos y de las que pertenecen al mercado y el año que se solicita
  datos_muestra <- datos_muestra[datos_muestra$YEAR==(as.numeric(YEAR)),] #Eliminamos todos los que no sean del año contable anterior al año del evento
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"ASSETS(t-1)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[!is.na(datos_muestra[,"ASSETS(t-2)"]),] #Eliminamos todos los que no tienen datos
  datos_muestra <- datos_muestra[datos_muestra$MARKET==MARKET,] #Eliminamos todos los que no sean de ese mercado
  datos_muestra <- datos_muestra[datos_muestra[,"ASSETS(t-2)"]!=0,] #Eliminamos todos los que tengan ASSETS(t-2) igual a cero
  # ELIMINAR (USAR TODAS LAS EMPRESAS PARA CALCULAR FACTORES)
  #Cogemos solo las empresas que han tenido eventos en el mismo año que el evento que analizamos
  # datos_muestra <- datos_muestra[datos_muestra$COMPANY %in% datos_eventos$COMPANY,]
  #Calculo del crecimiento y la inversión
  datos_muestra$CREC <- datos_muestra$"ASSETS(t-1)" - datos_muestra$"ASSETS(t-2)"
  datos_muestra$INV <- datos_muestra$CREC / datos_muestra$"ASSETS(t-2)"
  #Calculo de la matriz de EMPRESAs con valores de crit. INV
  CRIT_INV <- data.frame(EMPRESA=datos_muestra$COMPANY,INV=datos_muestra$INV )
  #Ordenación del dataframe en descendente
  CRIT_INV <- CRIT_INV[order(CRIT_INV$INV,decreasing=F),]
  CRIT_INV$CMA <- ifelse(CRIT_INV$INV<=quantile(CRIT_INV$INV,porc_empr_CMA,na.rm=TRUE), "CONSERVATIVE", 
                        ifelse(CRIT_INV$INV<=quantile(CRIT_INV$INV,1-porc_empr_CMA,na.rm=TRUE),"MEDIUM","AGGRESSIVE"))
  EMPRESAS_CMA_ext <<- CRIT_INV
  return(CRIT_INV)
}

EMPRESAS_UMD <- function(fecha_evento, MARKET, datos_muestra="datos_muestra.txt", datos_eventos="datos_eventos.txt",porc_empr_UMD=0.3){
    #Año del evento

    YEAR <- format(as.Date(fecha_evento,format='%d/%m/%Y'),'%Y')
    #Lectura de los datos asociados a datos_muestra... (documento de texto)
    # datos_muestra<-read.table(datos_muestra, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
    datos_muestra<-read.table(datos_muestra, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
    colnames(datos_muestra) <- c("COMPANY","TICKER","MARKET",'YEAR',"STARTING_PRICE_YEAR(t-1)","CLOSING_PRICE_YEAR(t-1)","MARKET_CAP(t-1)",
                                 "TOTAL_EQUITY(t-1)","REVENUES(t-1)","COGS(t-1)","SG&A(t-1)","INTEREST_EXPENSE(t-1)","ASSETS(t-1)","ASSETS(t-2)")
    #Lectura de los datos del documento eventos
    # datos_eventos <- read.table(datos_eventos, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
    datos_eventos <- read.table(datos_eventos, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".",stringsAsFactors = F)
    colnames(datos_eventos) <- c("COMPANY","DATE","MARKET")
    #Filtraje de las empresas que han tenido eventos en el año del evento que estudiamos
    datos_eventos <- datos_eventos[format(as.Date(datos_eventos$DATE,format='%d/%m/%Y'),'%Y') == YEAR,] 
    #Selección solo de las empresas que tienen datos y de las que pertenecen al mercado y el año que se solicita
    datos_muestra <- datos_muestra[datos_muestra$YEAR==(as.numeric(YEAR)),] #Eliminamos todos los que no sean del año contable anterior al año del evento
    datos_muestra <- datos_muestra[!is.na(datos_muestra[,"CLOSING_PRICE_YEAR(t-1)"]),] #Eliminamos todos los que no tienen datos
    datos_muestra <- datos_muestra[!is.na(datos_muestra[,"STARTING_PRICE_YEAR(t-1)"]),] #Eliminamos todos los que no tienen datos
    datos_muestra <- datos_muestra[datos_muestra$MARKET==MARKET,] #Eliminamos todos los que no sean de ese mercado
    datos_muestra <- datos_muestra[datos_muestra[,"STARTING_PRICE_YEAR(t-1)"]!=0,]
    # ELIMINAR (USAR TODAS LAS EMPRESAS PARA CALCULAR FACTORES)
    # Cogemos solo las empresas que han tenido eventos en el mismo año que el evento que analizamos
    # datos_muestra <- datos_muestra[datos_muestra$COMPANY %in% datos_eventos$COMPANY,]
    #Calculo de % VARIACION ANUAL
    datos_muestra$INTERVAR <- (datos_muestra$"CLOSING_PRICE_YEAR(t-1)" - datos_muestra$`STARTING_PRICE_YEAR(t-1)`) / datos_muestra$`STARTING_PRICE_YEAR(t-1)` 
    #Calculo de la matriz de EMPRESAs con valores de capitalización
    CRIT_INTERVAR <- data.frame(EMPRESA=datos_muestra$COMPANY,INTERVAR=datos_muestra$INTERVAR)
    #Ordenación del dataframe en descendente
    CRIT_INTERVAR <- CRIT_INTERVAR[order(CRIT_INTERVAR$INTERVAR,decreasing=T),]
    CRIT_INTERVAR$UMD <- ifelse(CRIT_INTERVAR$INTERVAR<=quantile(CRIT_INTERVAR$INTERVAR,porc_empr_UMD,na.rm=TRUE), "DOWN", 
                           ifelse(CRIT_INTERVAR$INTERVAR<=quantile(CRIT_INTERVAR$INTERVAR,1-porc_empr_UMD,na.rm=TRUE),"MEDIUM","UP"))
    EMPRESAS_UMD_ext <<- CRIT_INTERVAR

    return(CRIT_INTERVAR)
  }   

CALCULO_HML <- function(datos_muestra="datos_muestra.txt",datos_eventos="datos_eventos.txt",
                        fecha_evento, MARKET,inicio,fin,porc_empr_HML=0.3,porc_empr_SMB=0.5, 
                        directorio){
  
  EMP_HML <- EMPRESAS_HML(datos_muestra=datos_muestra, datos_eventos=datos_eventos, 
                          fecha_evento=fecha_evento,MARKET=MARKET, porc_empr_HML = porc_empr_HML)
  EMP_SMB <- EMPRESAS_SMB(datos_muestra=datos_muestra, datos_eventos=datos_eventos,
                          fecha_evento=fecha_evento, MARKET=MARKET,porc_empr_SMB = porc_empr_SMB)
  dia1 = as.Date(inicio,format='%d/%m/%Y')
  dia2 = as.Date(fin, format='%d/%m/%Y')
  
  #CALCULO PARA EL CONJUNTO S/H
  matrizSH <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSH <- EMP_HML[EMP_HML$HML=="HIGH","EMPRESA"]

  empresasSH <- empresasSH[empresasSH %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/H
  for (i in 1:length(empresasSH)){
    EMPRESA <- as.character(empresasSH[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSH[,EMPRESA] <-  merge(x=matrizSH["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/H
  if(ncol(matrizSH) == 2) {
    matrizSH$AVG_SH <- matrizSH[,2:ncol(matrizSH)]
  }else{
    matrizSH$AVG_SH<- apply(matrizSH[,2:ncol(matrizSH)],1,mean,na.rm=TRUE)
  }

  #CALCULO PARA EL CONJUNTO S/HL (MEDIUM)
  matrizSHL <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSHL <- EMP_HML[EMP_HML$HML=="MEDIUM","EMPRESA"]
  empresasSHL <- empresasSHL[empresasSHL %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/HL
  for (i in 1:length(empresasSHL)){
    EMPRESA <- as.character(empresasSHL[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSHL[,EMPRESA] <-  merge(x=matrizSHL["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/HL
  if(ncol(matrizSHL) == 2) {
    matrizSHL$AVG_SHL <- matrizSHL[,2:ncol(matrizSHL)]
  }else{
    matrizSHL$AVG_SHL<- apply(matrizSHL[,2:ncol(matrizSHL)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO S/L
  matrizSL <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSL <- EMP_HML[EMP_HML$HML=="LOW","EMPRESA"]
  empresasSL <- empresasSL[empresasSL %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/L
  for (i in 1:length(empresasSL)){
    EMPRESA <- as.character(empresasSL[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSL[,EMPRESA] <-  merge(x=matrizSL["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/L
  if(ncol(matrizSL) == 2) {
    matrizSL$AVG_SL <- matrizSL[,2:ncol(matrizSL)]
  }else{
    matrizSL$AVG_SL<- apply(matrizSL[,2:ncol(matrizSL)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO B/H
  matrizBH <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBH <- EMP_HML[EMP_HML$HML=="HIGH","EMPRESA"]
  empresasBH <- empresasBH[empresasBH %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/H
  for (i in 1:length(empresasBH)){
    EMPRESA <- as.character(empresasBH[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBH[,EMPRESA] <-  merge(x=matrizBH["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/H
  if(ncol(matrizBH) == 2) {
    matrizBH$AVG_BH <- matrizBH[,2:ncol(matrizBH)]
  }else{
    matrizBH$AVG_BH<- apply(matrizBH[,2:ncol(matrizBH)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO B/HL (MEDIUM)
  matrizBHL <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBHL <- EMP_HML[EMP_HML$HML=="MEDIUM","EMPRESA"]
  empresasBHL <- empresasBHL[empresasBHL %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/HL
  for (i in 1:length(empresasBHL)){
    EMPRESA <- as.character(empresasBHL[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBHL[,EMPRESA] <-  merge(x=matrizBHL["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/HL
  if(ncol(matrizBHL) == 2) {
    matrizBHL$AVG_BHL <- matrizBHL[,2:ncol(matrizBHL)]
  }else{
    matrizBHL$AVG_BHL<- apply(matrizBHL[,2:ncol(matrizBHL)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO B/L
  matrizBL <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBL <- EMP_HML[EMP_HML$HML=="LOW","EMPRESA"]
  empresasBL <- empresasBL[empresasBL %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/L
  for (i in 1:length(empresasBL)){
    EMPRESA <- as.character(empresasBL[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBL[,EMPRESA] <-  merge(x=matrizBL["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/L
  if(ncol(matrizBL) == 2) {
    matrizBL$AVG_BL <- matrizBL[,2:ncol(matrizBL)]
  }else{
    matrizBL$AVG_BL<- apply(matrizBL[,2:ncol(matrizBL)],1,mean,na.rm=TRUE)
  }

  matrizHML <- data.frame(Date=matrizSH$Date,AVG_SH=matrizSH$AVG_SH,
                          AVG_SHL=matrizSHL$AVG_SHL,AVG_SL=matrizSL$AVG_SL,
                          AVG_BH=matrizBH$AVG_BH,AVG_BHL=matrizBHL$AVG_BHL,
                          AVG_BL=matrizBL$AVG_BL)
  matrizHML$HML <- (matrizHML$AVG_SH+matrizHML$AVG_BH)/2-(matrizHML$AVG_SL+matrizHML$AVG_BL)/2
  #Elimino las filas que tengan HML NA
  # matrizHML <- matrizHML[!is.na(matrizHML[,"HML"]),]
  
  return(matrizHML)
  
}

CALCULO_RMW <- function(datos_muestra="datos_muestra.txt",datos_eventos="datos_eventos.txt",
                        fecha_evento, MARKET,inicio,fin,porc_empr_RMW=0.3,porc_empr_SMB=0.5, 
                        directorio){
  
  EMP_RMW <- EMPRESAS_RMW(datos_muestra=datos_muestra, datos_eventos=datos_eventos, 
                          fecha_evento=fecha_evento,MARKET=MARKET, porc_empr_RMW = porc_empr_RMW)
  EMP_SMB <- EMPRESAS_SMB(datos_muestra=datos_muestra, datos_eventos=datos_eventos,
                          fecha_evento=fecha_evento, MARKET=MARKET,porc_empr_SMB = porc_empr_SMB)
  dia1 = as.Date(inicio,format='%d/%m/%Y')
  dia2 = as.Date(fin, format='%d/%m/%Y')
  
  #CALCULO PARA EL CONJUNTO S/R
  matrizSR <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSR <- EMP_RMW[EMP_RMW$RMW=="ROBUST","EMPRESA"]
  empresasSR <- empresasSR[empresasSR %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/R
  for (i in 1:length(empresasSR)){
    EMPRESA <- as.character(empresasSR[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSR[,EMPRESA] <-  merge(x=matrizSR["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/R
  if(ncol(matrizSR) == 2) {
    matrizSR$AVG_SR <- matrizSR[,2:ncol(matrizSR)]
  }else{
    matrizSR$AVG_SR<- apply(matrizSR[,2:ncol(matrizSR)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO S/RW (MEDIUM)
  matrizSRW <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSRW <- EMP_RMW[EMP_RMW$RMW=="MEDIUM","EMPRESA"]
  empresasSRW <- empresasSRW[empresasSRW %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/RW
  for (i in 1:length(empresasSRW)){
    EMPRESA <- as.character(empresasSRW[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSRW[,EMPRESA] <-  merge(x=matrizSRW["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/RW
  if(ncol(matrizSRW) == 2) {
    matrizSRW$AVG_SRW <- matrizSRW[,2:ncol(matrizSRW)]
  }else{
    matrizSRW$AVG_SRW<- apply(matrizSRW[,2:ncol(matrizSRW)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO S/W
  matrizSW <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSW <- EMP_RMW[EMP_RMW$RMW=="WEAK","EMPRESA"]
  empresasSW <- empresasSW[empresasSW %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/W
  for (i in 1:length(empresasSW)){
    EMPRESA <- as.character(empresasSW[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSW[,EMPRESA] <-  merge(x=matrizSW["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/W
  if(ncol(matrizSW) == 2) {
    matrizSW$AVG_SW <- matrizSW[,2:ncol(matrizSW)]
  }else{
    matrizSW$AVG_SW<- apply(matrizSW[,2:ncol(matrizSW)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO B/R
  matrizBR <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBR <- EMP_RMW[EMP_RMW$RMW=="ROBUST","EMPRESA"]
  empresasBR <- empresasBR[empresasBR %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/R
  for (i in 1:length(empresasBR)){
    EMPRESA <- as.character(empresasBR[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBR[,EMPRESA] <-  merge(x=matrizBR["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/R
  if(ncol(matrizBR) == 2) {
    matrizBR$AVG_BR <- matrizBR[,2:ncol(matrizBR)]
  }else{
    matrizBR$AVG_BR<- apply(matrizBR[,2:ncol(matrizBR)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO B/RW (MEDIUM)
  matrizBRW <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBRW <- EMP_RMW[EMP_RMW$RMW=="MEDIUM","EMPRESA"]
  empresasBRW <- empresasBRW[empresasBRW %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/RW
  for (i in 1:length(empresasBRW)){
    EMPRESA <- as.character(empresasBRW[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBRW[,EMPRESA] <-  merge(x=matrizBRW["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/RW
  if(ncol(matrizBRW) == 2) {
    matrizBRW$AVG_BRW <- matrizBRW[,2:ncol(matrizBRW)]
  }else{
    matrizBRW$AVG_BRW<- apply(matrizBRW[,2:ncol(matrizBRW)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO B/W
  matrizBW <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBW <- EMP_RMW[EMP_RMW$RMW=="WEAK","EMPRESA"]
  empresasBW <- empresasBW[empresasBW %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/W
  for (i in 1:length(empresasBW)){
    EMPRESA <- as.character(empresasBW[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBW[,EMPRESA] <-  merge(x=matrizBW["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/W
  if(ncol(matrizBW) == 2) {
    matrizBW$AVG_BW <- matrizBW[,2:ncol(matrizBW)]
  }else{
    matrizBW$AVG_BW<- apply(matrizBW[,2:ncol(matrizBW)],1,mean,na.rm=TRUE)
  }
  
  matrizRMW <- data.frame(Date=matrizSR$Date,AVG_SR=matrizSR$AVG_SR,
                          AVG_SRW=matrizSRW$AVG_SRW,AVG_SW=matrizSW$AVG_SW,
                          AVG_BR=matrizBR$AVG_BR, AVG_BRW=matrizBRW$AVG_BRW,
                          AVG_BW=matrizBW$AVG_BW)
  matrizRMW$RMW <- (matrizRMW$AVG_SR+matrizRMW$AVG_BR)/2-(matrizRMW$AVG_SW+matrizRMW$AVG_BW)/2
 
  return(matrizRMW)
  
}

CALCULO_CMA <- function(datos_muestra="datos_muestra.txt",datos_eventos="datos_eventos.txt",
                        fecha_evento, MARKET,inicio,fin,porc_empr_CMA=0.3,porc_empr_SMB=0.5, 
                        directorio){
  
  EMP_CMA <- EMPRESAS_CMA(datos_muestra=datos_muestra, datos_eventos=datos_eventos, 
                          fecha_evento=fecha_evento,MARKET=MARKET, porc_empr_CMA = porc_empr_CMA)
  EMP_SMB <- EMPRESAS_SMB(datos_muestra=datos_muestra, datos_eventos=datos_eventos,
                          fecha_evento=fecha_evento, MARKET=MARKET,porc_empr_SMB = porc_empr_SMB)
  dia1 = as.Date(inicio,format='%d/%m/%Y')
  dia2 = as.Date(fin, format='%d/%m/%Y')
  
  #CALCULO PARA EL CONJUNTO S/C
  matrizSC <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSC <- EMP_CMA[EMP_CMA$CMA=="CONSERVATIVE","EMPRESA"]
  empresasSC <- empresasSC[empresasSC %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/C
  for (i in 1:length(empresasSC)){
    EMPRESA <- as.character(empresasSC[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSC[,EMPRESA] <-  merge(x=matrizSC["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/C
  if(ncol(matrizSC) == 2) {
    matrizSC$AVG_SC <- matrizSC[,2:ncol(matrizSC)]
  }else{
    matrizSC$AVG_SC<- apply(matrizSC[,2:ncol(matrizSC)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO S/CA (MEDIUM)
  matrizSCA <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSCA <- EMP_CMA[EMP_CMA$CMA=="MEDIUM","EMPRESA"]
  empresasSCA <- empresasSCA[empresasSCA %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/CA
  for (i in 1:length(empresasSCA)){
    EMPRESA <- as.character(empresasSCA[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSCA[,EMPRESA] <-  merge(x=matrizSCA["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/CA
  if(ncol(matrizSCA) == 2) {
    matrizSCA$AVG_SCA <- matrizSCA[,2:ncol(matrizSCA)]
  }else{
    matrizSCA$AVG_SCA<- apply(matrizSCA[,2:ncol(matrizSCA)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO S/A
  matrizSA <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSA <- EMP_CMA[EMP_CMA$CMA=="AGGRESSIVE","EMPRESA"]
  empresasSA <- empresasSA[empresasSA %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/A
  for (i in 1:length(empresasSA)){
    EMPRESA <- as.character(empresasSA[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSA[,EMPRESA] <-  merge(x=matrizSA["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/A
  if(ncol(matrizSA) == 2) {
    matrizSA$AVG_SA <- matrizSA[,2:ncol(matrizSA)]
  }else{
    matrizSA$AVG_SA<- apply(matrizSA[,2:ncol(matrizSA)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO B/C
  matrizBC <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBC <- EMP_CMA[EMP_CMA$CMA=="CONSERVATIVE","EMPRESA"]
  empresasBC <- empresasBC[empresasBC %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/C
  for (i in 1:length(empresasBC)){
    EMPRESA <- as.character(empresasBC[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBC[,EMPRESA] <-  merge(x=matrizBC["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/C
  if(ncol(matrizBC) == 2) {
    matrizBC$AVG_BC <- matrizBC[,2:ncol(matrizBC)]
  }else{
    matrizBC$AVG_BC<- apply(matrizBC[,2:ncol(matrizBC)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO B/CA (MEDIUM)
  matrizBCA <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBCA <- EMP_CMA[EMP_CMA$CMA=="MEDIUM","EMPRESA"]
  empresasBCA <- empresasBCA[empresasBCA %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/CA
  for (i in 1:length(empresasBCA)){
    EMPRESA <- as.character(empresasBCA[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBCA[,EMPRESA] <-  merge(x=matrizBCA["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/CA
  if(ncol(matrizBCA) == 2) {
    matrizBCA$AVG_BCA <- matrizBCA[,2:ncol(matrizBCA)]
  }else{
    matrizBCA$AVG_BCA<- apply(matrizBCA[,2:ncol(matrizBCA)],1,mean,na.rm=TRUE)
  }
  
  #CALCULO PARA EL CONJUNTO B/A
  matrizBA <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBA <- EMP_CMA[EMP_CMA$CMA=="AGGRESSIVE","EMPRESA"]
  empresasBA <- empresasBA[empresasBA %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/A
  for (i in 1:length(empresasBA)){
    EMPRESA <- as.character(empresasBA[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBA[,EMPRESA] <-  merge(x=matrizBA["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/A
  if(ncol(matrizBA) == 2) {
    matrizBA$AVG_BA <- matrizBA[,2:ncol(matrizBA)]
  }else{
    matrizBA$AVG_BA<- apply(matrizBA[,2:ncol(matrizBA)],1,mean,na.rm=TRUE)
  }
  
  matrizCMA <- data.frame(Date=matrizSC$Date,AVG_SC=matrizSC$AVG_SC,
                          AVG_SCA=matrizSCA$AVG_SCA,AVG_SA=matrizSA$AVG_SA,
                          AVG_BC=matrizBC$AVG_BC,AVG_BCA=matrizBCA$AVG_BCA,
                          AVG_BA=matrizBA$AVG_BA)
  matrizCMA$CMA <- (matrizCMA$AVG_SC+matrizCMA$AVG_BC)/2-(matrizCMA$AVG_SA+matrizCMA$AVG_BA)/2
  
  return(matrizCMA)
  
}

CALCULO_UMD <- function(datos_muestra="datos_muestra.txt",datos_eventos="datos_eventos.txt",
                        fecha_evento, MARKET,inicio,fin,porc_empr_UMD=0.3,porc_empr_SMB=0.5, 
                        directorio){
  
  # EMP_UMD <- EMPRESAS_UMD(datos_muestra=datos_muestra, datos_eventos=datos_eventos, 
                          # fecha_evento=fecha_evento,MARKET=MARKET, porc_empr_UMD = porc_empr_UMD)
  EMP_UMD <- EMPRESAS_UMD(fecha_evento=fecha_evento, MARKET=MARKET,datos_muestra=datos_muestra, 
                           datos_eventos=datos_eventos, 
                           porc_empr_UMD = porc_empr_UMD)
  # EMPRESAS_UMD <- function(fecha_evento, MARKET, datos_muestra="datos_muestra.txt", 
                    # datos_eventos="datos_eventos.txt",porc_empr_UMD=0.3){
    
  EMP_SMB <- EMPRESAS_SMB(datos_muestra=datos_muestra, datos_eventos=datos_eventos,
                          fecha_evento=fecha_evento, MARKET=MARKET,porc_empr_SMB = porc_empr_SMB)
  dia1 = as.Date(inicio,format='%d/%m/%Y')
  dia2 = as.Date(fin, format='%d/%m/%Y')
  
  #CALCULO PARA EL CONJUNTO S/D
  matrizSD <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSD <- EMP_UMD[EMP_UMD$UMD=="DOWN","EMPRESA"]

  empresasSD <- empresasSD[empresasSD %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]

  #FOR para recorrer las EMPRESAS del conjunto S/D
  for (i in 1:length(empresasSD)){
    EMPRESA <- as.character(empresasSD[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSD[,EMPRESA] <-  merge(x=matrizSD["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }

  #calculo del promedio para S/D
  if(ncol(matrizSD) == 2) {
    matrizSD$AVG_SD <- matrizSD[,2:ncol(matrizSD)]
  }else{
    matrizSD$AVG_SD<- apply(matrizSD[,2:ncol(matrizSD)],1,mean,na.rm=TRUE)
  }

  #CALCULO PARA EL CONJUNTO S/UD (MEDIUM)
  matrizSUD <- data.frame(Date=seq(dia1,dia2,by=1))

  empresasSUD <- EMP_UMD[EMP_UMD$UMD=="MEDIUM","EMPRESA"]
  empresasSUD <- empresasSUD[empresasSUD %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/UD
  for (i in 1:length(empresasSUD)){
    EMPRESA <- as.character(empresasSUD[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSUD[,EMPRESA] <-  merge(x=matrizSUD["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }

  #calculo del promedio para S/UD
  if(ncol(matrizSUD) == 2) {
    matrizSUD$AVG_SUD <- matrizSUD[,2:ncol(matrizSUD)]
  }else{
    matrizSUD$AVG_SUD<- apply(matrizSUD[,2:ncol(matrizSUD)],1,mean,na.rm=TRUE)
  }

  #CALCULO PARA EL CONJUNTO S/U
  matrizSU <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasSU <- EMP_UMD[EMP_UMD$UMD=="UP","EMPRESA"]
  empresasSU <- empresasSU[empresasSU %in% EMP_SMB[EMP_SMB$SMB=="SMALL","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto S/U
  for (i in 1:length(empresasSU)){
    EMPRESA <- as.character(empresasSU[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizSU[,EMPRESA] <-  merge(x=matrizSU["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para S/U
  if(ncol(matrizSU) == 2) {
    matrizSU$AVG_SU <- matrizSU[,2:ncol(matrizSU)]
  }else{
    matrizSU$AVG_SU<- apply(matrizSU[,2:ncol(matrizSU)],1,mean,na.rm=TRUE)
  }

  #CALCULO PARA EL CONJUNTO B/D
  matrizBD <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBD <- EMP_UMD[EMP_UMD$UMD=="DOWN","EMPRESA"]
  empresasBD <- empresasBD[empresasBD %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/D
  for (i in 1:length(empresasBD)){
    EMPRESA <- as.character(empresasBD[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBD[,EMPRESA] <-  merge(x=matrizBD["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/D
  if(ncol(matrizBD) == 2) {
    matrizBD$AVG_BD <- matrizBD[,2:ncol(matrizBD)]
  }else{
    matrizBD$AVG_BD<- apply(matrizBD[,2:ncol(matrizBD)],1,mean,na.rm=TRUE)
  }

  #CALCULO PARA EL CONJUNTO B/UD (MEDIUM)
  matrizBUD <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBUD <- EMP_UMD[EMP_UMD$UMD=="MEDIUM","EMPRESA"]
  empresasBUD <- empresasBUD[empresasBUD %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/UD
  for (i in 1:length(empresasBUD)){
    EMPRESA <- as.character(empresasBUD[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBUD[,EMPRESA] <-  merge(x=matrizBUD["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/UD
  if(ncol(matrizBUD) == 2) {
    matrizBUD$AVG_BUD <- matrizBUD[,2:ncol(matrizBUD)]
  }else{
    matrizBUD$AVG_BUD<- apply(matrizBUD[,2:ncol(matrizBUD)],1,mean,na.rm=TRUE)
  }

  #CALCULO PARA EL CONJUNTO B/U
  matrizBU <- data.frame(Date=seq(dia1,dia2,by=1))
  empresasBU <- EMP_UMD[EMP_UMD$UMD=="UP","EMPRESA"]
  empresasBU <- empresasBU[empresasBU %in% EMP_SMB[EMP_SMB$SMB=="BIG","EMPRESA"]]
  #FOR para recorrer las EMPRESAs del conjunto B/U
  for (i in 1:length(empresasBU)){
    EMPRESA <- as.character(empresasBU[i])
    datos <- rentabilidadEmpresa(EMPRESA, directorio = directorio)
    matrizBU[,EMPRESA] <-  merge(x=matrizBU["Date"],y=datos[,c("Date",EMPRESA)],by="Date",all.x=TRUE)[,2] 
  }
  #calculo del promedio para B/U
  if(ncol(matrizBU) == 2) {
    matrizBU$AVG_BU <- matrizBU[,2:ncol(matrizBU)]
  }else{
    matrizBU$AVG_BU<- apply(matrizBU[,2:ncol(matrizBU)],1,mean,na.rm=TRUE)
  }

  matrizUMD <- data.frame(Date=matrizSD$Date,AVG_SD=matrizSD$AVG_SD,
                          AVG_SUD=matrizSUD$AVG_SUD,AVG_SU=matrizSU$AVG_SU,
                          AVG_BD=matrizBD$AVG_BD,AVG_BUD=matrizBUD$AVG_BUD,
                          AVG_BU=matrizBU$AVG_BU)
  matrizUMD$UMD <- (matrizUMD$AVG_SU+matrizUMD$AVG_BU)/2-(matrizUMD$AVG_SD+matrizUMD$AVG_BD)/2
  
  return(matrizUMD)
  
}


CALCULO_SMB <- function(datos_muestra="datos_muestra.txt",datos_eventos="datos_eventos.txt",
                        fecha_evento, MARKET,inicio,fin,porc_empr_HML=0.3, porc_empr_RMW=0.3, 
                        porc_empr_CMA=0.3,porc_empr_SMB=0.5,directorio){
  matrizHML <- CALCULO_HML(datos_muestra=datos_muestra, datos_eventos=datos_eventos, fecha_evento=fecha_evento,
              MARKET=MARKET , inicio, fin,porc_empr_HML=porc_empr_HML,porc_empr_SMB=porc_empr_SMB,
              directorio=directorio)
  matrizRMW <- CALCULO_RMW(datos_muestra=datos_muestra, datos_eventos=datos_eventos, fecha_evento=fecha_evento,
              MARKET=MARKET , inicio, fin,porc_empr_RMW=porc_empr_RMW,porc_empr_SMB=porc_empr_SMB,
              directorio=directorio)
  matrizCMA <- CALCULO_CMA(datos_muestra=datos_muestra, datos_eventos=datos_eventos, fecha_evento=fecha_evento,
              MARKET=MARKET , inicio, fin,porc_empr_CMA=porc_empr_CMA,porc_empr_SMB=porc_empr_SMB,
              directorio=directorio)  
  
  matrizSMB <- cbind(matrizHML,matrizRMW[,-1],matrizCMA[,-1])
  matrizSMB$SMBBM <- (matrizHML$AVG_SH+matrizHML$AVG_SHL+matrizHML$AVG_SL)/3-
    (matrizHML$AVG_BH+matrizHML$AVG_BHL+matrizHML$AVG_BL)/3
  matrizSMB$SMBProf <- (matrizRMW$AVG_SR+matrizRMW$AVG_SRW+matrizRMW$AVG_SW)/3-
    (matrizRMW$AVG_BR+matrizRMW$AVG_BRW+matrizRMW$AVG_BW)/3
  matrizSMB$SMBInv <- (matrizCMA$AVG_SC+matrizCMA$AVG_SCA+matrizCMA$AVG_SA)/3-
    (matrizCMA$AVG_BC+matrizCMA$AVG_BCA+matrizCMA$AVG_BA)/3
  matrizSMB$SMB <- (matrizSMB$SMBBM+matrizSMB$SMBProf+matrizSMB$SMBInv)/3
  return(matrizSMB)
  
}


CALCULO_MATRIZKPI <- function (MARKET, COMPANY, fecha_evento, inicio, fin, 
                               datos_muestra="datos_muestra.txt",
                               datos_mercados="datos_mercados.txt",
                               datos_eventos="datos_eventos.txt",
                               valores_estables="Riskfree.txt",
                               porc_empr_SMB=0.5,
                               porc_empr_HML=0.3,
                               porc_empr_UMD=0.3,
                               porc_empr_RMW=0.3, 
                               porc_empr_CMA=0.3,
                               directorio, FD=NULL){
  #Valores estables: valores de un mercado de bajo riesgo, por ejemplo letras del tesoro
  #mercado: mercado donde cotiza la EMPRESA
  # datos <- read.table(datos_mercados, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  datos <- read.table(datos_mercados, na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(datos)[1] <- "Date"
  dia1 = as.Date(inicio,format='%d/%m/%Y')
  dia2 = as.Date(fin, format='%d/%m/%Y')
  MATRIZKPI <- data.frame(Date=seq(dia1,dia2,by=1))

  #Calculamos los valores de rentabilidad diaria para los valores estables y el mercado
  #mercado
  rentabilidad_mercado <- rentabilidadMercado(datos_mercados, MARKET)
  rentabilidad_mercado$Date <- as.Date(rentabilidad_mercado$Date, format="%d/%m/%Y")
  
  MATRIZKPI$RM <- merge(x=MATRIZKPI, y=rentabilidad_mercado[,c("Date","Rentabilidad")], by="Date",all.x=TRUE)[,2]
  #Valores Estables --> Partimos de un documento que ya cuenta con una columna de rentabilidades diarias
  # RF <- read.table(valores_estables,comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  RF <- read.table(valores_estables,na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(RF) <- c("Date", "RF")
  RF$Date <- as.Date(RF$Date, format= "%d/%m/%Y")

  MATRIZKPI$RF <- merge(x=data.frame(Date=MATRIZKPI[,1]), y=RF[,c("Date","RF")], by="Date",all.x=TRUE)[,2]

  #Calculamos los valores de rentabilidad para la EMPRESA en cuestión
  EMP <- rentabilidadEmpresa(COMPANY,directorio=directorio)
  EMP$Date <- as.Date(EMP$Date,format="%d/%m/%Y")
  colnames(EMP)<-c("Date","PX_LAST","PX_VOLUME","RE")
  MATRIZKPI$RE <- merge(x=MATRIZKPI["Date"], y=EMP[,c("Date","RE")], by="Date",all.x=TRUE)[,2]

  if (is.null(FD)){
      #RM-Rf
      MATRIZKPI$RMRF <- MATRIZKPI$RM - MATRIZKPI$RF
    
      #Añadimos el conjunto SMB,HML y UMD
      #SMB
      SMB <- CALCULO_SMB(datos_muestra=datos_muestra, datos_eventos=datos_eventos, fecha_evento=fecha_evento,MARKET=MARKET , inicio, fin,porc_empr_SMB=porc_empr_SMB,porc_empr_RMW=porc_empr_RMW,porc_empr_HML=porc_empr_HML,porc_empr_CMA=porc_empr_CMA,directorio=directorio)
      MATRIZKPI$SMB <- merge(x=MATRIZKPI["Date"], y=SMB[,c("Date","SMB")], by="Date",all.x=TRUE)[,2]
      #HML
      HML <- CALCULO_HML(datos_muestra=datos_muestra, datos_eventos=datos_eventos, fecha_evento=fecha_evento,MARKET=MARKET , inicio, fin,porc_empr_HML=porc_empr_HML,porc_empr_SMB=porc_empr_SMB,directorio=directorio)
      MATRIZKPI$HML <- merge(x=MATRIZKPI["Date"], y=HML[,c("Date","HML")], by="Date",all.x=TRUE)[,2]
      #UMD
      UMD <- CALCULO_UMD(datos_muestra=datos_muestra, datos_eventos=datos_eventos, fecha_evento=fecha_evento,MARKET=MARKET , inicio, fin,porc_empr_UMD=porc_empr_UMD,porc_empr_SMB=porc_empr_SMB,directorio=directorio)
      MATRIZKPI$UMD <- merge(x=MATRIZKPI["Date"], y=UMD[,c("Date","UMD")], by="Date",all.x=TRUE)[,2]
      #RMW
      RMW <- CALCULO_RMW(datos_muestra=datos_muestra, datos_eventos=datos_eventos, fecha_evento=fecha_evento,MARKET=MARKET , inicio, fin,porc_empr_RMW=porc_empr_RMW,porc_empr_SMB=porc_empr_SMB,directorio=directorio)
      MATRIZKPI$RMW <- merge(x=MATRIZKPI["Date"], y=RMW[,c("Date","RMW")], by="Date",all.x=TRUE)[,2]
      #CMA
      CMA <- CALCULO_CMA(datos_muestra=datos_muestra, datos_eventos=datos_eventos, fecha_evento=fecha_evento,MARKET=MARKET , inicio, fin,porc_empr_CMA=porc_empr_CMA,porc_empr_SMB=porc_empr_SMB,directorio=directorio)
      MATRIZKPI$CMA <- merge(x=MATRIZKPI["Date"], y=CMA[,c("Date","CMA")], by="Date",all.x=TRUE)[,2]
  } else{
      MATRIZKPI$RF <- NULL
      MATRIZKPI <- merge(x=MATRIZKPI,y=FD, all.x=TRUE,by="Date")
  }

  #Rit-Rf
  MATRIZKPI$RitRf <- MATRIZKPI$RE - MATRIZKPI$RF
  
  #Elimino los dias en que HML es NA, serán festivos o fines de semana...
  MATRIZKPI <- MATRIZKPI[!is.na(MATRIZKPI[,"HML"]),]

  if (COMPANY=="3M") MATRIZ <<- MATRIZKPI
  return(MATRIZKPI) 
}


##### FUNCIONES PARA EL CALCULO DE ANORMALIDADES PARA UNA EMPRESA #####
#PRUEBA <- ESTIMACION_3F_EMPRESA(MARKET="IGBM", COMPANY="Acciona.txt", fecha_evento="14/07/2011", inicio="16/11/2010", fin="28/07/2011")
#PRUEBA_CATALANA <- ESTIMACION_3F_EMPRESA( MARKET="IGBM", COMPANY="CatalanaOccidente.txt", fecha_evento="26/04/2012",margen_dias_previo = 250)
#A <- ESTIMACION_3F_EMPRESA(fecha_evento="30/07/2012",COMPANY="SecuoyaGrupo.txt", MARKET="IGBM")
ESTIMACION_3F_EMPRESA <- function(fecha_evento,COMPANY,MARKET,
                                  margen_dias_previo= 300,margen_dias_post= 50,
                                  LIE=170,LVE=10, 
                                  porc_empr_SMB=0.5, porc_empr_HML=0.3,
                                  datos_muestra="datos_muestra.txt",
                                  datos_mercados="datos_mercados.txt", 
                                  valores_estables="Riskfree.txt",
                                  datos_eventos="datos_eventos.txt",
                                  directorio,
                                  FD=NULL
                                  #,inicio, 
                                  #fin
){
  #Como dias de inicio y fin de las ventanas de matrizKPI hago una ventana de margen_dias=200 filas de cotizacion desde el evento,
  #es un aprox, para nada crítico
  
  #EMPRESA
  # datos_EMPRESA <- read.table(paste(directorio,'/',COMPANY,'.txt',sep=''), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  datos_EMPRESA <- read.table(paste0(directorio,'/',COMPANY,'.txt'), na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(datos_EMPRESA) <- c("Date","PX_LAST","PX_VOLUME")
  datos_EMPRESA$Date <- as.Date(datos_EMPRESA$Date, format="%d/%m/%Y")
  #Establezco la fila donde esta el evento dentro del doc de la EMPRESA
  fila_evento <- which(datos_EMPRESA$Date == as.Date(fecha_evento,format="%d/%m/%Y"))
  #Establezco la fecha inicio y fin para pasarlo a la funcion que calcula MATRIZKPI
  inicio <- as.Date(datos_EMPRESA$Date[fila_evento-margen_dias_previo],format= "%d/%m/%Y")
  fin <- as.Date(datos_EMPRESA$Date[fila_evento + margen_dias_post],format= "%d/%m/%Y")

  #Calculo de matrizKPI
  MATRIZKPI <- CALCULO_MATRIZKPI(fecha_evento=fecha_evento, MARKET=MARKET, COMPANY=COMPANY, inicio=inicio, fin=fin, datos_muestra=datos_muestra,
                                 datos_mercados=datos_mercados, valores_estables=valores_estables,datos_eventos=datos_eventos,
                                 porc_empr_SMB=porc_empr_SMB,porc_empr_HML=porc_empr_HML,
                                 directorio=directorio, FD=FD)
  

  #AJUSTE
  fila_evento_matriz <- which(MATRIZKPI$Date == as.Date(fecha_evento,format="%d/%m/%Y"))
  MATRIZ_ESTIMACION <- data.frame(Day = paste("Day", -LIE:LVE, sep=""),
                                  Date=MATRIZKPI$Date[(fila_evento_matriz - LIE):(fila_evento_matriz + LVE)])

  #Incluimos valores que teniamos en la MATRIZKPI y que van a servir para hacer el ajuste...
  MATRIZ_ESTIMACION$RitRf <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RitRf")], by="Date",all.x=TRUE)[,2]

  MATRIZ_ESTIMACION$RM <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RM")], by="Date",all.x=TRUE)[,2]

  MATRIZ_ESTIMACION$RF <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RF")], by="Date",all.x=TRUE)[,2]

  MATRIZ_ESTIMACION$RMRF <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RMRF")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$SMB <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","SMB")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$HML <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","HML")], by="Date",all.x=TRUE)[,2]

  #Cálculo coeficientes de ajuste
  ajuste <- lm(RitRf ~ 0 + RMRF + SMB + HML , 
               data = MATRIZ_ESTIMACION[1:as.numeric(dim(MATRIZ_ESTIMACION)[1]-(LVE*2)),])

  #Aplico los coeficientes del ajuste para la estimación...
  #RirRfEstimado
  MATRIZ_ESTIMACION$RitRfEstim <- predict(ajuste,MATRIZ_ESTIMACION)
  #RitReal
  MATRIZ_ESTIMACION$RE <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RE")], by="Date",all.x=TRUE)[,2]

   #RitEstimado
  MATRIZ_ESTIMACION$RitEstim <- MATRIZ_ESTIMACION$RitRfEstim + MATRIZ_ESTIMACION$RF

  #Anormalidad
  MATRIZ_ESTIMACION$ARIT <- MATRIZ_ESTIMACION$RE - MATRIZ_ESTIMACION$RitEstim

  return(MATRIZ_ESTIMACION)
}

#Prueba
#A <- ESTIMACION_3F_EMPRESA(fecha_evento = "31/03/2017",EMPRESA="ADVEO_GROUP.txt", mercado="IGBM")

ESTIMACION_4F_EMPRESA <- function(fecha_evento,COMPANY,MARKET,
                                  margen_dias_previo= 300,margen_dias_post= 50,
                                  LIE=170, LVE=10,
                                  porc_empr_SMB=0.5, porc_empr_HML=0.3,porc_empr_UMD=0.3,
                                  datos_muestra="datos_muestra.txt",
                                  datos_mercados="datos_mercados.txt", 
                                  valores_estables="Riskfree.txt",
                                  datos_eventos="datos_eventos.txt",
                                  directorio, FD=NULL){
  
  #Como dias de inicio y fin de las ventanas de matrizKPI hago una ventana de margen_dias=200 filas de cotizacion desde el evento,
  #es un aprox, para nada crítico
  
  #EMPRESA
  # datos_EMPRESA <- read.table(paste(directorio,'/',COMPANY,'.txt',sep=""), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  datos_EMPRESA <- read.table(paste(directorio,'/',COMPANY,'.txt',sep=""), na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(datos_EMPRESA) <- c("Date","PX_LAST","PX_VOLUME")
  datos_EMPRESA$Date <- as.Date(datos_EMPRESA$Date, format="%d/%m/%Y")
  #Establezco la fila donde esta el evento dentro del doc de la EMPRESA
  fila_evento <- which(datos_EMPRESA$Date == as.Date(fecha_evento,format="%d/%m/%Y"))
  #Establezco la fecha inicio y fin para pasarlo a la funcion que calcula MATRIZKPI
  inicio <- as.Date(datos_EMPRESA$Date[fila_evento-margen_dias_previo],format= "%d/%m/%Y")
  fin <- as.Date(datos_EMPRESA$Date[fila_evento + margen_dias_post],format= "%d/%m/%Y")
  
  #Calculo de matrizKPI
  
  MATRIZKPI <- CALCULO_MATRIZKPI(fecha_evento=fecha_evento,
                                 MARKET=MARKET, 
                                 COMPANY=COMPANY, 
                                 inicio=inicio, 
                                 fin=fin,
                                 datos_muestra=datos_muestra,
                                 datos_mercados=datos_mercados, 
                                 datos_eventos = datos_eventos,
                                 valores_estables=valores_estables,
                                 porc_empr_SMB=porc_empr_SMB, porc_empr_HML=porc_empr_HML,porc_empr_UMD=porc_empr_UMD,directorio = directorio,
                                 FD=FD)
  
  #AJUSTE
  fila_evento_matriz <- which(MATRIZKPI$Date == as.Date(fecha_evento,format="%d/%m/%Y"))
  MATRIZ_ESTIMACION <- data.frame(Day = paste("Day", -LIE:LVE, sep=""),
                                  Date=MATRIZKPI$Date[(fila_evento_matriz - LIE):(fila_evento_matriz + LVE)])
  
  #Incluimos valores que teniamos en la MATRIZKPI y que van a servir para hacer el ajuste...
  MATRIZ_ESTIMACION$RitRf <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RitRf")], by="Date",all.x=TRUE)[,2]
  
  MATRIZ_ESTIMACION$RM <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RM")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$RF <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RF")], by="Date",all.x=TRUE)[,2]
  
  MATRIZ_ESTIMACION$RMRF <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RMRF")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$SMB <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","SMB")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$HML <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","HML")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$UMD <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","UMD")], by="Date",all.x=TRUE)[,2]
  
  #Cálculo coeficientes de ajuste
  ajuste <- lm(RitRf ~ 0 + RMRF + SMB + HML + UMD , 
               data = MATRIZ_ESTIMACION[1:as.numeric(dim(MATRIZ_ESTIMACION)[1]-(LVE*2)),])

  #Aplico los coeficientes del ajuste para la estimación...
  #RirRfEstimado
  MATRIZ_ESTIMACION$RitRfEstim <- predict(ajuste,MATRIZ_ESTIMACION)
  #RitReal
  MATRIZ_ESTIMACION$RE <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RE")], by="Date",all.x=TRUE)[,2]
  #RitEstimado
  MATRIZ_ESTIMACION$RitEstim <- MATRIZ_ESTIMACION$RitRfEstim + MATRIZ_ESTIMACION$RF
  #YEARrmalidad
  MATRIZ_ESTIMACION$ARIT <- MATRIZ_ESTIMACION$RE - MATRIZ_ESTIMACION$RitEstim
  
  return(MATRIZ_ESTIMACION)
}

#head(ESTIMACION_4F_EMPRESA(fecha_evento = "23/03/2017",EMPRESA="ACS.txt", mercado="IGBM"),15)

ESTIMACION_5F_EMPRESA <- function(fecha_evento,COMPANY,MARKET,
                                  margen_dias_previo= 300,margen_dias_post= 50,
                                  LIE=170,LVE=10,
                                  porc_empr_SMB=0.5, porc_empr_HML=0.3,porc_empr_RMW=0.3, porc_empr_CMA=0.3,
                                  datos_muestra="datos_muestra.txt",
                                  datos_mercados="datos_mercados.txt",
                                  valores_estables="Riskfree.txt",
                                  datos_eventos="datos_eventos.txt", directorio,FD=NULL){
  
  #Como dias de inicio y fin de las ventanas de matrizKPI hago una ventana de margen_dias=200 filas de cotizacion desde el evento,
  #es un aprox, para nada crítico
  
  #EMPRESA
  # datos_EMPRESA <- read.table(paste(directorio,'/',COMPANY,'.txt',sep=""), comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  datos_EMPRESA <- read.table(paste(directorio,'/',COMPANY,'.txt',sep=""), na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  colnames(datos_EMPRESA) <- c("Date","PX_LAST","PX_VOLUME")
  datos_EMPRESA$Date <- as.Date(datos_EMPRESA$Date, format="%d/%m/%Y")
  #Establezco la fila donde esta el evento dentro del doc de la EMPRESA
  fila_evento <- which(datos_EMPRESA$Date == as.Date(fecha_evento,format="%d/%m/%Y"))
  #Establezco la fecha inicio y fin para pasarlo a la funcion que calcula MATRIZKPI
  inicio <- as.Date(datos_EMPRESA$Date[fila_evento-margen_dias_previo],format= "%d/%m/%Y")
  fin <- as.Date(datos_EMPRESA$Date[fila_evento + margen_dias_post],format= "%d/%m/%Y")
  
  #Calculo de matrizKPI
  MATRIZKPI <- CALCULO_MATRIZKPI(fecha_evento=fecha_evento,
                                 MARKET=MARKET, 
                                 COMPANY=COMPANY, 
                                 inicio=inicio, 
                                 fin=fin,
                                 datos_muestra=datos_muestra,
                                 datos_mercados=datos_mercados, 
                                 valores_estables=valores_estables,
                                 datos_eventos=datos_eventos,
                                 porc_empr_SMB=porc_empr_SMB, porc_empr_HML=porc_empr_HML, porc_empr_RMW=porc_empr_RMW,
                                 porc_empr_CMA=porc_empr_CMA, directorio = directorio,
                                 FD=FD)
  #AJUSTE
  fila_evento_matriz <- which(MATRIZKPI$Date == as.Date(fecha_evento,format="%d/%m/%Y"))
  MATRIZ_ESTIMACION <- data.frame(Day = paste("Day", -LIE:LVE, sep=""),
                                  Date=MATRIZKPI$Date[(fila_evento_matriz - LIE):(fila_evento_matriz + LVE)])
  
  #Incluimos valores que teniamos en la MATRIZKPI y que van a servir para hacer el ajuste...
  MATRIZ_ESTIMACION$RitRf <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RitRf")], by="Date",all.x=TRUE)[,2]
  
  MATRIZ_ESTIMACION$RM <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RM")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$RF <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RF")], by="Date",all.x=TRUE)[,2]
  
  MATRIZ_ESTIMACION$RMRF <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RMRF")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$SMB <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","SMB")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$HML <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","HML")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$RMW <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RMW")], by="Date",all.x=TRUE)[,2]
  MATRIZ_ESTIMACION$CMA <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","CMA")], by="Date",all.x=TRUE)[,2]
  
  #Cálculo coeficientes de 
  ajuste <- lm(RitRf ~ 0 + RMRF + SMB + HML + RMW + CMA , 
               data = MATRIZ_ESTIMACION[1:as.numeric(dim(MATRIZ_ESTIMACION)[1]-(LVE*2)),])
  #Aplico los coeficientes del ajuste para la estimación...
  #RirRfEstimado
  MATRIZ_ESTIMACION$RitRfEstim <- predict(ajuste,MATRIZ_ESTIMACION)
  if (COMPANY == "3M"){
    ESTIMACION <<- MATRIZ_ESTIMACION
    AJUSTE5F <<- ajuste
  }
  #RitReal
  MATRIZ_ESTIMACION$RE <- merge(x=MATRIZ_ESTIMACION["Date"], y=MATRIZKPI[,c("Date","RE")], by="Date",all.x=TRUE)[,2]
  #RitEstimado
  MATRIZ_ESTIMACION$RitEstim <- MATRIZ_ESTIMACION$RitRfEstim + MATRIZ_ESTIMACION$RF
  #YEARrmalidad
  MATRIZ_ESTIMACION$ARIT <- MATRIZ_ESTIMACION$RE - MATRIZ_ESTIMACION$RitEstim
  
  return(MATRIZ_ESTIMACION)
}

##### FUNCIONES PARA EL LANZAMIENTO DE CALCULOS ANORMALIDADES POR FACTORES DE TODA UNA MUESTRA #####

#PRUEBA <- ACUMULATIVA_3F()

ACUMULATIVA_3F <- function( margen_dias_previo= 225,margen_dias_post= 50,LIE=170, LVE=10,
                            porc_empr_SMB=0.5, porc_empr_HML=0.3,
                            datos_eventos="datos_eventos.txt",
                            datos_muestra="datos_muestra.txt",
                            datos_mercados="datos_mercados.txt", 
                            valores_estables="Riskfree.txt",
                            format="%d/%m/%Y",
                            directorio,FD=NULL){
  
  #analisis_array <- read.table(datos_eventos, comment.char="",na.strings=c("","#N/A","N/A","NULL","-","NA"),sep="\t",quote="",header=T,dec=".")
  
  #analisis_array$FECHA_EVENTO <- as.Date(analisis_array$FECHA_EVENTO, format="%d/%m/%Y")
  # analisis_array <- analisisRentabilidad(datos=datos_eventos,datos_mercados =  datos_mercados, LIE=LIE, LVE=LVE,format=format,directorio = directorio)
  analisis_array <- analisisRentabilidad(datos=datos_eventos,datos_mercados =  datos_mercados, LIE=LIE, LVE=LVE,directorio = directorio)
  analisis_array <- analisis_array[which(analisis_array$ControlEvento == "OK"),]
  analisis_array <- analisis_array[!is.na(analisis_array$Fecha_LIE), ]
  analisis_array <- analisis_array[!is.na(analisis_array$Fecha_LVE), ]
  MATRIZ_RESULTADOS <- data.frame(Day = paste("Day", -LIE:LVE, sep=""))
  b=2
  for(i in 1:nrow(analisis_array)){
    # i es el contador de filas del documento que contiene los datos de la muestra a analizar
    # b es el contador de columnas de la matriz resultado
    EMPRESA <- as.character(analisis_array[i,"COMPANY"])
    mercado <- as.character(analisis_array[i,"MARKET"])
    fecha_evento <- as.character(analisis_array[i,"Fecha_evento_def"])
    #inicio <- as.character(analisis_array[i,"Fecha_LIE"])
    #fin <- as.character(analisis_array[i,"Fecha_LVE"])
    MATRIZ_RESULTADOS[,b] <- ESTIMACION_3F_EMPRESA(fecha_evento=as.Date(as.Date(as.character(fecha_evento))), 
                                                   COMPANY=EMPRESA, 
                                                   MARKET=mercado,
                                                   datos_muestra = datos_muestra,
                                                   margen_dias_previo = margen_dias_previo,
                                                   margen_dias_post = margen_dias_post,
                                                   LIE=LIE, 
                                                   LVE=LVE, 
                                                   datos_mercados=datos_mercados, 
                                                   valores_estables=valores_estables,
                                                   datos_eventos = datos_eventos,
                                                   #inicio=inicio,
                                                   #fin=fin,
                                                   porc_empr_SMB=porc_empr_SMB, porc_empr_HML=porc_empr_HML,
                                                   directorio = directorio, FD=FD)[,"ARIT"]
    b=b+1
  }
  colnames(MATRIZ_RESULTADOS) <- c("Day",paste(analisis_array[,1], analisis_array[,2],sep="||"))
  return(MATRIZ_RESULTADOS)
}

## 4 FACTORS
#PRUEBA:
#PRUEBA_CATALANA_4F <- ESTIMACION_4F_EMPRESA( MARKET="IGBM", COMPANY="CatalanaOccidente.txt", fecha_evento="26/04/2012",margen_dias_previo = 250)

#PRUEBA4F <- ACUMULATIVA_4F()

ACUMULATIVA_4F <- function(margen_dias_previo= 225,margen_dias_post= 50,
                           LIE=170, LVE=10,
                           porc_empr_SMB=0.5, porc_empr_HML=0.3,porc_empr_UMD=0.3,
                           datos_eventos="datos_eventos.txt",
                           datos_muestra="datos_muestra.txt", 
                           datos_mercados="datos_mercados.txt", 
                           valores_estables="Riskfree.txt",
                           format="%d/%m/%Y", directorio, FD=NULL){
  
  analisis_array <- analisisRentabilidad(datos=datos_eventos,datos_mercados=datos_mercados, LIE=LIE, LVE=LVE,format=format,directorio=directorio)
  analisis_array <- analisis_array[which(analisis_array$ControlEvento == "OK"),]
  analisis_array <- analisis_array[!is.na(analisis_array$Fecha_LIE), ]
  analisis_array <- analisis_array[!is.na(analisis_array$Fecha_LVE), ]
  MATRIZ_RESULTADOS <- data.frame(Day = paste("Day", -LIE:LVE, sep=""))
  MATRIZ_RESULTADOS <- data.frame(Day = paste("Day", -LIE:LVE, sep=""))
  b=2
  for(i in 1:nrow(analisis_array)){
    # i es el contador de filas del documento que contiene los datos de la muestra a analizar
    # b es el contador de columnas de la matriz resultado
    EMPRESA <- as.character(analisis_array[i,"COMPANY"])
    mercado <- as.character(analisis_array[i,"MARKET"])
    fecha_evento <- as.character(analisis_array[i,"Fecha_evento_def"])
    MATRIZ_RESULTADOS[,b] <- ESTIMACION_4F_EMPRESA(fecha_evento=as.Date(as.Date(as.character(fecha_evento))), 
                                                   COMPANY=EMPRESA, 
                                                   MARKET=mercado,
                                                   datos_muestra = datos_muestra,
                                                   margen_dias_previo = margen_dias_previo,
                                                   margen_dias_post = margen_dias_post,
                                                   LIE=LIE,
                                                   LVE=LVE,
                                                   datos_mercados=datos_mercados, 
                                                   datos_eventos = datos_eventos,
                                                   valores_estables=valores_estables,
                                                   porc_empr_SMB=porc_empr_SMB, porc_empr_HML=porc_empr_HML,porc_empr_UMD=porc_empr_UMD,
                                                   directorio=directorio,FD=FD)[,"ARIT"]
    b=b+1
  }
  colnames(MATRIZ_RESULTADOS) <- c("Day",paste(analisis_array[,1], analisis_array[,2],sep="||"))
  return(MATRIZ_RESULTADOS)
}

#Ejemplo:
#ESTIM_5F <- ESTIMACION_5F_EMPRESA(fecha_evento = "23/03/2017",EMPRESA="ACS.txt", mercado="IGBM")
#PRUEBA_CATALANA_5F <- ESTIMACION_5F_EMPRESA( MARKET="IGBM", COMPANY="CatalanaOccidente.txt", fecha_evento="26/04/2012",margen_dias_previo = 250)
#PRUEBA5F <- ACUMULATIVA_5F()

ACUMULATIVA_5F <- function(margen_dias_previo= 225,margen_dias_post= 50,
                           LIE=170, LVE=10,
                           porc_empr_SMB=0.5, porc_empr_HML=0.3,porc_empr_RMW=0.3, porc_empr_CMA=0.3,
                           datos_eventos="datos_eventos.txt",
                           datos_muestra="datos_muestra.txt",
                           datos_mercados="datos_mercados.txt", 
                           valores_estables="Riskfree.txt",
                           directorio, FD=NULL){
  
  analisis_array <- analisisRentabilidad(datos=datos_eventos,datos_mercados = datos_mercados, LIE=LIE, LVE=LVE,format=format,directorio = directorio)
  analisis_array <- analisis_array[which(analisis_array$ControlEvento == "OK"),]
  analisis_array <- analisis_array[!is.na(analisis_array$Fecha_LIE), ]
  analisis_array <- analisis_array[!is.na(analisis_array$Fecha_LVE), ]
  MATRIZ_RESULTADOS <- data.frame(Day = paste("Day", -LIE:LVE, sep=""))
  b=2
  for(i in 1:nrow(analisis_array)){
    # i es el contador de filas del documento que contiene los datos de la muestra a analizar
    # b es el contador de columnas de la matriz resultado
    EMPRESA <- as.character(analisis_array[i,"COMPANY"])
    mercado <- as.character(analisis_array[i,"MARKET"])
    fecha_evento <- as.character(analisis_array[i,"Fecha_evento_def"])
    MATRIZ_RESULTADOS[,b] <- ESTIMACION_5F_EMPRESA(fecha_evento=as.Date(as.Date(as.character(fecha_evento))), 
                                                   COMPANY=EMPRESA, 
                                                   MARKET=mercado,
                                                   datos_muestra = datos_muestra,
                                                   margen_dias_previo = margen_dias_previo, 
                                                   margen_dias_post = margen_dias_post,
                                                   LIE=LIE, LVE=LVE, 
                                                   datos_eventos = datos_eventos,
                                                   datos_mercados=datos_mercados, 
                                                   valores_estables=valores_estables,
                                                   porc_empr_SMB=porc_empr_SMB, 
                                                   porc_empr_HML=porc_empr_HML,
                                                   porc_empr_RMW=porc_empr_RMW,
                                                   porc_empr_CMA=porc_empr_CMA,
                                                   directorio = directorio,
                                                   FD=FD)[,"ARIT"]
    b=b+1
  }
  colnames(MATRIZ_RESULTADOS) <- c("Day",paste(analisis_array[,1], analisis_array[,2],sep="||"))
  return(MATRIZ_RESULTADOS)
}

##### FUNCIONES PARA EL USO DE LOS DATOS DE KEN FRENCH #####

French_3F <- function(margen_dias_previo= 225,margen_dias_post= 50,LIE=170, LVE=10,
                      porc_empr_SMB=0.5, porc_empr_HML=0.3,
                      datos_eventos="datos_eventos.txt",
                      datos_muestra="datos_muestra.txt",
                      datos_mercados="datos_mercados.txt", 
                      valores_estables="Riskfree.txt",
                      format="%d/%m/%Y",
                      directorio,FD=NULL){
  ACUM_3F <- ACUMULATIVA_3F(margen_dias_previo=margen_dias_previo,
                            margen_dias_post=margen_dias_post, LIE=LIE, LVE=LVE,
                            datos_eventos=datos_eventos,datos_muestra=datos_muestra,
                            datos_mercados=datos_mercados,valores_estables=valores_estables,
                            format=format,directorio=directorio,FD=FD)
  
  ACUM_3F[ACUM_3F == 0] <- NA
  ACUM_3F <- ACUM_3F[,colSums(is.na(ACUM_3F))<nrow(ACUM_3F)]
  vol3 <- ACUM_3F
  vol3[,2:ncol(vol3)] <- abs(vol3[,2:ncol(vol3)])
  vol3[,"mean"] <- rowMeans(vol3[,2:ncol(vol3)], na.rm = TRUE)
  ncol <- ncol(ACUM_3F)
  ACUM_3F[,"Mean"] <- rowMeans(ACUM_3F[,2:ncol(ACUM_3F)], na.rm = TRUE)
  num <- (LIE - LVE + 1)
  vol3[,"St Des"] <- sd(vol3[1:num, "mean"],na.rm = TRUE)
  vol3[,"Average"] <- mean(vol3[1:num, "mean"],na.rm = TRUE)
  vol3[,"mean - average"] <- vol3[,"mean"] - vol3[,"Average"]
  ACUM_3F[,"St Des"] <- sd(ACUM_3F[1:num,"Mean"],na.rm = TRUE)
  # result <- shapiro.test(ACUM_3F[1:nrow(ACUM_3F),"Mean"])
  
  if (length(na.omit(ACUM_3F[1:num,"Mean"]))>4 & length(na.omit(vol3[1:num,"mean - average"]))>4){
    result <- lillie.test(ACUM_3F[1:num,"Mean"])
    # resultVol <- shapiro.test(vol3[,"mean - average"])
    resultVol <- lillie.test(vol3[1:num,"mean - average"])
    vol3[,"Normal"] <- ifelse(resultVol$p.value > 0.05, "NORMAL", "NO NORMAL")
    ACUM_3F[,"Normal"] <- ifelse(result$p.value > 0.05, "NORMAL", "NO NORMAL")
    vol3[,"p-value"] <- resultVol$p.value
    ACUM_3F[,"p-value"] <- result$p.value
  } else {
    vol3[,"Normal"] <- "NORMAL"
    ACUM_3F[,"Normal"] <- "NORMAL"
    vol3[,"p-value"] <- NA
    ACUM_3F[,"p-value"] <- NA
  }
  
  if (ACUM_3F[1 ,"Normal"] == "NORMAL") {
    ACUM_3F[,"t-test"] <- ACUM_3F[,"Mean"]/ACUM_3F[1 ,"St Des"]
  } 
  corrado2 <- est_corrado(ACUM_3F, ncol)
  ACUM_3F[,"corrado mean"] <- corrado2[[1]]
  ACUM_3F[,"Sk"] <- corrado2[[2]]
  ACUM_3F[,"corrado value"] <- ACUM_3F[,"corrado mean"]/ACUM_3F[,"Sk"]
  
  if (vol3[1 ,"Normal"] == "NORMAL") {
    vol3[,"t-test"] <- vol3[,"mean - average"]/vol3[1 ,"St Des"]
  } 
  corrado3 <- est_corrado(vol3, ncol)
  vol3[,"corrado mean"] <- corrado3[[1]]
  vol3[,"Sk"] <- corrado3[[2]]
  vol3[,"corrado value"] <- vol3[,"corrado mean"]/vol3[,"Sk"]
  
  return(list(ACUM_3F,vol3))
}

French_4F <- function(margen_dias_previo= 225,margen_dias_post= 50,LIE=170, LVE=10,
                      porc_empr_SMB=0.5, porc_empr_HML=0.3,
                      datos_eventos="datos_eventos.txt",
                      datos_muestra="datos_muestra.txt",
                      datos_mercados="datos_mercados.txt", 
                      valores_estables="Riskfree.txt",
                      format="%d/%m/%Y",
                      directorio,FD=NULL){

  ACUM_4F <- ACUMULATIVA_4F(margen_dias_previo= margen_dias_previo,
                            margen_dias_post=margen_dias_post, 
                            LIE=LIE, LVE=LVE,
                            datos_eventos=datos_eventos,
                            datos_muestra=datos_muestra,
                            datos_mercados=datos_mercados,
                            valores_estables=valores_estables,
                            directorio = directorio,FD=FD)
  ACUM_4F[ACUM_4F == 0] <- NA
  ACUM_4F <- ACUM_4F[,colSums(is.na(ACUM_4F))<nrow(ACUM_4F)]
  vol4 <- ACUM_4F
  vol4[,2:ncol(vol4)] <- abs(vol4[,2:ncol(vol4)])
  vol4[,"mean"] <- rowMeans(vol4[,2:ncol(vol4)], na.rm = TRUE)
  ncol <- ncol(ACUM_4F)
  ACUM_4F[,"Mean"] <- rowMeans(ACUM_4F[,2:ncol(ACUM_4F)], na.rm = TRUE)
  num <- (LIE - LVE + 1)
  vol4[,"St Des"] <- sd(vol4[1:num, "mean"],na.rm = TRUE)
  vol4[,"Average"] <- mean(vol4[1:num, "mean"],na.rm = TRUE)
  vol4[,"mean - average"] <- vol4[,"mean"] - vol4[,"Average"]
  ACUM_4F[,"St Des"] <- sd(ACUM_4F[1:num,"Mean"],na.rm = TRUE)
  # result <- shapiro.test(ACUM_4F[1:nrow(ACUM_4F),"Mean"])

  if (length(na.omit(ACUM_4F[1:num,"Mean"]))>4 & length(na.omit(vol4[1:num,"mean - average"]))>4){
    result <- lillie.test(ACUM_4F[1:num,"Mean"])
    # resultVol <- shapiro.test(vol4[,"mean - average"])
    resultVol <- lillie.test(vol4[1:num,"mean - average"])
    vol4[,"Normal"] <- ifelse(resultVol$p.value > 0.05, "NORMAL", "NO NORMAL")
    ACUM_4F[,"Normal"] <- ifelse(result$p.value > 0.05, "NORMAL", "NO NORMAL")
    vol4[,"p-value"] <- resultVol$p.value
    ACUM_4F[,"p-value"] <- result$p.value
  } else {
    vol4[,"Normal"] <- "NORMAL"
    ACUM_4F[,"Normal"] <- "NORMAL"
    vol4[,"p-value"] <- NA
    ACUM_4F[,"p-value"] <- NA
  }
  if (ACUM_4F[1 ,"Normal"] == "NORMAL") {
    ACUM_4F[,"t-test"] <- ACUM_4F[,"Mean"]/ACUM_4F[1 ,"St Des"]
  }
  corrado2 <- est_corrado(ACUM_4F, ncol)
  ACUM_4F[,"corrado mean"] <- corrado2[[1]]
  ACUM_4F[,"Sk"] <- corrado2[[2]]
  ACUM_4F[,"corrado value"] <- ACUM_4F[,"corrado mean"]/ACUM_4F[,"Sk"]

  if (vol4[1 ,"Normal"] == "NORMAL") {
    vol4[,"t-test"] <- vol4[,"mean - average"]/vol4[1 ,"St Des"]
  } 
  corrado3 <- est_corrado(vol4, ncol)
  vol4[,"corrado mean"] <- corrado3[[1]]
  vol4[,"Sk"] <- corrado3[[2]]
  vol4[,"corrado value"] <- vol4[,"corrado mean"]/vol4[,"Sk"]

  ACUM_4F_ext <<- ACUM_4F
  vol4_ext <<- vol4
  return(list(ACUM_4F,vol4))
}

French_5F <- function(margen_dias_previo= 225,margen_dias_post= 50,LIE=170, LVE=10,
                      porc_empr_SMB=0.5, porc_empr_HML=0.3,
                      datos_eventos="datos_eventos.txt",
                      datos_muestra="datos_muestra.txt",
                      datos_mercados="datos_mercados.txt", 
                      valores_estables="Riskfree.txt",
                      format="%d/%m/%Y",
                      directorio,FD=NULL){
  
  ACUM_5F <- ACUMULATIVA_5F(margen_dias_previo=margen_dias_previo,
                            margen_dias_post=margen_dias_post, LIE=LIE, LVE=LVE,
                            datos_eventos=datos_eventos,
                            datos_muestra=datos_muestra,
                            datos_mercados=datos_mercados,
                            valores_estables=valores_estables,
                            directorio=directorio, FD=FD)
  ACUM_5F <- as.data.frame(ACUM_5F)
  ACUM_5F[ACUM_5F == 0] <- NA
  ACUM_5F <- ACUM_5F[,colSums(is.na(ACUM_5F))<nrow(ACUM_5F)]
  vol5 <- ACUM_5F
  vol5[,2:ncol(vol5)] <- abs(vol5[,2:ncol(vol5)])
  vol5[,"mean"] <- rowMeans(vol5[,2:ncol(vol5)], na.rm = TRUE)
  ncol <- ncol(ACUM_5F)
  ACUM_5F[,"Mean"] <- rowMeans(ACUM_5F[,2:ncol(ACUM_5F)], na.rm = TRUE)
  num <- (LIE - LVE + 1)
  CINCF <<- ACUM_5F
  NUM <<- num
  vol5[,"St Des"] <- sd(vol5[1:num, "mean"],na.rm = TRUE)
  vol5[,"Average"] <- mean(vol5[1:num, "mean"],na.rm = TRUE)
  vol5[,"mean - average"] <- vol5[,"mean"] - vol5[,"Average"]
  ACUM_5F[,"St Des"] <- sd(ACUM_5F[1:num,"Mean"],na.rm = TRUE)
  
  if (length(na.omit(ACUM_5F[1:num,"Mean"]))>4 & length(na.omit(vol5[1:num,"mean - average"]))>4){
    result <- lillie.test(ACUM_5F[1:num,"Mean"])
    RESULTATKS <<- result
    resultVol <- lillie.test(vol5[1:num,"mean - average"])
    vol5[,"Normal"] <- ifelse(resultVol$p.value > 0.05, "NORMAL", "NO NORMAL")
    ACUM_5F[,"Normal"] <- ifelse(result$p.value > 0.05, "NORMAL", "NO NORMAL")
    vol5[,"p-value"] <- resultVol$p.value
    ACUM_5F[,"p-value"] <- result$p.value
  } else {
    vol5[,"Normal"] <- "NORMAL"
    ACUM_5F[,"Normal"] <- "NORMAL"
    vol5[,"p-value"] <- NA
    ACUM_5F[,"p-value"] <- NA
  }
  if (ACUM_5F[1 ,"Normal"] == "NORMAL") {
    ACUM_5F[,"t-test"] <- ACUM_5F[,"Mean"]/ACUM_5F[1 ,"St Des"]
  } 
  corrado2 <- est_corrado(ACUM_5F, ncol)
  ACUM_5F[,"corrado mean"] <- corrado2[[1]]
  ACUM_5F[,"Sk"] <- corrado2[[2]]
  ACUM_5F[,"corrado value"] <- ACUM_5F[,"corrado mean"]/ACUM_5F[,"Sk"]

  if (vol5[1 ,"Normal"] == "NORMAL") {
    vol5[,"t-test"] <- vol5[,"mean - average"]/vol5[1 ,"St Des"]
  }
  corrado3 <- est_corrado(vol5, ncol)
  vol5[,"corrado mean"] <- corrado3[[1]]
  vol5[,"Sk"] <- corrado3[[2]]
  vol5[,"corrado value"] <- vol5[,"corrado mean"]/vol5[,"Sk"]
  
  return(list(ACUM_5F,vol5))
}

##### FUNCIONES PARA EL TEST DE CORRADO #####
est_corrado89 <-function(data, col=NULL) {
  if(is.null(col)) col <- ncol(data)
  
  Corrado <- data[,2:col]
  rs <- rank(Corrado[, 1], na.last = "keep") + (nrow(Corrado) - rank(-Corrado[, 1], na.last = "keep") - rank(Corrado[, 1], na.last = "keep") + 1)/2
  resCorrado <- matrix(rs)
  for (i in 2:ncol(Corrado)) {
    rs <- rank(Corrado[, i], na.last = "keep") + (nrow(Corrado) - rank(-Corrado[, i], na.last = "keep") - rank(Corrado[, i], na.last = "keep") + 1)/2
    resCorrado <- cbind(resCorrado, rs)
  }
  resCorrado <- as.data.frame(resCorrado)
  promedio <- rowMeans(resCorrado, na.rm = TRUE) - mean(colMeans(resCorrado, na.rm = TRUE))
  est <- sqrt(sum((promedio^2)/nrow(resCorrado)))
  return(list(promedio, est))
}

est_corrado <-function(data, col) {
  Corrado <- data[,2:col]
  rs <- rank(Corrado[, 1], na.last = "keep", ties.method = "average")
  U <- rs/(1+length(na.omit(rs)))
  resCorrado <- matrix(U)
  for (i in 2:ncol(Corrado)) {
    rs <- rank(Corrado[, i], na.last = "keep", ties.method = "average")
    U <- rs/(1+length(na.omit(rs)))
    resCorrado <- cbind(resCorrado, U)
  }
  resCorrado <- as.data.frame(resCorrado)
  resCorradoEXT <<- resCorrado
  t3den <- sqrt(mean((rowSums(resCorrado-0.5)/sqrt(rowSums(!is.na(resCorrado))))^2,na.rm=T))
  t3num <- rowSums(resCorrado-0.5,na.rm=T)/(sqrt(rowSums(!is.na(resCorrado))))
  
  return(list(t3num, t3den))
}

##### FUNCIONES PARA LA DESCARGA DE VALORES DE KEN FRENCH #####
download_french_3F <-function(webname) {
  name <- tempfile()
  download.file(webname,name)
  Innerfile <- unzip(name,list=T)
  df <- readLines(unz(name,Innerfile$Name))
  file.remove(name)
  df <- df[grepl("^[0-9]",df)]
  df <- read.table(text=paste(df,collapse = "\n"),dec=".",header=F,
                   na.strings = c("-99.99","-999","NA","","N/A","-"))
  colnames(df) <- c("Date","RMRF","SMB","HML","RF")
  df$Date <- as.Date(as.character(df$Date),format = "%Y%m%d")
  df$RMRF <- as.numeric(df$RMRF)/100
  df$SMB <- as.numeric(df$SMB)/100
  df$HML <- as.numeric(df$HML)/100
  df$RF <- as.numeric(df$RF)/100
  df$CMA <- NA
  df$UMD <- NA
  df$RMW <- NA
  return(df)
}

download_french_4F <-function(webname1,webname2) {
  name <- tempfile()
  download.file(webname1,name)
  Innerfile <- unzip(name,list=T)
  df <- readLines(unz(name,Innerfile$Name))
  file.remove(name)
  df <- df[grepl("^[0-9]",df)]
  df <- read.table(text=paste(df,collapse = "\n"),dec=".",header=F,
                   na.strings = c("-99.99","-999","NA","","N/A","-"))
  colnames(df) <- c("Date","RMRF","SMB","HML","RF")
  df$Date <- as.Date(as.character(df$Date),format = "%Y%m%d")
  df$RMRF <- as.numeric(df$RMRF)/100
  df$SMB <- as.numeric(df$SMB)/100
  df$HML <- as.numeric(df$HML)/100
  df$RF <- as.numeric(df$RF)/100
  df$CMA <- NA
  df$RMW <- NA

  
  name <- tempfile()
  download.file(webname2,name)
  Innerfile <- unzip(name,list=T)
  df2 <- readLines(unz(name,Innerfile$Name))
  file.remove(name)
  df2 <- df2[grepl("^[0-9]",df2)]
  df2 <- read.table(text=paste(df2,collapse = "\n"),dec=".",header=F,
                   na.strings = c("-99.99","-999","NA","","N/A","-"))
  colnames(df2) <- c("Date","UMD")
  df2$Date <- as.Date(as.character(df2$Date),format = "%Y%m%d")
  df2$UMD <- as.numeric(df2$UMD)/100
  df <- merge(df,df2,by="Date")


  return(df)
}

download_french_5F <-function(webname) {
  name <- tempfile()
  download.file(webname,name)
  Innerfile <- unzip(name,list=T)
  df <- readLines(unz(name,Innerfile$Name))
  file.remove(name)
  df <- df[grepl("^[0-9]",df)]
  df <- read.table(text=paste(df,collapse = "\n"),dec=".",header=F,
                   na.strings = c("-99.99","-999","NA","","N/A","-"))
  colnames(df) <- c("Date","RMRF","SMB","HML","RMW","CMA","RF")
  df$Date <- as.Date(as.character(df$Date),format = "%Y%m%d")
  df$RMRF <- as.numeric(df$RMRF)/100
  df$SMB <- as.numeric(df$SMB)/100
  df$HML <- as.numeric(df$HML)/100
  df$RMW <- as.numeric(df$RMW)/100
  df$CMA <- as.numeric(df$CMA)/100
  df$RF <- as.numeric(df$RF)/100
  df$UMD <- NA
  return(df)
}