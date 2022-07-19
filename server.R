source("global.R")

AN_DOC_VOL <- NULL
ACUM_VOL <- NULL
AN_DOC_RENT <- NULL
ACUM_RENT <- NULL
ACUM_3F <-NULL
ACUM_4F <-NULL
ACUM_5F <-NULL
pathE <- NULL
pathM <- NULL
pathMK <- NULL
pathR <- NULL
myInputDir1 <- NULL
vol5 <- NULL
vol4 <- NULL

est_corrado <-function(data, col=NULL) {
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


server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  console <- reactiveVal('Ready...<\n>')

  volumes = getVolumes()

    observe({
    shinyDirChoose(input, "Directory", roots = volumes(), session =
                     session)
    myInputDir1 <<- parseDirPath(volumes(), input$Directory)
    })
  
  
  observeEvent(input$datos_eventos, {
    pathE <<- input$datos_eventos
    txt <- paste(console(), 'File uploaded for Event Data: ', pathE$name,'<\n>', sep="")
    console(txt)
  })
  
  observeEvent(input$datos_mercados, {
    pathMK <<- input$datos_mercados
    txt <- paste(console(), 'File uploaded for Market Data: ', pathMK$name,'<\n>', sep="")
    console(txt)
})
  
  observeEvent(input$datos_muestra, {
    pathM <<- input$datos_muestra
    txt <- paste(console(), 'File uploaded for Sample Data: ', pathM$name,'<\n>', sep="")
    console(txt)
  })
  
  observeEvent(input$Riskfree, {
    pathR <<- input$Riskfree
    txt <- paste(console(), 'File uploaded for Risk Free: ', pathR$name,'<\n>', sep="")
    console(txt)
  })
  
  
  observeEvent(input$run, {
    txt <- paste(console(), 'Chosen directory: ', myInputDir1,'<\n>', sep="")
    txt <- paste(txt, 'Please wait while the data is being processed to ',basename(myInputDir1),'<\n>', sep="")
    txt <- paste(txt, 'Processing...','<\n>', sep="")
    console(txt)
    withProgress(message = paste('Processing',input$Function, sep=" "), value = 0, {

    if (input$Function == 'Volume events analysis') {
      ## Volume events analysis ####  
      AN_DOC_VOL <-
        ANALISIS_DOC_VOLUMEN(
          datos = pathE$datapath,
          LSPE1 = input$LSPE1,
          LSPE2 = input$LSPE2,
          directorio = myInputDir1
        )
      AN_DOC_VOL_ext <<- AN_DOC_VOL
    } else if (input$Function == 'Abnormal volumes') {
      ## Abnormal volumes ####
      ACUM_VOL <-
        ACUMULATIVA_VOLUMEN_MEDIA(
          datos = pathE$datapath,
          LSPE1 = input$LSPE11,
          LIPE1 = input$LIPE11,
          LSPE2 = input$LSPE22,
          LIPE2 = input$LIPE22,
          LVE = input$LVE,
          directorio = myInputDir1
        )
      ACUM_VOL[ACUM_VOL == 0] <- NA
      ncol <- ncol(ACUM_VOL)
      ACUM_VOL[,"Mean"] <- rowMeans(ACUM_VOL[,2:(ncol(ACUM_VOL))], na.rm = TRUE) 
      ACUM_VOL[,"Mean - 1"] <- ACUM_VOL[,"Mean"] - 1
      num <- (input$LSPE11 - input$LIPE11 + 1) + (input$LSPE22 - input$LIPE22 + 1)
      ACUM_VOL[,"St Des"] <- sd(ACUM_VOL[1:num,"Mean"])
      # result <- shapiro.test(ACUM_VOL[1:nrow(ACUM_VOL),"Mean"])
      # result <- ks.test(ACUM_VOL[1:nrow(ACUM_VOL),"Mean"], pnorm,
      #                   mean(ACUM_VOL[1:nrow(ACUM_VOL),"Mean"]),
      #                   sd(ACUM_VOL[1:nrow(ACUM_VOL),"Mean"]))
      # https://rdrr.io/cran/nortest/man/lillie.test.html (mirar "Notes")
      if (length(na.omit(ACUM_VOL[1:num,"Mean"]))>4){
        result <- lillie.test(ACUM_VOL[1:num,"Mean"])
        ACUM_VOL[,"Normal"] <- ifelse(result$p.value > 0.05, "NORMAL", "NO NORMAL")
        ACUM_VOL[,"p-value"] <- result$p.value
      } else {
        ACUM_VOL[,"Normal"] <- "NORMAL"
        ACUM_VOL[,"p-value"] <- NA
      }
      if (ACUM_VOL[1 ,"Normal"] == "NORMAL") {
        ACUM_VOL[,"t-test"] <- ACUM_VOL[,"Mean - 1"]/ACUM_VOL[1 ,"St Des"]
      } else {
        corrado <- est_corrado(ACUM_VOL, ncol)
        ACUM_VOL[,"corrado mean"] <- corrado[[1]]
        ACUM_VOL[,"Sk"] <- corrado[[2]]
        ACUM_VOL[,"corrado value"] <- ACUM_VOL[,"corrado mean"]/ACUM_VOL[,"Sk"]
      }
      ACUM_VOL_ext <<- ACUM_VOL
    } else if (input$Function == 'Return events analysis') {
      ## Return events analysis ####
      AnalisisDirectorio(directorioDatos = myInputDir1,
                         documentoEventos = pathE$datapath)
      AN_DOC_RENT <-
        analisisRentabilidad(
          datos = pathE$datapath,
          datos_mercados = pathMK$datapath,
          LIE = input$LIE1,
          LVE = input$LVE1,
          directorio = myInputDir1
        )
      AN_DOC_RENT_ext <<- AN_DOC_RENT
    } else if (input$Function == 'Market model') {
      ## Market model ####
      ACUM_RENT <-
        ACUMULATIVA_RENTABILIDAD_MERCADO(
          pathE$datapath,
          datos_mercados = pathMK$datapath,
          LIE = input$LIE2,
          LVE = input$LVE2,
          ventana_pre_evento = input$VPE,
          directorio = myInputDir1
        )
      ACUM_RENT[ACUM_RENT == 0] <- NA
      ACUM_RENT <- ACUM_RENT[,colSums(is.na(ACUM_RENT))<nrow(ACUM_RENT)]
      
      vol2 <- ACUM_RENT
      vol2[,2:ncol(vol2)] <- abs(vol2[,2:ncol(vol2)])
      vol2[,"mean"] <- rowMeans(vol2[,2:ncol(vol2)], na.rm = TRUE)
      ncol <- ncol(ACUM_RENT)
      ACUM_RENT[,"Mean"] <- rowMeans(ACUM_RENT[,2:ncol(ACUM_RENT)], na.rm = TRUE) 
      num <- (input$LIE2 - input$LVE2 + 1)
      vol2[,"St Des"] <- sd(vol2[1:num, "mean"])
      vol2[,"Average"] <- mean(vol2[1:num, "mean"])
      vol2[,"mean - average"] <- vol2[,"mean"] - vol2[,"Average"]
      ACUM_RENT[,"St Des"] <- sd(ACUM_RENT[1:num,"Mean"])
      
      # result <- shapiro.test(ACUM_RENT[1:nrow(ACUM_RENT),"Mean"])
      if (length(na.omit(ACUM_RENT[1:num,"Mean"]))>4 & length(na.omit(vol2[1:num,"mean - average"]))>4){
        result <- lillie.test(ACUM_RENT[1:num,"Mean"])
        # resultVol <- shapiro.test(vol2[,"mean - average"])
        resultVol <- lillie.test(vol2[1:num,"mean - average"])
        vol2[,"Normal"] <- ifelse(resultVol$p.value > 0.05, "NORMAL", "NO NORMAL")
        ACUM_RENT[,"Normal"] <- ifelse(result$p.value > 0.05, "NORMAL", "NO NORMAL")
        vol2[,"p-value"] <- resultVol$p.value
        ACUM_RENT[,"p-value"] <- result$p.value
      } else {
        vol2[,"Normal"] <- "NORMAL"
        ACUM_RENT[,"Normal"] <- "NORMAL"
        vol2[,"p-value"] <- NA
        ACUM_RENT[,"p-value"] <- NA
      }
      
      if (ACUM_RENT[1 ,"Normal"] == "NORMAL") {
        ACUM_RENT[,"t-test"] <- ACUM_RENT[,"Mean"]/ACUM_RENT[1 ,"St Des"]
      } else {
        corrado2 <- est_corrado(ACUM_RENT, ncol)
        ACUM_RENT[,"corrado mean"] <- corrado2[[1]]
        ACUM_RENT[,"Sk"] <- corrado2[[2]]
        ACUM_RENT[,"corrado value"] <- ACUM_RENT[,"corrado mean"]/ACUM_RENT[,"Sk"]
      }
      
      if (vol2[1 ,"Normal"] == "NORMAL") {
        vol2[,"t-test"] <- vol2[,"mean - average"]/vol2[1 ,"St Des"]
      } else {
        corrado3 <- est_corrado(vol2, ncol)
        vol2[,"corrado mean"] <- corrado3[[1]]
        vol2[,"Sk"] <- corrado3[[2]]
        vol2[,"corrado value"] <- vol2[,"corrado mean"]/vol2[,"Sk"]
      }
      ACUM_RENT_ext <<- ACUM_RENT
      vol2_ext <<- vol2

    } else if (input$Function == '3 Factors') {
      ## 3 Factors ####
      ACUM_3F <- ACUMULATIVA_3F(margen_dias_previo= input$DNE ,margen_dias_post= input$DNV, LIE=input$LIE3, LVE=input$LVE3,
                                datos_eventos=pathE$datapath,
                                datos_muestra=pathM$datapath,
                                datos_mercados=pathMK$datapath, 
                                valores_estables=pathR$datapath,
                                directorio = myInputDir1)
      ACUM_3F[ACUM_3F == 0] <- NA
      ACUM_3F <- ACUM_3F[,colSums(is.na(ACUM_3F))<nrow(ACUM_3F)]
      vol3 <- ACUM_3F
      vol3[,2:ncol(vol3)] <- abs(vol3[,2:ncol(vol3)])
      vol3[,"mean"] <- rowMeans(vol3[,2:ncol(vol3)], na.rm = TRUE)
      ncol <- ncol(ACUM_3F)
      ACUM_3F[,"Mean"] <- rowMeans(ACUM_3F[,2:ncol(ACUM_3F)], na.rm = TRUE) 
      num <- (input$LIE3 - input$LVE3 + 1)
      vol3[,"St Des"] <- sd(vol3[1:num, "mean"])
      vol3[,"Average"] <- mean(vol3[1:num, "mean"])
      vol3[,"mean - average"] <- vol3[,"mean"] - vol3[,"Average"]
      ACUM_3F[,"St Des"] <- sd(ACUM_3F[1:num,"Mean"])
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
      } else {
        corrado2 <- est_corrado(ACUM_3F, ncol)
        ACUM_3F[,"corrado mean"] <- corrado2[[1]]
        ACUM_3F[,"Sk"] <- corrado2[[2]]
        ACUM_3F[,"corrado value"] <- ACUM_3F[,"corrado mean"]/ACUM_3F[,"Sk"]
      }
      
      if (vol3[1 ,"Normal"] == "NORMAL") {
        vol3[,"t-test"] <- vol3[,"mean - average"]/vol3[1 ,"St Des"]
      } else {
        corrado3 <- est_corrado(vol3, ncol)
        vol3[,"corrado mean"] <- corrado3[[1]]
        vol3[,"Sk"] <- corrado3[[2]]
        vol3[,"corrado value"] <- vol3[,"corrado mean"]/vol3[,"Sk"]
      }
      ACUM_3F_ext <<- ACUM_3F
      vol3_ext <<- vol3
      
    } else if (input$Function == '4 Factors') {
      ## 4 Factors ####
      ########################
      ACUM_4F <- ACUMULATIVA_4F(margen_dias_previo= input$DNE1G ,margen_dias_post= input$DNV1, LIE=input$LIE4, LVE=input$LVE4,
                                datos_eventos=pathE$datapath,
                                datos_muestra=pathM$datapath,
                                datos_mercados=pathMK$datapath, 
                                valores_estables=pathR$datapath,
                                directorio = myInputDir1)
      ACUM_4F[ACUM_4F == 0] <- NA
      ACUM_4F <- ACUM_4F[,colSums(is.na(ACUM_4F))<nrow(ACUM_4F)]
      vol4 <- ACUM_4F
      vol4[,2:ncol(vol4)] <- abs(vol4[,2:ncol(vol4)])
      vol4[,"mean"] <- rowMeans(vol4[,2:ncol(vol4)], na.rm = TRUE)
      ncol <- ncol(ACUM_4F)
      ACUM_4F[,"Mean"] <- rowMeans(ACUM_4F[,2:ncol(ACUM_4F)], na.rm = TRUE) 
      num <- (input$LIE4 - input$LVE4 + 1)
      vol4[,"St Des"] <- sd(vol4[1:num, "mean"])
      vol4[,"Average"] <- mean(vol4[1:num, "mean"])
      vol4[,"mean - average"] <- vol4[,"mean"] - vol4[,"Average"]
      ACUM_4F[,"St Des"] <- sd(ACUM_4F[1:num,"Mean"])
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
      } else {
        corrado2 <- est_corrado(ACUM_4F, ncol)
        ACUM_4F[,"corrado mean"] <- corrado2[[1]]
        ACUM_4F[,"Sk"] <- corrado2[[2]]
        ACUM_4F[,"corrado value"] <- ACUM_4F[,"corrado mean"]/ACUM_4F[,"Sk"]
      }
      
      if (vol4[1 ,"Normal"] == "NORMAL") {
        vol4[,"t-test"] <- vol4[,"mean - average"]/vol4[1 ,"St Des"]
      } else {
        corrado3 <- est_corrado(vol4, ncol)
        vol4[,"corrado mean"] <- corrado3[[1]]
        vol4[,"Sk"] <- corrado3[[2]]
        vol4[,"corrado value"] <- vol4[,"corrado mean"]/vol4[,"Sk"]
      }
      ACUM_4F_ext <<- ACUM_4F
      vol4_ext <<- vol4
      
      ########################
      # ACUM_4F <- ACUMULATIVA_4F(margen_dias_previo= input$DNE1 ,margen_dias_post= input$DNV1, LIE=input$LIE4, LVE=input$LVE4,
      #                           datos_eventos=pathE$datapath,
      #                           datos_muestra=pathM$datapath,
      #                           datos_mercados=pathMK$datapath, 
      #                           valores_estables=pathR$datapath,
      #                           directorio = myInputDir1)
      # ACUM_4F[ACUM_4F == 0] <- NA
      # ncol <- ncol(ACUM_4F)
      # ACUM_4F[,"Mean"] <- rowMeans(ACUM_4F[,2:ncol(ACUM_4F)], na.rm = TRUE) 
      # num <- (input$LIE4 - input$LVE4 + 1)
      # ACUM_4F[,"St Des"] <- sd(ACUM_4F[1:num,"Mean"])
      # result <- shapiro.test(ACUM_4F[1:nrow(ACUM_4F),"Mean"])
      # ACUM_4F[,"Normal"] <- ifelse(result$p.value > 0.05, "NORMAL", "NO NORMAL")
      # ACUM_4F[,"p-value"] <- result$p.value
      # if (ACUM_4F[1 ,"Normal"] == "NORMAL") {
      #   ACUM_4F[,"t-test"] <- ACUM_4F[,"Mean"]/ACUM_4F[1,"St Des"]
      # } else {
      #   corrado <- est_corrado(ACUM_4F, ncol)
      #   ACUM_4F[,"corrado mean"] <- corrado[[1]]
      #   ACUM_4F[,"Sk"] <- corrado[[2]]
      #   ACUM_4F[,"corrado value"] <- ACUM_4F[,"corrado mean"]/ACUM_4F[,"Sk"]
      # }
      # ACUM_4F_ext <<- ACUM_4F
      
    } else if (input$Function == '5 Factors') {
      ## 5 Factors ####
      #####################
      ACUM_5F <- ACUMULATIVA_5F(margen_dias_previo= input$DNE2 ,margen_dias_post= input$DNV2, LIE=input$LIE5, LVE=input$LVE5,
                                datos_eventos=pathE$datapath,
                                datos_muestra=pathM$datapath,
                                datos_mercados=pathMK$datapath, 
                                valores_estables=pathR$datapath,
                                directorio = myInputDir1)
      ACUM_5F <- as.data.frame(ACUM_5F)
      ACUM_5F[ACUM_5F == 0] <- NA
      ACUM_5F <- ACUM_5F[,colSums(is.na(ACUM_5F))<nrow(ACUM_5F)]
      vol5 <- ACUM_5F
      vol5[,2:ncol(vol5)] <- abs(vol5[,2:ncol(vol5)])
      vol5[,"mean"] <- rowMeans(vol5[,2:ncol(vol5)], na.rm = TRUE)
      ncol <- ncol(ACUM_5F)
      ACUM_5F[,"Mean"] <- rowMeans(ACUM_5F[,2:ncol(ACUM_5F)], na.rm = TRUE) 
      num <- (input$LIE5 - input$LVE5 + 1)
      CINCF <<- ACUM_5F
      NUM <<- num
      vol5[,"St Des"] <- sd(vol5[1:num, "mean"])
      vol5[,"Average"] <- mean(vol5[1:num, "mean"])
      vol5[,"mean - average"] <- vol5[,"mean"] - vol5[,"Average"]
      ACUM_5F[,"St Des"] <- sd(ACUM_5F[1:num,"Mean"])
      # result <- shapiro.test(ACUM_5F[1:nrow(ACUM_5F),"Mean"])
      
      if (length(na.omit(ACUM_5F[1:num,"Mean"]))>4 & length(na.omit(vol5[1:num,"mean - average"]))>4){
        result <- lillie.test(ACUM_5F[1:num,"Mean"])
        RESULTATKS <<- result
        # resultVol <- shapiro.test(vol5[,"mean - average"])
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
      } else {
        corrado2 <- est_corrado(ACUM_5F, ncol)
        ACUM_5F[,"corrado mean"] <- corrado2[[1]]
        ACUM_5F[,"Sk"] <- corrado2[[2]]
        ACUM_5F[,"corrado value"] <- ACUM_5F[,"corrado mean"]/ACUM_5F[,"Sk"]
      }
      
      if (vol5[1 ,"Normal"] == "NORMAL") {
        vol5[,"t-test"] <- vol5[,"mean - average"]/vol5[1 ,"St Des"]
      } else {
        corrado3 <- est_corrado(vol5, ncol)
        vol5[,"corrado mean"] <- corrado3[[1]]
        vol5[,"Sk"] <- corrado3[[2]]
        vol5[,"corrado value"] <- vol5[,"corrado mean"]/vol5[,"Sk"]
      }
      
      ACUM_5F_ext <<- ACUM_5F
      vol5_ext <<- vol5
      
      #####################
      # ACUM_5F <- ACUMULATIVA_5F(margen_dias_previo= input$DNE2 ,margen_dias_post= input$DNV2, LIE=input$LIE5, LVE=input$LVE5,
      #                           datos_eventos=pathE$datapath,
      #                           datos_muestra=pathM$datapath,
      #                           datos_mercados=pathMK$datapath, 
      #                           valores_estables=pathR$datapath,
      #                           directorio = myInputDir1)
      # ACUM_5F[ACUM_5F == 0] <- NA
      # ncol <- ncol(ACUM_5F)
      # ACUM_5F[,"Mean"] <- rowMeans(ACUM_5F[,2:ncol(ACUM_5F)], na.rm = TRUE) 
      # num <- (input$LIE5 - input$LVE5 + 1)
      # ACUM_5F[,"St Des"] <- sd(ACUM_5F[1:num,"Mean"])
      # result <- shapiro.test(ACUM_5F[1:nrow(ACUM_5F),"Mean"])
      # ACUM_5F[,"Normal"] <- ifelse(result$p.value > 0.05, "NORMAL", "NO NORMAL")
      # ACUM_5F[,"p-value"] <- result$p.value
      # if (ACUM_5F[1 ,"Normal"] == "NORMAL") {
      #   ACUM_5F[,"t-test"] <- ACUM_5F[,"Mean"]/ACUM_5F[1 ,"St Des"]
      # } else {
      #   corrado <- est_corrado(ACUM_5F, ncol)
      #   ACUM_5F[,"corrado mean"] <- corrado[[1]]
      #   ACUM_5F[,"Sk"] <- corrado[[2]]
      #   ACUM_5F[,"corrado value"] <- ACUM_5F[,"corrado mean"]/ACUM_5F[,"Sk"]
      # }
      # ACUM_5F_ext <<- ACUM_5F
    } else if (input$Function == 'Global') {
      ## Global ####
      AN_DOC_RENT <-
        analisisRentabilidad(
          datos = pathE$datapath,
          datos_mercados = pathMK$datapath,
          LIE = input$LIE1G,
          LVE = input$LVE1G,
          directorio = myInputDir1
        )
      AN_DOC_RENT_ext <<- AN_DOC_RENT
      
      AN_DOC_VOL <- ANALISIS_DOC_VOLUMEN(
        datos = pathE$datapath,
        LSPE1 = input$LSPE1G,
        LSPE2 = input$LSPE2G,
        directorio = myInputDir1
      )
      AN_DOC_VOL_ext <<- AN_DOC_VOL
      
      ACUM_VOL <-
        ACUMULATIVA_VOLUMEN_MEDIA(
          datos = pathE$datapath,
          LSPE1 = input$LSPE11G,
          LIPE1 = input$LIPE11G,
          LSPE2 = input$LSPE22G,
          LIPE2 = input$LIPE22G,
          LVE = input$LVE,
          directorio = myInputDir1
        )
    
      ACUM_VOL[ACUM_VOL == 0] <- NA
      ACUM_VOL <- ACUM_VOL[,colSums(is.na(ACUM_VOL))<nrow(ACUM_VOL)]
      ncol <- ncol(ACUM_VOL)
      ACUM_VOL[,"Mean"] <- rowMeans(ACUM_VOL[,2:(ncol(ACUM_VOL))], na.rm = TRUE) 
      ACUM_VOL[,"Mean - 1"] <- ACUM_VOL[,"Mean"] - 1
      num <- (input$LSPE11G - input$LIPE11G + 1) + (input$LSPE22G - input$LIPE22G + 1)
      ACUM_VOL[,"St Des"] <- sd(ACUM_VOL[1:num,"Mean"])
      # result <- shapiro.test(ACUM_VOL[1:nrow(ACUM_VOL),"Mean"])
     
      if (length(na.omit(ACUM_VOL[1:num,"Mean"]))>4){
        result <- lillie.test(ACUM_VOL[1:num,"Mean"])
        ACUM_VOL[,"Normal"] <- ifelse(result$p.value > 0.05, "NORMAL", "NO NORMAL")
        ACUM_VOL[,"p-value"] <- result$p.value
      } else {
        ACUM_VOL[,"Normal"] <- "NORMAL"
        ACUM_VOL[,"p-value"] <- NA
      }
      if (ACUM_VOL[1 ,"Normal"] == "NORMAL") {
        ACUM_VOL[,"t-test"] <- ACUM_VOL[,"Mean - 1"]/ACUM_VOL[1 ,"St Des"]
      } else {
        corrado1 <- est_corrado(ACUM_VOL, ncol)
        ACUM_VOL[,"corrado mean"] <- corrado1[[1]]
        ACUM_VOL[,"Sk"] <- corrado1[[2]]
        ACUM_VOL[,"corrado value"] <- ACUM_VOL[,"corrado mean"]/ACUM_VOL[,"Sk"]
      }
      ACUM_VOL_ext <<- ACUM_VOL
      
      ACUM_RENT <-
        ACUMULATIVA_RENTABILIDAD_MERCADO(
          pathE$datapath,
          datos_mercados = pathMK$datapath,
          LIE = input$LIE2,
          LVE = input$LVE2,
          ventana_pre_evento = input$VPE,
          directorio = myInputDir1
        )
      ACUM_RENT[ACUM_RENT == 0] <- NA
      ACUM_RENT <- ACUM_RENT[,colSums(is.na(ACUM_RENT))<nrow(ACUM_RENT)]
      vol2 <- ACUM_RENT
      vol2[,2:ncol(vol2)] <- abs(vol2[,2:ncol(vol2)])
      vol2[,"mean"] <- rowMeans(vol2[,2:ncol(vol2)], na.rm = TRUE)
      ncol <- ncol(ACUM_RENT)
      ACUM_RENT[,"Mean"] <- rowMeans(ACUM_RENT[,2:ncol(ACUM_RENT)], na.rm = TRUE) 
      # num <- (input$LIE2 - input$VPE + 1)?????????
      num <- (input$LIE2G - input$LVE2G + 1)
      vol2[,"St Des"] <- sd(vol2[1:num, "mean"])
      vol2[,"Average"] <- mean(vol2[1:num, "mean"])
      vol2[,"mean - average"] <- vol2[,"mean"] - vol2[,"Average"]
      ACUM_RENT[,"St Des"] <- sd(ACUM_RENT[1:num,"Mean"])
      
      # result <- shapiro.test(ACUM_RENT[1:nrow(ACUM_RENT),"Mean"])
      
      if (length(na.omit(ACUM_RENT[1:num,"Mean"]))>4 & length(na.omit(vol2[1:num,"mean - average"]))>4){
        result <- lillie.test(ACUM_RENT[1:num,"Mean"])
        # resultVol <- shapiro.test(vol2[,"mean - average"])
        resultVol <- lillie.test(vol2[1:num,"mean - average"])
        vol2[,"Normal"] <- ifelse(resultVol$p.value > 0.05, "NORMAL", "NO NORMAL")
        ACUM_RENT[,"Normal"] <- ifelse(result$p.value > 0.05, "NORMAL", "NO NORMAL")
        vol2[,"p-value"] <- resultVol$p.value
        ACUM_RENT[,"p-value"] <- result$p.value
      } else {
        vol2[,"Normal"] <- "NORMAL"
        ACUM_RENT[,"Normal"] <- "NORMAL"
        vol2[,"p-value"] <- NA
        ACUM_RENT[,"p-value"] <- NA
      }
      if (ACUM_RENT[1 ,"Normal"] == "NORMAL") {
        ACUM_RENT[,"t-test"] <- ACUM_RENT[,"Mean"]/ACUM_RENT[1 ,"St Des"]
      } else {
        corrado2 <- est_corrado(ACUM_RENT, ncol)
        ACUM_RENT[,"corrado mean"] <- corrado2[[1]]
        ACUM_RENT[,"Sk"] <- corrado2[[2]]
        ACUM_RENT[,"corrado value"] <- ACUM_RENT[,"corrado mean"]/ACUM_RENT[,"Sk"]
      }
      
      if (vol2[1 ,"Normal"] == "NORMAL") {
        vol2[,"t-test"] <- vol2[,"mean - average"]/vol2[1 ,"St Des"]
      } else {
        corrado3 <- est_corrado(vol2, ncol)
        vol2[,"corrado mean"] <- corrado3[[1]]
        vol2[,"Sk"] <- corrado3[[2]]
        vol2[,"corrado value"] <- vol2[,"corrado mean"]/vol2[,"Sk"]
      }
      ACUM_RENT_ext <<- ACUM_RENT
      vol2_ext <<- vol2
      
      
      ACUM_3F <- ACUMULATIVA_3F(margen_dias_previo= input$DNE ,margen_dias_post= input$DNV, LIE=input$LIE3, LVE=input$LVE3,
                                datos_eventos=pathE$datapath,
                                datos_muestra=pathM$datapath,
                                datos_mercados=pathMK$datapath, 
                                valores_estables=pathR$datapath,
                                directorio = myInputDir1)
      ACUM_3F[ACUM_3F == 0] <- NA
      ACUM_3F <- ACUM_3F[,colSums(is.na(ACUM_3F))<nrow(ACUM_3F)]
      vol3 <- ACUM_3F
      vol3[,2:ncol(vol3)] <- abs(vol3[,2:ncol(vol3)])
      vol3[,"mean"] <- rowMeans(vol3[,2:ncol(vol3)], na.rm = TRUE)
      ncol <- ncol(ACUM_3F)
      ACUM_3F[,"Mean"] <- rowMeans(ACUM_3F[,2:ncol(ACUM_3F)], na.rm = TRUE) 
      num <- (input$LIE3G - input$LVE3G + 1)
      vol3[,"St Des"] <- sd(vol3[1:num, "mean"])
      vol3[,"Average"] <- mean(vol3[1:num, "mean"])
      vol3[,"mean - average"] <- vol3[,"mean"] - vol3[,"Average"]
      ACUM_3F[,"St Des"] <- sd(ACUM_3F[1:num,"Mean"])
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
      } else {
        corrado2 <- est_corrado(ACUM_3F, ncol)
        ACUM_3F[,"corrado mean"] <- corrado2[[1]]
        ACUM_3F[,"Sk"] <- corrado2[[2]]
        ACUM_3F[,"corrado value"] <- ACUM_3F[,"corrado mean"]/ACUM_3F[,"Sk"]
      }
      
      if (vol3[1 ,"Normal"] == "NORMAL") {
        vol3[,"t-test"] <- vol3[,"mean - average"]/vol3[1 ,"St Des"]
      } else {
        corrado3 <- est_corrado(vol3, ncol)
        vol3[,"corrado mean"] <- corrado3[[1]]
        vol3[,"Sk"] <- corrado3[[2]]
        vol3[,"corrado value"] <- vol3[,"corrado mean"]/vol3[,"Sk"]
      }
      ACUM_3F_ext <<- ACUM_3F
      vol3_ext <<- vol3

      ACUM_4F <- ACUMULATIVA_4F(margen_dias_previo= input$DNE1G ,margen_dias_post= input$DNV1G, LIE=input$LIE4G, LVE=input$LVE4G,
                                datos_eventos=pathE$datapath,
                                datos_muestra=pathM$datapath,
                                datos_mercados=pathMK$datapath, 
                                valores_estables=pathR$datapath,
                                directorio = myInputDir1)
      ACUM_4F[ACUM_4F == 0] <- NA
      ACUM_4F <- ACUM_4F[,colSums(is.na(ACUM_4F))<nrow(ACUM_4F)]
      vol4 <- ACUM_4F
      vol4[,2:ncol(vol4)] <- abs(vol4[,2:ncol(vol4)])
      vol4[,"mean"] <- rowMeans(vol4[,2:ncol(vol4)], na.rm = TRUE)
      ncol <- ncol(ACUM_4F)
      ACUM_4F[,"Mean"] <- rowMeans(ACUM_4F[,2:ncol(ACUM_4F)], na.rm = TRUE) 
      num <- (input$LIE4G - input$LVE4G + 1)
      vol4[,"St Des"] <- sd(vol4[1:num, "mean"])
      vol4[,"Average"] <- mean(vol4[1:num, "mean"])
      vol4[,"mean - average"] <- vol4[,"mean"] - vol4[,"Average"]
      ACUM_4F[,"St Des"] <- sd(ACUM_4F[1:num,"Mean"])
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
      } else {
        corrado2 <- est_corrado(ACUM_4F, ncol)
        ACUM_4F[,"corrado mean"] <- corrado2[[1]]
        ACUM_4F[,"Sk"] <- corrado2[[2]]
        ACUM_4F[,"corrado value"] <- ACUM_4F[,"corrado mean"]/ACUM_4F[,"Sk"]
      }
      
      if (vol4[1 ,"Normal"] == "NORMAL") {
        vol4[,"t-test"] <- vol4[,"mean - average"]/vol4[1 ,"St Des"]
      } else {
        corrado3 <- est_corrado(vol4, ncol)
        vol4[,"corrado mean"] <- corrado3[[1]]
        vol4[,"Sk"] <- corrado3[[2]]
        vol4[,"corrado value"] <- vol4[,"corrado mean"]/vol4[,"Sk"]
      }
      ACUM_4F_ext <<- ACUM_4F
      vol4_ext <<- vol4
      
      ACUM_5F <- ACUMULATIVA_5F(margen_dias_previo= input$DNE2G ,margen_dias_post= input$DNV2G, LIE=input$LIE5G, LVE=input$LVE5G,
                                datos_eventos=pathE$datapath,
                                datos_muestra=pathM$datapath,
                                datos_mercados=pathMK$datapath, 
                                valores_estables=pathR$datapath,
                                directorio = myInputDir1)
      ACUM_5F <- as.data.frame(ACUM_5F)
      ACUM_5F[ACUM_5F == 0] <- NA
      ACUM_5F <- ACUM_5F[,colSums(is.na(ACUM_5F))<nrow(ACUM_5F)]
      vol5 <- ACUM_5F
      vol5[,2:ncol(vol5)] <- abs(vol5[,2:ncol(vol5)])
      vol5[,"mean"] <- rowMeans(vol5[,2:ncol(vol5)], na.rm = TRUE)
      ncol <- ncol(ACUM_5F)
      ACUM_5F[,"Mean"] <- rowMeans(ACUM_5F[,2:ncol(ACUM_5F)], na.rm = TRUE) 
      num <- (input$LIE5G - input$LVE5G + 1)
      vol5[,"St Des"] <- sd(vol5[1:num, "mean"])
      vol5[,"Average"] <- mean(vol5[1:num, "mean"])
      vol5[,"mean - average"] <- vol5[,"mean"] - vol5[,"Average"]
      ACUM_5F[,"St Des"] <- sd(ACUM_5F[1:num,"Mean"])
      # result <- shapiro.test(ACUM_5F[1:nrow(ACUM_5F),"Mean"])
      # result <- ks.test(ACUM_5F[1:num,"Mean"], pnorm,
      #                      mean(ACUM_5F[1:num,"Mean"]),
      #                      sd(ACUM_5F[1:num,"Mean"]))
      
      if (length(na.omit(ACUM_5F[1:num,"Mean"]))>4 & length(na.omit(vol5[1:num,"mean - average"]))>4){
        result <- lillie.test(ACUM_5F[1:num,"Mean"])
        # resultVol <- shapiro.test(vol5[,"mean - average"])
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
      } else {
        corrado2 <- est_corrado(ACUM_5F, ncol)
        ACUM_5F[,"corrado mean"] <- corrado2[[1]]
        ACUM_5F[,"Sk"] <- corrado2[[2]]
        ACUM_5F[,"corrado value"] <- ACUM_5F[,"corrado mean"]/ACUM_5F[,"Sk"]
      }
      
      if (vol5[1 ,"Normal"] == "NORMAL") {
        vol5[,"t-test"] <- vol5[,"mean - average"]/vol5[1 ,"St Des"]
      } else {
        corrado3 <- est_corrado(vol5, ncol)
        vol5[,"corrado mean"] <- corrado3[[1]]
        vol5[,"Sk"] <- corrado3[[2]]
        vol5[,"corrado value"] <- vol5[,"corrado mean"]/vol5[,"Sk"]
      }
      
      ACUM_5F_ext <<- ACUM_5F
      vol5_ext <<- vol5
      
    }
      for (n in 1:6){
        incProgress(1/n)
        Sys.sleep(0.5)
      }
    })
    txt <- paste(txt, 'You may now download the data from ',input$Function,' for ',basename(myInputDir1),'<\n>', sep="")
    console(txt)
  })
  
  observeEvent(input$LIE3, {
    updateNumericInput(session, 'DNE', value = ceiling(((
      input$LIE3 / (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LVE3, {
    updateNumericInput(session, 'DNV', value = ceiling(((
      input$LVE3 / (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LIE4, {
    updateNumericInput(session, 'DNE1', value = ceiling(((
      input$LIE4 / (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LVE4, {
    updateNumericInput(session, 'DNV1', value = ceiling(((
      input$LVE4/ (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LIE5, {
    updateNumericInput(session, 'DNE2', value = ceiling(((
      input$LIE5 / (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LVE5, {
    updateNumericInput(session, 'DNV2', value = ceiling(((
      input$LVE5/ (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LIE3G, {
    updateNumericInput(session, 'DNEG', value = ceiling(((
      input$LIE3G / (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LVE3G, {
    updateNumericInput(session, 'DNVG', value = ceiling(((
      input$LVE3G / (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LIE4G, {
    updateNumericInput(session, 'DNE1G', value = ceiling(((
      input$LIE4G / (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LVE4G, {
    updateNumericInput(session, 'DNV1G', value = ceiling(((
      input$LVE4G / (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LIE5G, {
    updateNumericInput(session, 'DNE2G', value = ceiling(((
      input$LIE5G / (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  observeEvent(input$LVE5G, {
    updateNumericInput(session, 'DNV2G', value = ceiling(((
      input$LVE5G/ (365 - 53 * 2)
    ) * 365) + 6))
  })
  
  output$dwn <- downloadHandler(
    filename = function() {
      if (input$Function == 'Volume events analysis') {
        paste("Volume - ",
              basename(myInputDir1),
              ' - ',
              Sys.Date(),
              ".xlsx",
              sep = "")
        
      } else if (input$Function == 'Abnormal volumes') {
        paste("Abnormal Volumes - ",
              basename(myInputDir1),
              ' - ',
              Sys.Date(),
              ".xlsx",
              sep = "")
        
      } else if (input$Function == 'Return events analysis') {
        paste("Return - ",
              basename(myInputDir1),
              ' - ',
              Sys.Date(),
              ".xlsx",
              sep = "")
      } else if (input$Function == 'Market model') {
        paste(
          "Market model - ",
          basename(myInputDir1),
          ' - ',
          Sys.Date(),
          ".xlsx",
          sep = ""
        )
       } else if (input$Function == '3 Factors') {
        paste(
          "3 Factors - ",
          basename(myInputDir1),
          ' - ',
          Sys.Date(),
          ".xlsx",
          sep = ""
        )
       } else if (input$Function == '4 Factors') {
         paste(
           "4 Factors - ",
           basename(myInputDir1),
           ' - ',
           Sys.Date(),
           ".xlsx",
           sep = ""
         )
       }  else if (input$Function == '5 Factors') {
         paste(
           "5 Factors - ",
           basename(myInputDir1),
           ' - ',
           Sys.Date(),
           ".xlsx",
           sep = ""
         )
       } else if (input$Function == 'Global') {
         paste(
           "Global - ",
           basename(myInputDir1),
           ' - ',
           Sys.Date(),
           ".xlsx",
           sep = ""
         )
       }
      
      
    },
    content = function(file) {
      if (input$Function == 'Volume events analysis') {
        write_xlsx(
          list("Volume events analysis"=AN_DOC_VOL_ext),
          file,
          col_names = TRUE
        )
      } else if (input$Function == 'Abnormal volumes') {
        write_xlsx(
          list("Abnormal volumes"=ACUM_VOL_ext),
          file,
          col_names = TRUE
        )
      } else if (input$Function == 'Return events analysis') {
        write_xlsx(
          list("Return events analysis"=AN_DOC_RENT_ext),
          file,
          col_names = TRUE
        )
      } else if (input$Function == 'Market model') {
        write_xlsx(
          list("Market model returns"=ACUM_RENT_ext,
               "Market model volatility"=vol2_ext),
          file,
          col_names = TRUE
        )
      } else if (input$Function == '3 Factors') {
        write_xlsx(
          list("3 Factors returns"=ACUM_3F_ext,
               "3 Factors volatility"=vol3_ext),
          file,
          col_names = TRUE
        )
      } else if (input$Function == '4 Factors') {
        write_xlsx(
          list("4 Factors returns"=ACUM_4F_ext,
               "4 Factors volatility"=vol4_ext),
          file,
          col_names = TRUE
        )
      } else if (input$Function == '5 Factors') {
        write_xlsx(
          list("5 Factors returns"=ACUM_5F_ext,
               "5 Factors volatility"=vol5_ext),
          file,
          col_names = TRUE
        )
      } else if (input$Function == 'Global') {
        write_xlsx(
          list("Volume events analysis"=AN_DOC_VOL_ext,
               "Abnormal volumes"=ACUM_VOL_ext,
               "Return events analysis"=AN_DOC_RENT_ext,
               "Market model returns"=ACUM_RENT_ext,
               "Market model volatility"=vol2_ext,
               "3 Factors returns"=ACUM_3F_ext,
               "3 Factors volatility"=vol3_ext,
               "4 Factors returns"=ACUM_4F_ext,
               "4 Factors volatility"=vol4_ext,
               "5 Factors returns"=ACUM_5F_ext,
               "5 Factors volatility"=vol5_ext),
          file,
          col_names = TRUE
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  output$text2 <- renderText({
    
    '
    <br/>
    In order to properly execute the analysis, we need to follow the steps listed below:<br/>
    <br/>
    - Open the 4 files required for the analysis:<br/>
    <br/>
    &emsp;> Event Data file<br/>
    &emsp;> Sample Data file<br/>
    &emsp;> Market Data file<br/>
    &emsp;> Risk Free file<br/>
    <br/>
    To run the Volume events analysis, Abnormal volumes, and Return events analysis only 
    Event Data file and Stocks Sample folder are required.
    <br/>
    <br/>
    - Each folder needs to be open in its required location<br/>
    - Select the stocks sample folder<br/>
    - Select the analysis to be undertaken and adjust its parameters<br/>
    - Once the parameters are set, the system will automatically calculate the number of trading days to be taken into account<br/>
    - Before proceeding with the results, we need to check for confounding effects. You can check for them in Analysis<br/>
    - Once the data have been analyzed, you may download the results as an Excel file<br/>
    - Data frames are then available for further analysis in R or R Studio<br/>
    '
  })
  
  output$text3 <- renderText({
    
    '
    <br/>
    Event Data: 3 columns separated by tabulations<br/>
    &emsp;> Company name<br/>
    &emsp;> Date of the event (dd/mm/YY format)<br/>
    &emsp;> Market (stock) where it happens<br/>
    <br/>   
    Sample data: 14 columns separated by tabulations<br/>
    &emsp;> Company name<br/>
    &emsp;> Ticker (stock code) of the company<br/>
    &emsp;> Market (stock) where the event takes place<br/>
    &emsp;> Year when it happens (YY format)<br/>
    &emsp;> Starting price (t-1) of the company<br/>
    &emsp;> Closing price (t-1) of the company<br/>
    &emsp;> Market capitalization (t-1) of the company<br/> 
    &emsp;> Total Equity (t-1) of the company<br/>
    &emsp;> Revenues (t-1) of the company<br/>
    &emsp;> Costs of Goods Sold (COGS) (t-1) of the company<br/>
    &emsp;> Selling, General and Administrative Expenses (SG&A) (t-1) of the company<br/>
    &emsp;> Interest expenses (t-1) of the company<br/>
    &emsp;> Assets (t-1) of the company<br/>
    &emsp;> Assets (t-2) of the company<br/>
    <br/>
    Market Data: At least 2 columns separated by tabulations<br/>
    &emsp;> Date (list of days in a period of time where the event is included, dd/mm/YY format)<br/>
    &emsp;> Index value in each day<br/>
    <br/>
    Risk Free: 2 columns separated by tabulations<br/>
    &emsp;> Date (list of days in a period of time where the event is included, dd/mm/YY format)<br/>
    &emsp;> Risk Free Rate of the stock in each day<br/>
    <br/>
    Stocks sample files: 3 columns separated by tabulations<br/>
    &emsp;> Date (list of days in a period of time where the event is included, dd/mm/YY format)<br/>
    &emsp;> Stock closing value per day<br/>
    &emsp;> Number of stocks traded per day<br/>
    <br/>
    The decimal separator symbol used must be the point    
    '
  })
  
  output$text4 <- renderText({
    console()
  })
  
  observeEvent(input$stop, {
    stopApp(returnValue = invisible())
  })
}
