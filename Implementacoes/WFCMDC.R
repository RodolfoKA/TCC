
source("WFCM.R")

atribuirPesos <- function(nDados, cteDecaimento){
  pesos <- rep(1, nDados)
  for(i in 2:nDados){
    pesos[i] <- pesos[i-1]*(2**cteDecaimento)
  }
  return(pesos)
}

WFCMDC <- function(nGrupos, nDados, cteFuzzy, distanciaMaximaPermitida, cteDecaimento, dadosMedida, nomeDoConjunto, medirXiebeni = FALSE, medirTempo = FALSE, FSCoef = FALSE,
                   medirWLI = FALSE, qtdeMaxCluster = FALSE, medirCWB = FALSE, medirDSO = FALSE, medirPC = FALSE, medirPE = FALSE){
  
  if(cteDecaimento > 0){
  
    streamCSV <- DSD_ReadCSV(nomeDoConjunto, sep=",", header=TRUE, loop=TRUE, class = NULL)
    
    centrosAnteriores <- NULL
    pesos <- NULL
    pesosTotais <- NULL
    dadosTotais <- 0
    dadosLidos <- 0
    
    # if(medirXiebeni){
    #   xieBeniWFCMDC <- paste("WFCMDC/xiebeniWFCMDC", as.character(nGrupos), sep = "")
    #   xieBeniWFCMDC <- paste(xieBeniWFCMDC, "grupos", sep = "")
    #   xieBeniWFCMDC <- paste(xieBeniWFCMDC, nomeDoConjunto, sep = "")
    #   if (file.exists(xieBeniWFCMDC)) 
    #     #Delete file if it exist
    #     file.remove(xieBeniWFCMDC)
    # }
    # 
    # if(medirTempo){
    #   tempoWFCMDC <- paste("WFCMDC/tempoWFCMDC", as.character(nGrupos), sep = "")
    #   tempoWFCMDC <- paste(tempoWFCMDC, "grupos", sep = "")
    #   tempoWFCMDC <- paste(tempoWFCMDC, nomeDoConjunto, sep = "") 
    #   if (file.exists(tempoWFCMDC)) 
    #     #Delete file if it exist
    #     file.remove(tempoWFCMDC)
    # }
    # if(FSCoef){
    #   fsWFCMDC <- paste("WFCMDC/fsWFCMDC", as.character(nGrupos), sep = "")
    #   fsWFCMDC <- paste(fsWFCMDC, "grupos", sep = "")
    #   fsWFCMDC <- paste(fsWFCMDC, nomeDoConjunto, sep = "")
    #   if (file.exists(fsWFCMDC)) 
    #     #Delete file if it exist
    #     file.remove(fsWFCMDC)
    # }
    # if(medirWLI){
    #   wliWFCMDC <- paste("WFCMDC/wliWFCMDC", as.character(nGrupos), sep = "")
    #   wliWFCMDC <- paste(wliWFCMDC, "grupos", sep = "")  
    #   wliWFCMDC <- paste(wliWFCMDC, nomeDoConjunto, sep = "")
    #   if (file.exists(wliWFCMDC)) 
    #     #Delete file if it exist
    #     file.remove(wliWFCMDC)
    # }
    # if(medirPC){
    #   pcWFCMDC <- paste("WFCMDC/pcWFCMDC", as.character(nGrupos), sep = "")
    #   pcWFCMDC <- paste(pcWFCMDC, "grupos", sep = "")  
    #   pcWFCMDC <- paste(pcWFCMDC, nomeDoConjunto, sep = "")
    #   if (file.exists(pcWFCMDC)) 
    #     #Delete file if it exist
    #     file.remove(pcWFCMDC)
    # }
    # if(medirPE){
    #   peWFCMDC <- paste("WFCMDC/peWFCMDC", as.character(nGrupos), sep = "")
    #   peWFCMDC <- paste(peWFCMDC, "grupos", sep = "")  
    #   peWFCMDC <- paste(peWFCMDC, nomeDoConjunto, sep = "")
    #   if (file.exists(peWFCMDC)) 
    #     #Delete file if it exist
    #     file.remove(peWFCMDC)
    # }
    
    if(medirXiebeni){
      
      xieBeniWFCMDC <- verificaTabela("WFCMDC/xiebeniWFCMDC", nGrupos, nDados, nomeDoConjunto)
      
      #  xieBeniWFCMDC <- paste("WFCMDC/xiebeniWFCMDC", as.character(nGrupos), sep = "")
      #  xieBeniWFCMDC <- paste(xieBeniWFCMDC, "grupos", sep = "")
      #  xieBeniWFCMDC <- paste(xieBeniWFCMDC, as.character(nDados), sep = "")
      #  xieBeniWFCMDC <- paste(xieBeniWFCMDC, "chunk_", sep = "")
      #  xieBeniWFCMDC <- paste(xieBeniWFCMDC, nomeDoConjunto, sep = "")
      #  if (file.exists(xieBeniWFCMDC)) 
      #Delete file if it exist
      #    file.remove(xieBeniWFCMDC)
    }
    
    if(medirTempo){
      
      tempoWFCMDC <- verificaTabela("WFCMDC/tempoWFCMDC", nGrupos, nDados, nomeDoConjunto) 
      
      #tempoWFCMDC <- paste("WFCMDC/tempoWFCMDC", as.character(nGrupos), sep = "")
      #tempoWFCMDC <- paste(tempoWFCMDC, "grupos", sep = "")
      #tempoWFCMDC <- paste(tempoWFCMDC, as.character(nDados), sep = "")
      #tempoWFCMDC <- paste(tempoWFCMDC, "chunk_", sep = "")
      #tempoWFCMDC <- paste(tempoWFCMDC, nomeDoConjunto, sep = "") 
      #if (file.exists(tempoWFCMDC)) 
      #Delete file if it exist
      # file.remove(tempoWFCMDC)
    }
    if(FSCoef){
      
      fsWFCMDC <- verificaTabela("WFCMDC/fsWFCMDC", nGrupos, nDados, nomeDoConjunto) 
      
      #fsWFCMDC <- paste("WFCMDC/fsWFCMDC", as.character(nGrupos), sep = "")
      #fsWFCMDC <- paste(fsWFCMDC, "grupos", sep = "")
      #fsWFCMDC <- paste(fsWFCMDC, nomeDoConjunto, sep = "")
      #if (file.exists(fsWFCMDC)) 
      #Delete file if it exist
      #file.remove(fsWFCMDC)
    }
    if(medirWLI){
      
      wliWFCMDC <- verificaTabela("WFCMDC/wliWFCMDC", nGrupos, nDados, nomeDoConjunto) 
      
      #wliWFCMDC <- paste("WFCMDC/wliWFCMDC", as.character(nGrupos), sep = "")
      #wliWFCMDC <- paste(wliWFCMDC, "grupos", sep = "")  
      #wliWFCMDC <- paste(wliWFCMDC, nomeDoConjunto, sep = "")
      #if (file.exists(wliWFCMDC)) 
      #Delete file if it exist
      # file.remove(wliWFCMDC)
    }
    if(medirPC){
      
      pcWFCMDC <- verificaTabela("WFCMDC/pcWFCMDC", nGrupos, nDados, nomeDoConjunto) 
      
      #pcWFCMDC <- paste("WFCMDC/pcWFCMDC", as.character(nGrupos), sep = "")
      #pcWFCMDC <- paste(pcWFCMDC, "grupos", sep = "")  
      #pcWFCMDC <- paste(pcWFCMDC, nomeDoConjunto, sep = "")
      #if (file.exists(pcWFCMDC)) 
      #Delete file if it exist
      #file.remove(pcWFCMDC)
    }
    if(medirPE){
      
      peWFCMDC <- verificaTabela("WFCMDC/peWFCMDC", nGrupos, nDados, nomeDoConjunto)
      
      #peWFCMDC <- paste("WFCMDC/peWFCMDC", as.character(nGrupos), sep = "")
      #peWFCMDC <- paste(peWFCMDC, "grupos", sep = "")
      #peWFCMDC <- paste(peWFCMDC, nomeDoConjunto, sep = "")
      #if (file.exists(peWFCMDC)) 
      #Delete file if it exist
      #file.remove(peWFCMDC)
    }
    
    while(TRUE && dadosTotais <= 5000){
      
      horarioInicio <- Sys.time()
      
      chunk <- get_points(streamCSV, n= nDados)
      chunk <- data.matrix(chunk, rownames.force = NA)
      if(is.null(pesos)){
        pesos <- atribuirPesos(nDados, cteDecaimento)
        pesosTotais <- pesos
      }
      else{
        pesos <- atribuirPesos(nDados, cteDecaimento)
        pesosTotais <- pesos
        chunk <-rbind(chunk, centrosAnteriores)
        pesosTotais <- c(pesos, pesosCentros)
      }
      
      
      
      centrosEPesos <- WFCM(chunk, pesosTotais, nGrupos, cteFuzzy, distanciaMaximaPermitida)
       
      
      
      centrosAnteriores <- centrosEPesos$centros
      pesosCentros <- centrosEPesos$pesos
      pesosCentros <- pesosCentros/(2**(nDados*cteDecaimento))
      
      #matrizPertinencia <- centrosEPesos$matrizPertinencia
      #xiebeni <- XB(chunk, t(matrizPertinencia), centrosEPesos$centros, cteFuzzy)
      #print("XieBeni:")
      #print(xiebeni)
      #plotar(centrosAnteriores, chunk)
     
      horarioFim <- Sys.time()
      tempoExec <- horarioFim - horarioInicio
      
      dadosTotais <- dadosTotais + nDados
      dadosLidos <- dadosLidos + nDados
      
      if(dadosLidos >= dadosMedida){
        plotName <- paste("WFCMDC/WFCMDC", as.character(dadosTotais), sep = "")
        plotName <- paste(plotName, "dados", sep = "")
        plotName <- paste(plotName, as.character(nGrupos), sep = "")
        plotName <- paste(plotName, "clusters_", sep = "")
        plotName <- paste(plotName, nomeDoConjunto, sep = "")
        plotName <- paste(str_sub(plotName, 1, str_length(plotName)-4), ".png", sep = "")
        plotar(plotName, centrosAnteriores, chunk)
        dadosLidos<-0
        matrizPertinencia <- centrosEPesos$matrizPertinencia
        if(medirXiebeni){
          xiebeni <- XB(chunk, t(matrizPertinencia), centrosAnteriores, cteFuzzy)
          print("XieBeni:")
          print(xiebeni)
          write.table(paste(as.character(dadosTotais), as.character(xiebeni), sep = ","), file = xieBeniWFCMDC, sep = ",", append = TRUE, col.names = F, row.names = F)
        }
        if(medirTempo){
          tempoExec <- horarioFim - horarioInicio
          print(tempoExec)
          write.table(paste(as.character(dadosTotais), as.character(tempoExec), sep = ","), file = tempoWFCMDC, sep = ",", append = TRUE, col.names = F, row.names = F)
        }
        if(FSCoef){
          FS <- FS(chunk, matrizPertinencia, FSCoef)
          print("FS:")
          print(FS)
          write.table(paste(as.character(dadosTotais), as.character(FS), sep = ","), file = fsWFCMDC, sep = ",", append = TRUE, col.names = F, row.names = F)
        }
        if(medirWLI){
          WLI <- WLI(chunk, matrizPertinencia, centrosAnteriores)
          print("WLI:")
          print(WLI)
          write.table(paste(as.character(dadosTotais), as.character(WLI), sep = ","), file = wliWFCMDC, sep = ",", append = TRUE, col.names = F, row.names = F)
        }
        if(medirPC){
          pc <- PC(matrizPertinencia)
          print("PC:")
          print(pc)
          write.table(paste(as.character(dadosTotais), as.character(pc), sep = ","), file = pcWFCMDC, sep = ",", append = TRUE, col.names = F, row.names = F)
        }
        if(medirPE){
          pe <- PE(matrizPertinencia)
          print("PE:")
          print(pe)
          write.table(paste(as.character(dadosTotais), as.character(pe), sep = ","), file = peWFCMDC, sep = ",", append = TRUE, col.names = F, row.names = F)
        }
        
        if(qtdeMaxCluster){
          if(medirCWB){
            scat <- rep(0, qtdeMaxCluster)
            dis <- rep(0, qtdeMaxCluster)
            for(c in 2:qtdeMaxCluster){
              cwbWFCMDC <- paste("WFCMDC/cwbWFCMDC", as.character(dadosTotais), sep = "")
              cwbWFCMDC <- paste(cwbWFCMDC, "dados", sep = "")
              cwbWFCMDC <- paste(cwbWFCMDC, as.character(nDados), sep = "")
              cwbWFCMDC <- paste(cwbWFCMDC, "chunk_", sep = "")
              cwbWFCMDC <- paste(cwbWFCMDC, nomeDoConjunto, sep = "")
              if (file.exists(cwbWFCMDC)) 
                #Delete file if it exist
                file.remove(cwbWFCMDC)
              temp <- WFCM(chunk, pesosTotais, c, cteFuzzy, distanciaMaximaPermitida)
              scat[c] <- scattering(chunk, temp$matrizPertinencia, temp$centros)
              dis[c] <- functionalDistance(temp$centros)
            }
            CWB <- rep(0, qtdeMaxCluster)
            for(c in 2:qtdeMaxCluster){
              CWB[c] <- CWB(scat[c],dis[c],dis[qtdeMaxCluster])
              write.table(paste(as.character(c), as.character(CWB[c]), sep = ","), file = cwbWFCMDC, sep = ",", append = TRUE, col.names = F, row.names = F)
            }
          }
          if(medirDSO){
            disp <- rep(0, qtdeMaxCluster)
            sep <- rep(0, qtdeMaxCluster)
            overlap <- rep(0, qtdeMaxCluster)
            dispMax <- -Inf
            sepMax <- -Inf
            overlapMax <- -Inf
            for(c in 2:qtdeMaxCluster){
              dsoWFCMDC <- paste("WFCMDC/dsoWFCMDC", as.character(dadosTotais), sep = "")
              dsoWFCMDC <- paste(dsoWFCMDC, "dados", sep = "")
              dsoWFCMDC <- paste(dsoWFCMDC, as.character(nDados), sep = "")
              dsoWFCMDC <- paste(dsoWFCMDC, "chunk_", sep = "")
              dsoWFCMDC <- paste(dsoWFCMDC, nomeDoConjunto, sep = "")
              if (file.exists(dsoWFCMDC)) 
                #Delete file if it exist
                file.remove(dsoWFCMDC)
              temp <- WFCM(chunk, pesosTotais, c, cteFuzzy, distanciaMaximaPermitida)
              disp[c] <- dispersion(chunk, temp$centros)
              if(disp[c] > dispMax){
                dispMax <- disp[c]
              }
              sep[c] <- separation(temp$matrizPertinencia)
              if(sep[c] > sepMax){
                sepMax <- sep[c]
              }
              overlap[c] <- overlap(temp$matrizPertinencia)
              if(overlap[c] > overlapMax){
                overlapMax <- overlap[c]
              }
            }
            DSO <- rep(0, qtdeMaxCluster)
            for(c in 2:qtdeMaxCluster){
              DSO[c] <- DSO(disp[c],sep[c],overlap[c],dispMax,sepMax,overlapMax)
              write.table(paste(as.character(c), as.character(DSO[c]), sep = ","), file = dsoWFCMDC, sep = ",", append = TRUE, col.names = F, row.names = F)
            }
          }
        }
      }
      
    }
  }
  else{
    print("Constante <= 0!")
  }
}