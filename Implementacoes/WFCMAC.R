source("WFCM.R")

#sempre ter mais de 1 cluster

WFCMAC <- function(nGruposInicial, nDados, cteFuzzy, distanciaMaximaPermitida, dadosMedida, nomeDoConjunto, medirXiebeni = FALSE, medirTempo = FALSE, 
                   FSCoef = FALSE, medirWLI = FALSE, qtdeMaxCluster = FALSE, medirCWB = FALSE, medirDSO = FALSE, medirPC = FALSE, medirPE = FALSE){
    
    streamCSV <- DSD_ReadCSV(nomeDoConjunto, sep=",", header=TRUE, loop=TRUE, class = NULL)
    
    pesos <- NULL
    
    nGrupos <- nGruposInicial
    dadosTotais <- 0
    dadosLidos <- 0
    
    # if(medirXiebeni){
    #   xieBeniWFCMAC <- paste("WFCMAC/xiebeniWFCMAC", as.character(nGrupos), sep = "")
    #   xieBeniWFCMAC <- paste(xieBeniWFCMAC, "grupos", sep = "")
    #   xieBeniWFCMAC <- paste(xieBeniWFCMAC, nomeDoConjunto, sep = "")
    #   if (file.exists(xieBeniWFCMAC)) 
    #     #Delete file if it exist
    #     file.remove(xieBeniWFCMAC)
    # }
    # 
    # if(medirTempo){
    #   tempoWFCMAC <- paste("WFCMAC/tempoWFCMAC", as.character(nGrupos), sep = "")
    #   tempoWFCMAC <- paste(tempoWFCMAC, "grupos", sep = "")
    #   tempoWFCMAC <- paste(tempoWFCMAC, nomeDoConjunto, sep = "")
    #   if (file.exists(tempoWFCMAC)) 
    #     #Delete file if it exist
    #     file.remove(tempoWFCMAC)
    # }
    # if(FSCoef){
    #   fsWFCMAC <- paste("WFCMAC/fsWFCMAC", as.character(nGrupos), sep = "")
    #   fsWFCMAC <- paste(fsWFCMAC, "grupos", sep = "")
    #   fsWFCMAC <- paste(fsWFCMAC, nomeDoConjunto, sep = "")
    #   if (file.exists(fsWFCMAC)) 
    #     #Delete file if it exist
    #     file.remove(fsWFCMAC)
    # }
    # if(medirWLI){
    #   wliWFCMAC <- paste("WFCMAC/wliWFCMAC", as.character(nGrupos), sep = "")
    #   wliWFCMAC <- paste(wliWFCMAC, "grupos", sep = "")  
    #   wliWFCMAC <- paste(wliWFCMAC, nomeDoConjunto, sep = "")
    #   if (file.exists(wliWFCMAC)) 
    #     #Delete file if it exist
    #     file.remove(wliWFCMAC)
    # }
    # if(medirPC){
    #   pcWFCMAC <- paste("WFCMAC/pcWFCMAC", as.character(nGrupos), sep = "")
    #   pcWFCMAC <- paste(pcWFCMAC, "grupos", sep = "")  
    #   pcWFCMAC <- paste(pcWFCMAC, nomeDoConjunto, sep = "")
    #   if (file.exists(pcWFCMAC)) 
    #     #Delete file if it exist
    #     file.remove(pcWFCMAC)
    # }
    # if(medirPE){
    #   peWFCMAC <- paste("WFCMAC/peWFCMAC", as.character(nGrupos), sep = "")
    #   peWFCMAC <- paste(peWFCMAC, "grupos", sep = "")  
    #   peWFCMAC <- paste(peWFCMAC, nomeDoConjunto, sep = "")
    #   if (file.exists(peWFCMAC)) 
    #     #Delete file if it exist
    #     file.remove(peWFCMAC)
    # }
    
    if(medirXiebeni){
      
      xieBeniWFCMAC <- verificaTabela("WFCMAC/xiebeniWFCMAC", nGrupos, nDados, nomeDoConjunto)
      
      #  xieBeniWFCMAC <- paste("WFCMAC/xiebeniWFCMAC", as.character(nGrupos), sep = "")
      #  xieBeniWFCMAC <- paste(xieBeniWFCMAC, "grupos", sep = "")
      #  xieBeniWFCMAC <- paste(xieBeniWFCMAC, as.character(nDados), sep = "")
      #  xieBeniWFCMAC <- paste(xieBeniWFCMAC, "chunk_", sep = "")
      #  xieBeniWFCMAC <- paste(xieBeniWFCMAC, nomeDoConjunto, sep = "")
      #  if (file.exists(xieBeniWFCMAC)) 
      #Delete file if it exist
      #    file.remove(xieBeniWFCMAC)
    }
    
    if(medirTempo){
      
      tempoWFCMAC <- verificaTabela("WFCMAC/tempoWFCMAC", nGrupos, nDados, nomeDoConjunto) 
      
      #tempoWFCMAC <- paste("WFCMAC/tempoWFCMAC", as.character(nGrupos), sep = "")
      #tempoWFCMAC <- paste(tempoWFCMAC, "grupos", sep = "")
      #tempoWFCMAC <- paste(tempoWFCMAC, as.character(nDados), sep = "")
      #tempoWFCMAC <- paste(tempoWFCMAC, "chunk_", sep = "")
      #tempoWFCMAC <- paste(tempoWFCMAC, nomeDoConjunto, sep = "") 
      #if (file.exists(tempoWFCMAC)) 
      #Delete file if it exist
      # file.remove(tempoWFCMAC)
    }
    if(FSCoef){
      
      fsWFCMAC <- verificaTabela("WFCMAC/fsWFCMAC", nGrupos, nDados, nomeDoConjunto) 
      
      #fsWFCMAC <- paste("WFCMAC/fsWFCMAC", as.character(nGrupos), sep = "")
      #fsWFCMAC <- paste(fsWFCMAC, "grupos", sep = "")
      #fsWFCMAC <- paste(fsWFCMAC, nomeDoConjunto, sep = "")
      #if (file.exists(fsWFCMAC)) 
      #Delete file if it exist
      #file.remove(fsWFCMAC)
    }
    if(medirWLI){
      
      wliWFCMAC <- verificaTabela("WFCMAC/wliWFCMAC", nGrupos, nDados, nomeDoConjunto) 
      
      #wliWFCMAC <- paste("WFCMAC/wliWFCMAC", as.character(nGrupos), sep = "")
      #wliWFCMAC <- paste(wliWFCMAC, "grupos", sep = "")  
      #wliWFCMAC <- paste(wliWFCMAC, nomeDoConjunto, sep = "")
      #if (file.exists(wliWFCMAC)) 
      #Delete file if it exist
      # file.remove(wliWFCMAC)
    }
    if(medirPC){
      
      pcWFCMAC <- verificaTabela("WFCMAC/pcWFCMAC", nGrupos, nDados, nomeDoConjunto) 
      
      #pcWFCMAC <- paste("WFCMAC/pcWFCMAC", as.character(nGrupos), sep = "")
      #pcWFCMAC <- paste(pcWFCMAC, "grupos", sep = "")  
      #pcWFCMAC <- paste(pcWFCMAC, nomeDoConjunto, sep = "")
      #if (file.exists(pcWFCMAC)) 
      #Delete file if it exist
      #file.remove(pcWFCMAC)
    }
    if(medirPE){
      
      peWFCMAC <- verificaTabela("WFCMAC/peWFCMAC", nGrupos, nDados, nomeDoConjunto)
      
      #peWFCMAC <- paste("WFCMAC/peWFCMAC", as.character(nGrupos), sep = "")
      #peWFCMAC <- paste(peWFCMAC, "grupos", sep = "")
      #peWFCMAC <- paste(peWFCMAC, nomeDoConjunto, sep = "")
      #if (file.exists(peWFCMAC)) 
      #Delete file if it exist
      #file.remove(peWFCMAC)
    }
    
    while(TRUE && dadosTotais <= 5000){
      
      horarioInicio <- Sys.time()
      
      chunk <- get_points(streamCSV, n= nDados)
      chunk <- data.matrix(chunk, rownames.force = NA)
      pesos <- rep(1, nDados)
      
      
      
      centrosEPesos <- WFCM(chunk, pesos, nGrupos, cteFuzzy, distanciaMaximaPermitida)
      
      centrosAnterioresNormal <- centrosEPesos$centros
      matrizPertinenciaNormal <- centrosEPesos$matrizPertinencia
      xiebeniNormal <- XB(chunk, t(matrizPertinenciaNormal), centrosAnterioresNormal, cteFuzzy)
      
      menosNGrupos <- nGrupos - 1
      xiebeniMenos <- Inf
      if(menosNGrupos > 1){
        
        centrosEPesos <- WFCM(chunk, pesos, menosNGrupos, cteFuzzy, distanciaMaximaPermitida)
        
        centrosAnterioresMenos <- centrosEPesos$centros
        matrizPertinenciaMenos <- centrosEPesos$matrizPertinencia
        xiebeniMenos <- XB(chunk, t(matrizPertinenciaMenos), centrosAnterioresMenos, cteFuzzy)
      }
      
      maisNGrupos <- nGrupos + 1
      
      centrosEPesos <- WFCM(chunk, pesos, maisNGrupos, cteFuzzy, distanciaMaximaPermitida)
      
      centrosAnterioresMais <- centrosEPesos$centros
      matrizPertinenciaMais <- centrosEPesos$matrizPertinencia
      xiebeniMais <- XB(chunk, t(matrizPertinenciaMais), centrosAnterioresMais, cteFuzzy)
      
      horarioFim <- Sys.time()
      tempoExec <- horarioFim - horarioInicio
      
      if(xiebeniNormal < xiebeniMais && xiebeniNormal < xiebeniMenos){
        plotName <- paste("WFCMAC/WFCMAC", as.character(dadosTotais), sep = "")
        plotName <- paste(plotName, "dados", sep = "")
        plotName <- paste(plotName, as.character(nGruposInicial), sep = "")
        plotName <- paste(plotName, "clusters_", sep = "")
        plotName <- paste(plotName, nomeDoConjunto, sep = "")
        plotName <- paste(str_sub(plotName, 1, str_length(plotName)-4), ".png", sep = "")
        plotar(plotName, centrosAnterioresNormal, chunk)
        dadosTotais <- dadosTotais + nDados
        dadosLidos <- dadosLidos + nDados
        
        if(dadosLidos >= dadosMedida){
          dadosLidos<-0
          matrizPertinencia <- centrosEPesos$matrizPertinencia
          if(medirXiebeni){
            #xiebeni <- XB(chunk, t(matrizPertinencia), centrosAnteriores, cteFuzzy)
            print("XieBeni:")
            print(xiebeniNormal)
            write.table(paste(as.character(dadosTotais), as.character(xiebeniNormal), sep = ","), file = xieBeniWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirTempo){
            tempoExec <- horarioFim - horarioInicio
            print(tempoExec)
            write.table(paste(as.character(dadosTotais), as.character(tempoExec), sep = ","), file = tempoWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(FSCoef){
            FS <- FS(chunk, matrizPertinenciaNormal, FSCoef)
            print("FS:")
            print(FS)
            write.table(paste(as.character(dadosTotais), as.character(FS), sep = ","), file = fsWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirWLI){
            WLI <- WLI(chunk, matrizPertinenciaNormal, centrosAnterioresNormal)
            print("WLI:")
            print(WLI)
            write.table(paste(as.character(dadosTotais), as.character(WLI), sep = ","), file = wliWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirPC){
            pc <- PC(matrizPertinenciaNormal)
            print("pc:")
            print(pc)
            write.table(paste(as.character(dadosTotais), as.character(pc), sep = ","), file = pcWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirPE){
            pe <- PE(matrizPertinenciaNormal)
            print("pe:")
            print(pe)
            write.table(paste(as.character(dadosTotais), as.character(pe), sep = ","), file = peWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          
          if(qtdeMaxCluster){
            if(medirCWB){
              scat <- rep(0, qtdeMaxCluster)
              dis <- rep(0, qtdeMaxCluster)
              for(c in 2:qtdeMaxCluster){
                cwbWFCMAC <- paste("WFCMAC/cwbWFCMAC", as.character(dadosTotais), sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, "dados", sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, as.character(nDados), sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, "chunk_", sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, nomeDoConjunto, sep = "")
                if (file.exists(cwbWFCMAC)) 
                  #Delete file if it exist
                  file.remove(cwbWFCMAC)
                temp <- WFCM(chunk, pesos, c, cteFuzzy, distanciaMaximaPermitida)
                scat[c] <- scattering(chunk, temp$matrizPertinencia, temp$centros)
                dis[c] <- functionalDistance(temp$centros)
              }
              CWB <- rep(0, qtdeMaxCluster)
              for(c in 2:qtdeMaxCluster){
                CWB[c] <- CWB(scat[c],dis[c],dis[qtdeMaxCluster])
                write.table(paste(as.character(c), as.character(CWB[c]), sep = ","), file = cwbWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
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
                dsoWFCMAC <- paste("WFCMAC/dsoWFCMAC", as.character(dadosTotais), sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, "dados", sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, as.character(nDados), sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, "chunk_", sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, nomeDoConjunto, sep = "")
                if (file.exists(dsoWFCMAC)) 
                  #Delete file if it exist
                  file.remove(dsoWFCMAC)
                temp <- WFCM(chunk, pesos, c, cteFuzzy, distanciaMaximaPermitida)
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
                write.table(paste(as.character(c), as.character(DSO[c]), sep = ","), file = dsoWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
              }
            }
          }
        }
      }
      else if((xiebeniMenos < xiebeniNormal && xiebeniMenos < xiebeniMais) && menosNGrupos > 1){
        nGrupos <- menosNGrupos
        plotName <- paste("WFCMAC/WFCMAC", as.character(dadosTotais), sep = "")
        plotName <- paste(plotName, "dados", sep = "")
        plotName <- paste(plotName, as.character(nGruposInicial), sep = "")
        plotName <- paste(plotName, "clusters_", sep = "")
        plotName <- paste(plotName, nomeDoConjunto, sep = "")
        plotName <- paste(str_sub(plotName, 1, str_length(plotName)-4), ".png", sep = "")
        plotar(plotName, centrosAnterioresMenos, chunk)
        dadosTotais <- dadosTotais + nDados
        dadosLidos <- dadosLidos + nDados
        
        if(dadosLidos >= dadosMedida){
          dadosLidos<-0
          matrizPertinencia <- centrosEPesos$matrizPertinencia
          if(medirXiebeni){
            #xiebeni <- XB(chunk, t(matrizPertinencia), centrosAnteriores, cteFuzzy)
            print("XieBeni:")
            print(xiebeniMenos)
            write.table(paste(as.character(dadosTotais), as.character(xiebeniMenos), sep = ","), file = xieBeniWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirTempo){
            tempoExec <- horarioFim - horarioInicio
            print(tempoExec)
            write.table(paste(as.character(dadosTotais), as.character(tempoExec), sep = ","), file = tempoWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(FSCoef){
            FS <- FS(chunk, matrizPertinenciaMenos, FSCoef)
            print("FS:")
            print(FS)
            write.table(paste(as.character(dadosTotais), as.character(FS), sep = ","), file = fsWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirWLI){
            WLI <- WLI(chunk, matrizPertinenciaMenos, centrosAnterioresMenos)
            print("WLI:")
            print(WLI)
            write.table(paste(as.character(dadosTotais), as.character(WLI), sep = ","), file = wliWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirPC){
            pc <- PC(matrizPertinenciaMenos)
            print("pc:")
            print(pc)
            write.table(paste(as.character(dadosTotais), as.character(pc), sep = ","), file = pcWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirPE){
            pe <- PE(matrizPertinenciaMenos)
            print("pe:")
            print(pe)
            write.table(paste(as.character(dadosTotais), as.character(pe), sep = ","), file = peWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          
          if(qtdeMaxCluster){
            if(medirCWB){
              scat <- rep(0, qtdeMaxCluster)
              dis <- rep(0, qtdeMaxCluster)
              for(c in 2:qtdeMaxCluster){
                cwbWFCMAC <- paste("WFCMAC/cwbWFCMAC", as.character(dadosTotais), sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, "dados", sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, as.character(nDados), sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, "chunk_", sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, nomeDoConjunto, sep = "")
                if (file.exists(cwbWFCMAC)) 
                  #Delete file if it exist
                  file.remove(cwbWFCMAC)
                temp <- WFCM(chunk, pesos, c, cteFuzzy, distanciaMaximaPermitida)
                scat[c] <- scattering(chunk, temp$matrizPertinencia, temp$centros)
                dis[c] <- functionalDistance(temp$centros)
              }
              CWB <- rep(0, qtdeMaxCluster)
              for(c in 2:qtdeMaxCluster){
                CWB[c] <- CWB(scat[c],dis[c],dis[qtdeMaxCluster])
                write.table(paste(as.character(c), as.character(CWB[c]), sep = ","), file = cwbWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
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
                dsoWFCMAC <- paste("WFCMAC/dsoWFCMAC", as.character(dadosTotais), sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, "dados", sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, as.character(nDados), sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, "chunk_", sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, nomeDoConjunto, sep = "")
                if (file.exists(dsoWFCMAC)) 
                  #Delete file if it exist
                  file.remove(dsoWFCMAC)
                temp <- WFCM(chunk, pesos, c, cteFuzzy, distanciaMaximaPermitida)
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
                write.table(paste(as.character(c), as.character(DSO[c]), sep = ","), file = dsoWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
              }
            }
          }
        }
      }
      else{
        nGrupos <- maisNGrupos
        plotName <- paste("WFCMAC/WFCMAC", as.character(dadosTotais), sep = "")
        plotName <- paste(plotName, "dados", sep = "")
        plotName <- paste(plotName, as.character(nGruposInicial), sep = "")
        plotName <- paste(plotName, "clusters_", sep = "")
        plotName <- paste(plotName, nomeDoConjunto, sep = "")
        plotName <- paste(str_sub(plotName, 1, str_length(plotName)-4), ".png", sep = "")
        plotar(plotName, centrosAnterioresMais, chunk)
        dadosTotais <- dadosTotais + nDados
        dadosLidos <- dadosLidos + nDados
        
        if(dadosLidos >= dadosMedida){
          dadosLidos<-0
          matrizPertinencia <- centrosEPesos$matrizPertinencia
          if(medirXiebeni){
            #xiebeni <- XB(chunk, t(matrizPertinencia), centrosAnteriores, cteFuzzy)
            print("XieBeni:")
            print(xiebeniMenos)
            write.table(paste(as.character(dadosTotais), as.character(xiebeniMais), sep = ","), file = xieBeniWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirTempo){
            tempoExec <- horarioFim - horarioInicio
            print(tempoExec)
            write.table(paste(as.character(dadosTotais), as.character(tempoExec), sep = ","), file = tempoWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(FSCoef){
            FS <- FS(chunk, matrizPertinenciaMais, FSCoef)
            print("FS:")
            print(FS)
            write.table(paste(as.character(dadosTotais), as.character(FS), sep = ","), file = fsWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirWLI){
            WLI <- WLI(chunk, matrizPertinenciaMais, centrosAnterioresMais)
            print("WLI:")
            print(WLI)
            write.table(paste(as.character(dadosTotais), as.character(WLI), sep = ","), file = wliWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirPC){
            pc <- PC(matrizPertinenciaMais)
            print("pc:")
            print(pc)
            write.table(paste(as.character(dadosTotais), as.character(pc), sep = ","), file = pcWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          if(medirPE){
            pe <- PE(matrizPertinenciaMais)
            print("pe:")
            print(pe)
            write.table(paste(as.character(dadosTotais), as.character(pe), sep = ","), file = peWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
          
          if(qtdeMaxCluster){
            if(medirCWB){
              scat <- rep(0, qtdeMaxCluster)
              dis <- rep(0, qtdeMaxCluster)
              for(c in 2:qtdeMaxCluster){
                cwbWFCMAC <- paste("WFCMAC/cwbWFCMAC", as.character(dadosTotais), sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, "dados", sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, as.character(nDados), sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, "chunk_", sep = "")
                cwbWFCMAC <- paste(cwbWFCMAC, nomeDoConjunto, sep = "")
                if (file.exists(cwbWFCMAC)) 
                  #Delete file if it exist
                  file.remove(cwbWFCMAC)
                temp <- WFCM(chunk, pesos, c, cteFuzzy, distanciaMaximaPermitida)
                scat[c] <- scattering(chunk, temp$matrizPertinencia, temp$centros)
                dis[c] <- functionalDistance(temp$centros)
              }
              CWB <- rep(0, qtdeMaxCluster)
              for(c in 2:qtdeMaxCluster){
                CWB[c] <- CWB(scat[c],dis[c],dis[qtdeMaxCluster])
                write.table(paste(as.character(c), as.character(CWB[c]), sep = ","), file = cwbWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
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
                dsoWFCMAC <- paste("WFCMAC/dsoWFCMAC", as.character(dadosTotais), sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, "dados", sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, as.character(nDados), sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, "chunk_", sep = "")
                dsoWFCMAC <- paste(dsoWFCMAC, nomeDoConjunto, sep = "")
                if (file.exists(dsoWFCMAC)) 
                  #Delete file if it exist
                  file.remove(dsoWFCMAC)
                temp <- WFCM(chunk, pesos, c, cteFuzzy, distanciaMaximaPermitida)
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
                write.table(paste(as.character(c), as.character(DSO[c]), sep = ","), file = dsoWFCMAC, sep = ",", append = TRUE, col.names = F, row.names = F)
              }
            }
          }
        }
      }
      
  }
}