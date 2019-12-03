source("WFCM.R")

#totalDados <- 600
#distanciaMaximaPermitida <- 0.1
#cteFuzzy <- 1.5
#nGrupos <- 3
#nDados <- 30
#nomeDoConjunto <- "conjunto1.csv"

OFCM <- function(totalDados, nGrupos, nDados, cteFuzzy, distanciaMaximaPermitida, nomeDoConjunto, medirXiebeni = FALSE, medirTempo = FALSE,
                 FSCoef = FALSE, medirWLI = FALSE, qtdeMaxCluster = FALSE, medirCWB = FALSE, medirDSO = FALSE, medirPC = FALSE, medirPE = FALSE){
  
  if(medirXiebeni){
  
    xieBeniOFCM <- verificaTabela("OFCM/xiebeniOFCM", nGrupos, nDados, nomeDoConjunto)
    
  #  xieBeniOFCM <- paste("OFCM/xiebeniOFCM", as.character(nGrupos), sep = "")
  #  xieBeniOFCM <- paste(xieBeniOFCM, "grupos", sep = "")
  #  xieBeniOFCM <- paste(xieBeniOFCM, as.character(nDados), sep = "")
  #  xieBeniOFCM <- paste(xieBeniOFCM, "chunk_", sep = "")
  #  xieBeniOFCM <- paste(xieBeniOFCM, nomeDoConjunto, sep = "")
  #  if (file.exists(xieBeniOFCM)) 
      #Delete file if it exist
  #    file.remove(xieBeniOFCM)
  }
  
  if(medirTempo){
    
    tempoOFCM <- verificaTabela("OFCM/tempoOFCM", nGrupos, nDados, nomeDoConjunto) 
    
    #tempoOFCM <- paste("OFCM/tempoOFCM", as.character(nGrupos), sep = "")
    #tempoOFCM <- paste(tempoOFCM, "grupos", sep = "")
    #tempoOFCM <- paste(tempoOFCM, as.character(nDados), sep = "")
    #tempoOFCM <- paste(tempoOFCM, "chunk_", sep = "")
    #tempoOFCM <- paste(tempoOFCM, nomeDoConjunto, sep = "") 
    #if (file.exists(tempoOFCM)) 
      #Delete file if it exist
     # file.remove(tempoOFCM)
  }
  if(FSCoef){
    
    fsOFCM <- verificaTabela("OFCM/fsOFCM", nGrupos, nDados, nomeDoConjunto) 
    
    #fsOFCM <- paste("OFCM/fsOFCM", as.character(nGrupos), sep = "")
    #fsOFCM <- paste(fsOFCM, "grupos", sep = "")
    #fsOFCM <- paste(fsOFCM, nomeDoConjunto, sep = "")
    #if (file.exists(fsOFCM)) 
      #Delete file if it exist
      #file.remove(fsOFCM)
  }
  if(medirWLI){
    
    wliOFCM <- verificaTabela("OFCM/wliOFCM", nGrupos, nDados, nomeDoConjunto) 
    
    #wliOFCM <- paste("OFCM/wliOFCM", as.character(nGrupos), sep = "")
    #wliOFCM <- paste(wliOFCM, "grupos", sep = "")  
    #wliOFCM <- paste(wliOFCM, nomeDoConjunto, sep = "")
    #if (file.exists(wliOFCM)) 
      #Delete file if it exist
     # file.remove(wliOFCM)
  }
  if(medirPC){
    
    pcOFCM <- verificaTabela("OFCM/pcOFCM", nGrupos, nDados, nomeDoConjunto) 
    
    #pcOFCM <- paste("OFCM/pcOFCM", as.character(nGrupos), sep = "")
    #pcOFCM <- paste(pcOFCM, "grupos", sep = "")  
    #pcOFCM <- paste(pcOFCM, nomeDoConjunto, sep = "")
    #if (file.exists(pcOFCM)) 
      #Delete file if it exist
      #file.remove(pcOFCM)
  }
  if(medirPE){
    
    peOFCM <- verificaTabela("OFCM/peOFCM", nGrupos, nDados, nomeDoConjunto)
    
    #peOFCM <- paste("OFCM/peOFCM", as.character(nGrupos), sep = "")
    #peOFCM <- paste(peOFCM, "grupos", sep = "")
    #peOFCM <- paste(peOFCM, nomeDoConjunto, sep = "")
    #if (file.exists(peOFCM)) 
      #Delete file if it exist
      #file.remove(peOFCM)
  }
  
  streamCSV <- DSD_ReadCSV(nomeDoConjunto, sep=",", header=TRUE, loop=TRUE, class = NULL)
  dadosLidos <- 0
  tamConjuntoCentros <- 0
  
  while(dadosLidos < totalDados){
    
    horarioInicio <- Sys.time()
    
    #todos os dados do fluxo tem peso 1
    chunk <- get_points(streamCSV, n= nDados)
    pesos <- rep(1, nDados)
    
    dadosLidos <- dadosLidos + nDados
    
    #agrupa os dados
    centrosEPesos <- WFCM(chunk, pesos, nGrupos, cteFuzzy, distanciaMaximaPermitida)
    
    #separa os centros dos pesos resultantes do agrupamento
    if(tamConjuntoCentros == 0){
      centros <- centrosEPesos$centros
      pesosCentros <- centrosEPesos$pesos
    }
    else{
      centros <-rbind(centros, centrosEPesos$centros)
      pesosCentros <- c(pesosCentros, centrosEPesos$pesos)
    }
    
    tamConjuntoCentros <- tamConjuntoCentros + nGrupos
    
    #se há dados suficientes para agrupar mais uma vez, agrupa de novo
    if(tamConjuntoCentros >= nDados){
      tamConjuntoCentros <- 0
      #plot(centros)
      temp <- WFCM(centros, pesosCentros, nGrupos, cteFuzzy, distanciaMaximaPermitida)
      horarioFim <- Sys.time()
      
      plotName <- paste("OFCM/OFCM", as.character(dadosLidos), sep = "")
      plotName <- paste(plotName, "dados", sep = "")
      plotName <- paste(plotName, as.character(nGrupos), sep = "")
      plotName <- paste(plotName, "clusters_", sep = "")
      plotName <- paste(plotName, nomeDoConjunto, sep = "")
      plotName <- paste(str_sub(plotName, 1, str_length(plotName)-4), ".png", sep = "")
      plotar(plotName,temp$centros, chunk)
      
      #plotar(temp$centros, centros)
      matrizPertinencia <- temp$matrizPertinencia
      
      if(medirXiebeni){
        #xiebeni <- XB(centros, t(matrizPertinencia), temp$centros, cteFuzzy)
        xiebeni <- XB(chunk, t(matrizPertinencia), temp$centros, cteFuzzy)
        print("XieBeni:")
        print(xiebeni)
        write.table(paste(as.character(dadosLidos), as.character(xiebeni), sep = ","), file = xieBeniOFCM, sep = ",", append = TRUE, col.names = F, row.names = F)
      }
      if(medirTempo){
        tempoExec <- horarioFim - horarioInicio
        print(tempoExec)
        write.table(paste(as.character(dadosLidos), as.character(tempoExec), sep = ","), file = tempoOFCM, sep = ",", append = TRUE, col.names = F, row.names = F)
      }
      if(FSCoef){
        FS <- FS(centros, matrizPertinencia, FSCoef)
        print("FS:")
        print(FS)
        write.table(paste(as.character(dadosLidos), as.character(FS), sep = ","), file = fsOFCM, sep = ",", append = TRUE, col.names = F, row.names = F)
      }
      if(medirWLI){
        WLI <- WLI(centros, matrizPertinencia, temp$centros)
        print("WLI:")
        print(WLI)
        write.table(paste(as.character(dadosLidos), as.character(WLI), sep = ","), file = wliOFCM, sep = ",", append = TRUE, col.names = F, row.names = F)
      }
      if(medirPC){
        pc <- PC(matrizPertinencia)
        print("PC:")
        print(pc)
        write.table(paste(as.character(dadosLidos), as.character(pc), sep = ","), file = pcOFCM, sep = ",", append = TRUE, col.names = F, row.names = F)
      }
      if(medirPE){
        pe <- PE(matrizPertinencia)
        print("PE:")
        print(pe)
        write.table(paste(as.character(dadosLidos), as.character(pe), sep = ","), file = peOFCM, sep = ",", append = TRUE, col.names = F, row.names = F)
      }
      # Como o algoritmo possui 2 fases de agrupamento, sendo a segunda dependente da primeira,
      # a segunda fase sempre tera numero ideal de clusters a quantidade de clusters definida pelo usuario
      if(qtdeMaxCluster){
        if(medirCWB){
          scat <- rep(0, qtdeMaxCluster)
          dis <- rep(0, qtdeMaxCluster)
          for(c in 2:qtdeMaxCluster){
            cwbOFCM <- paste("OFCM/cwbOFCM", as.character(dadosLidos), sep = "")
            cwbOFCM <- paste(cwbOFCM, "dados", sep = "")
            cwbOFCM <- paste(cwbOFCM, as.character(nDados), sep = "")
            cwbOFCM <- paste(cwbOFCM, "chunk_", sep = "")
            cwbOFCM <- paste(cwbOFCM, nomeDoConjunto, sep = "")
            if (file.exists(cwbOFCM)) 
              #Delete file if it exist
              file.remove(cwbOFCM)
            temp <- WFCM(centros, pesosCentros, c, cteFuzzy, distanciaMaximaPermitida)
            scat[c] <- scattering(centros, temp$matrizPertinencia, temp$centros)
            dis[c] <- functionalDistance(temp$centros)
          }
          CWB <- rep(0, qtdeMaxCluster)
          for(c in 2:qtdeMaxCluster){
            CWB[c] <- CWB(scat[c],dis[c],dis[qtdeMaxCluster])
            write.table(paste(as.character(c), as.character(CWB[c]), sep = ","), file = cwbOFCM, sep = ",", append = TRUE, col.names = F, row.names = F)
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
            dsoOFCM <- paste("OFCM/dsoOFCM", as.character(dadosLidos), sep = "")
            dsoOFCM <- paste(dsoOFCM, "dados", sep = "")
            dsoOFCM <- paste(dsoOFCM, as.character(nDados), sep = "")
            dsoOFCM <- paste(dsoOFCM, "chunk_", sep = "")
            dsoOFCM <- paste(dsoOFCM, nomeDoConjunto, sep = "")
            if (file.exists(dsoOFCM)) 
              #Delete file if it exist
              file.remove(dsoOFCM)
            temp <- WFCM(centros, pesosCentros, c, cteFuzzy, distanciaMaximaPermitida)
            disp[c] <- dispersion(centros, temp$centros)
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
            write.table(paste(as.character(c), as.character(DSO[c]), sep = ","), file = dsoOFCM, sep = ",", append = TRUE, col.names = F, row.names = F)
          }
        }
      }
    }
    
  }
  
  close_stream(streamCSV)
}