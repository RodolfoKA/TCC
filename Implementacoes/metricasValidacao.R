library(geometry)


verificaTabela <- function(local, nGrupos, nDados, nomeDoConjunto){
  nomeTabela <- paste(local, as.character(nGrupos), sep = "")
  nomeTabela <- paste(nomeTabela, "grupos", sep = "")
  nomeTabela <- paste(nomeTabela, as.character(nDados), sep = "")
  nomeTabela <- paste(nomeTabela, "chunk_", sep = "")
  nomeTabela <- paste(nomeTabela, nomeDoConjunto, sep = "")
  nomeTabela <- paste(nomeTabela, ".csv", sep = "")
  if (file.exists(nomeTabela)){ 
    #Delete file if it exist
    file.remove(nomeTabela)
  }
  
  return(nomeTabela)
}


##############################################
##                  FS                      ##
##############################################


# Comando para gerar uma matriz de pertinencia binaria, que sera usada na silhueta
gerarMatrizPertinenciaBinaria <- function(matrizPertinencia){
  coordMaior <- rep(0,2)
  matrizPertinenciaBinaria <- matrizPertinencia
  
  for(j in 1:ncol(matrizPertinencia)){
    maiorPertinencia <- -Inf
    primeiraLinha <- TRUE
    
    for(i in 1:nrow(matrizPertinencia)){
      if(matrizPertinencia[i,j] > maiorPertinencia){
        maiorPertinencia <- matrizPertinencia[i,j]
        matrizPertinenciaBinaria[i,j] <- 1
       
        if(!primeiraLinha){
          matrizPertinenciaBinaria[coordMaior[1],coordMaior[2]] <- 0
        }
        
        coordMaior[1] <- i
        coordMaior[2] <- j
        primeiraLinha <- FALSE
      }
      
      else{
        matrizPertinenciaBinaria[i,j] <- 0
      }
    }
  }
  return(matrizPertinenciaBinaria)
}

# Calculo da silhueta de cada dado, sera usado no calculo da fuzzy silhouette, dando divergencia(pmax talvez) com a funcao SIL ja implementada
silhueta <- function(chunk, matrizPertinencia){
  
  matrizPertinenciaBinaria <- gerarMatrizPertinenciaBinaria(matrizPertinencia)
  
  distanciaEntreDados <- as.matrix(cdist(chunk, chunk))
  
  s <- rep(0, ncol(matrizPertinenciaBinaria))
  
  # Calculo da medida para cada ponto do chunk
  for(p in 1:nrow(chunk)){
    pertenceCluster <- FALSE
    c <- 1
    # Descobre qual cluster o dado pertence
    while(!pertenceCluster){
      if(matrizPertinenciaBinaria[c,p] == 1){
        pertenceCluster <- TRUE
      }
      else{
        c <-  c + 1 
      }
    }
    
    # Distancia media entre um dado p e todos os outros pontos, separados por cluster
    distMediaEntreCluster <- rep(0, nrow(matrizPertinenciaBinaria))
    qtdeDadosCluster <- rep(0, nrow(matrizPertinenciaBinaria))
    for(q in 1:ncol(matrizPertinenciaBinaria)){
      # Se matrizPertinenciaBinaria[c,q] for 1, pertece ao cluster
      for(cluster in 1:nrow(matrizPertinenciaBinaria)){
          distMediaEntreCluster[cluster] <- distMediaEntreCluster[cluster] + distanciaEntreDados[p, q]*matrizPertinenciaBinaria[cluster,q]
          qtdeDadosCluster[cluster] <- qtdeDadosCluster[cluster] + matrizPertinenciaBinaria[cluster, q]
      }
    }

    distMediaEntreCluster <- distMediaEntreCluster/qtdeDadosCluster
    distMediaMesmoCluster <- distMediaEntreCluster[c]
    # Define-se como infinito no vetor para que a selecao da menor distancia nao escolha a distancia entre os dados de mesmo cluster
    distMediaEntreCluster[c] <- Inf
    
    s[p] <- (min(distMediaEntreCluster) - distMediaMesmoCluster)/pmax(min(distMediaEntreCluster), distMediaMesmoCluster)
    
  }
  
  return(s)
    
}

# Calculo da medida FS do agrupamento fuzzy, convergindo com SIL.F usando SIL
FS <- function(chunk,matrizPertinencia,wCoef){
  
  numerador <- 0
  denominador <- 0
  
  
  #s <- silhueta(chunk, matrizPertinencia)
  s <- SIL(chunk, t(matrizPertinencia))
  
  for(j in 1:ncol(matrizPertinencia)){
    primeiraMaiorPertinencia <- -Inf
    segundaMaiorPertinencia <- -Inf
    for(i in 1:nrow(matrizPertinencia)){
      # Pega o maior valor de pertinencia entre os clusters para o dado
      if(matrizPertinencia[i,j] > primeiraMaiorPertinencia){
        segundaMaiorPertinencia <- primeiraMaiorPertinencia
        primeiraMaiorPertinencia <- matrizPertinencia[i,j]
      }
      else{
         # Pega o segundo maior valor de pertinencia
         if(matrizPertinencia[i,j] > segundaMaiorPertinencia){
           segundaMaiorPertinencia <- matrizPertinencia[i,j]
         }
       }
    }
    
    # Calculo das somatorias da medida
    numerador <- numerador + ((primeiraMaiorPertinencia - segundaMaiorPertinencia)**wCoef)*s$sil.obj[j]
    denominador <- denominador + (primeiraMaiorPertinencia - segundaMaiorPertinencia)**wCoef
  }
  
  fuzzyS <- numerador/denominador
  
  return(fuzzyS)
  
}

##############################################
##                  CWB                     ##
##############################################


chunkVariation <- function(chunk){
  variation <- rep(0, ncol(chunk))
  temp <- var(chunk)
  for(i in 1:ncol(chunk)){
    variation[i] <- as.numeric(temp[i,i])
  }
  
  return(variation)
}

fuzzyVariation <- function(chunk, matrizPertinencia, centroide){
  fuzzyVar <- matrix(data = 0, nrow = nrow(centroide), ncol = ncol(centroide))
 
    
    for(i in 1:nrow(centroide)){
      for(p in 1:ncol(centroide)){
        somatoria <- 0
        for(k in 1:nrow(chunk)){
          somatoria <- somatoria + matrizPertinencia[i,k]*(chunk[k,p] - centroide[i,p])**2
        }
        fuzzyVar[i,p] <- somatoria/nrow(chunk)
      }
    }
  return(fuzzyVar)
}

scattering <- function(chunk, matrizPertinencia, centroide){
  varChunk <- chunkVariation(chunk)
  fuzzyVarCluster <- fuzzyVariation(chunk, matrizPertinencia, centroide)
  
  somatoria <- 0
  for(i in 1:nrow(fuzzyVarCluster)){
    somatoria <- somatoria + sqrt(dot(fuzzyVarCluster[i,],fuzzyVarCluster[i,]))
  }
  
  Scat <- (somatoria/nrow(fuzzyVarCluster))/sqrt(dot(varChunk, varChunk))
  
  return(Scat)
  
}

functionalDistance <- function(centroide){
  
  distanciaEntreClusters <- as.matrix(dist(centroide, centroide))
  
  somatoria1 <- 0
  for(k in 1:nrow(centroide)){
    somatoria2 <- 0
    for(z in 1:nrow(centroide)){
      somatoria2 <- somatoria2 + distanciaEntreClusters[k,z]
    }
    somatoria1 <- somatoria1 + 1/somatoria2
  }
  
  for(i in 1:nrow(distanciaEntreClusters)){
    distanciaEntreClusters[i,i] <- -Inf
  }
  disMax <- max(distanciaEntreClusters)
  for(i in 1:nrow(distanciaEntreClusters)){
    distanciaEntreClusters[i,i] <- Inf
  }
  disMin <- min(distanciaEntreClusters)
  
  funcDis <- (disMax/disMin)*somatoria1
  
  return(funcDis)
}

CWB <- function(scat, funcDis, funcDisCMax){
  cwb <- funcDisCMax*scat + funcDis
  
  return(cwb)
}

##############################################
##                 DSO                      ##
##############################################


dataPointStdDev <- function(chunk){
  chunkSqrd <- chunk**2
  sumChunkSqrd <- rep(0, ncol(chunk))
  mediaAtributosChunk <- rep(0, ncol(chunk))
  dataStdDev <- rep(0, ncol(chunk))
  for(n in 1:ncol(chunk)){
    sumChunkSqrd[n] <- sum(chunkSqrd[,n])
    mediaAtributosChunk[n] <- mean(chunk[,n])
    dataStdDev[n] <- sqrt((sumChunkSqrd[n] - ((mediaAtributosChunk[n]*nrow(chunk))**2)/nrow(chunk))/nrow(chunk))
  }
  
  stdDev <- list("dataStdDev" = dataStdDev, "media" = mediaAtributosChunk)
  
  return(stdDev)
}

clusterStdDev <- function(chunk, centroide){
  stdDev <- matrix(data = 0, nrow = nrow(centroide), ncol = ncol(centroide))
  for(c in 1:nrow(centroide)){
    for(n in 1:ncol(centroide)){
      somatoria <- 0
      for(i in 1:nrow(chunk)){
        somatoria <- somatoria + (chunk[i,n] - centroide[c,n])**2
      }
      stdDev[c,n] <- sqrt(somatoria/nrow(chunk))
    }
  }
  return(stdDev)
}

similarity <- function(matrizPertinencia){
  similarityMatrix <- matrix(data = Inf, nrow = nrow(matrizPertinencia), ncol = nrow(matrizPertinencia))
  minValue <- rep(0, ncol(matrizPertinencia))
  for(l in 1:nrow(matrizPertinencia)){
    
    for(r in 1:nrow(matrizPertinencia)){
      # Propriedade s[l,r] = s[r,l], poupa iteracoes
      if(similarityMatrix[r,l] < Inf){
        similarityMatrix[l,r] <- similarityMatrix[r,l]
      }
      # Propriedade s[l,r] = 1 se l = r, poupa iteracoes, coloco como 0 para nao ser pego quando tentar pegar o valor minimo posteriormente
      else if(l == r){
        similarityMatrix[l,r] <- 0
      }
      else{
        for(j in 1:ncol(matrizPertinencia)){
          minValue[j] <- min(matrizPertinencia[l,j],matrizPertinencia[r,j])
          
        }
        similarityMatrix[l,r] <- max(minValue)
      }
    }
  }
  return(similarityMatrix)
}

dataPointOverlap <- function(matrizPertinencia){
  dataPointOverlapBetweenCluster <- array(0, dim = c(ncol(matrizPertinencia), nrow(matrizPertinencia), nrow(matrizPertinencia)))

  for(l in 1:nrow(matrizPertinencia)){
    for(r in 1:nrow(matrizPertinencia)){
      if(l == r){
        dataPointOverlapBetweenCluster[,l,r] <- -Inf  
      }
      else{
        for(j in 1:ncol(matrizPertinencia)){
          # Nao ha overlap entre o mesmo cluster
          
            domMax <- max(matrizPertinencia[l,j],matrizPertinencia[r,j])
            domMin <- min(matrizPertinencia[l,j],matrizPertinencia[r,j])
            if(domMin > 0){
              if(domMax <= 0.5){
                dataPointOverlapBetweenCluster[j,l,r] <- 1.0
              }
              else if(domMax < 1){
                # Segue a funcao ]0.5,1[ -> ]0.9,0.1[
                dataPointOverlapBetweenCluster[j,l,r] <- -1.6*domMax + 1.7
              }
              else{
                dataPointOverlapBetweenCluster[j,l,r] <- 0
              }
            }
          }
      }
    }
  }
  return(dataPointOverlapBetweenCluster)
}

clustersOverlap <- function(dataPointOverlap){
  dimensions <- dim(dataPointOverlap)
  clusterOverlap <- matrix(data = 0, nrow = dimensions[2], ncol = dimensions[3])
  for(l in 1:nrow(clusterOverlap)){
    for(r in 1:ncol(clusterOverlap)){
      clusterOverlap[l,r] <- sum(dataPointOverlap[,l,r])
    }
  }
  return(clusterOverlap)
}

dispersion <- function(chunk, centroide){
  dataStdDev <- dataPointStdDev(chunk)
  coffVar  <- rep(0, ncol(chunk))
  coffVar <- dataStdDev$dataStdDev/dataStdDev$media
  clusterDev <- clusterStdDev(chunk, centroide)
  coffCluster <- clusterDev/centroide
  disp <- max(coffCluster, na.rm = T)/max(coffVar, na.rm = T)
  
  return(disp)
}

separation <- function(matrizPertinencia){
  similarityMatrix <- similarity(matrizPertinencia)
  sep <- min(1- similarityMatrix)
  
  return(sep)
}

overlap <- function(matrizPertinencia){
  dataPointOverlap <- dataPointOverlap(matrizPertinencia)
  clusterOverlap <- clustersOverlap(dataPointOverlap)
  overlap <- max(clusterOverlap)
}

DSO <- function(disp, sep, overlap, dispMax, sepMax, overlapMax){
  dispN <- disp/dispMax
  sepN <- sep/sepMax
  overlapN <- overlap/overlapMax
  
  dso <- (dispN + overlapN)/sepN
  return(dso)

}

##############################################
##                 WLI                      ##
##############################################

wln <- function(chunk, centroide, matrizPertinencia){
  distanciaDadosCentroides <- as.matrix(cdist(centroide, chunk))
  distanciaDadosCentroides < - distanciaDadosCentroides**2
  matrizDistanciaEPertinencia <- matrizPertinencia**2
  matrizDistanciaEPertinencia <- matrizDistanciaEPertinencia*distanciaDadosCentroides
  wln <- 0
  for(k in 1:nrow(centroide)){
    wln <- wln + sum(matrizDistanciaEPertinencia[k,])/sum(matrizPertinencia[k,])
  }
  return(wln)
}

wld <- function(centroide){
  distanciaCentroides <- as.matrix(cdist(centroide, centroide))
  distanciaCentroides <- distanciaCentroides**2
  for(n in 1:nrow(distanciaCentroides)){
    distanciaCentroides[n,n] <- NA
  }
  medianDist <- median(distanciaCentroides, na.rm = TRUE)
  minDist <- min(distanciaCentroides, na.rm = TRUE)
  
  wld <- (minDist + medianDist)/2
  
  return(wld)
}

WLI <- function(chunk, matrizPertinencia, centroide){
  wln <- wln(chunk, centroide, matrizPertinencia)
  wld <- wld(centroide)
  
  wli <- wln/(2*wld)
  return(wli)
}

##############################################
##                 PC                       ##
##############################################

# Valor: 1/c <= PC <= 1; quanto maior, melhor
# matrizPertinencia -> Uij, i = cluster(c), j = dados (n)
PC <- function(matrizPertinencia){
  nCluster <- nrow(matrizPertinencia)
  nDados <- ncol(matrizPertinencia)
  
  quadradoMatriz <- matrizPertinencia**2
  pc <- 0
  for(j in 1:nDados){
    for(i in 1:nCluster){
      pc <- pc + quadradoMatriz[i,j]
    }
  }
  pc <- pc/nDados
  
  return(pc)
}

##############################################
##                 PE                       ##
##############################################

# Valor: 0 <= PC <= log(Uij); quanto menor, melhor
# matrizPertinencia -> Uij, i = cluster(c), j = dados (n)
PE <- function(matrizPertinencia){
  nCluster <- nrow(matrizPertinencia)
  nDados <- ncol(matrizPertinencia)
  
  quadradoMatriz <- matrizPertinencia**2
  pc <- 0
  for(j in 1:nDados){
    for(i in 1:nCluster){
      pc <- pc + quadradoMatriz[i,j]*log2(matrizPertinencia[i,j])
    }
  }
  pc <- -pc/nDados
  
  return(pc)
}