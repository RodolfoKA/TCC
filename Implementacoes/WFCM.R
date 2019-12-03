library("stream")
library("fclust")
library("rdist")
library("stringr")
source("metricasValidacao.R")


#A matriz pertinencia tem numero de linhas igual ao de grupos e colunas igual ao de pontos
gerarMatrizPertinencia <- function(pesos, nGrupos, nDados){ #Gera uma matriz aleatoria inicial com a soma de cada linha dando 1
  
  matriz <- matrix(data = 0, nrow = nGrupos, ncol = nDados)
  for(j in 1:nDados){
    vetorPertinenciaAleatoria <- runif(nGrupos)
    vetorPertinenciaAleatoria <- (vetorPertinenciaAleatoria/sum(vetorPertinenciaAleatoria))*pesos[j]
    for(i in 1:nGrupos){
      matriz[i,j] <- vetorPertinenciaAleatoria[i]
    }
  }
  return(matriz)
}

#Gera os pesos dos centros
gerarPesos <- function(matrizPertinencia, pesosPontos, centrosAnteriores){ 
  
  #gera o vetor de pesos da matriz
  pesos <- rep(0, nrow(matrizPertinencia))
  #desconsidera os pesos dos centroides anteriores
  qtdePontosConsiderados <- ncol(matrizPertinencia)
  if(!is.null(centrosAnteriores)){
    qtdePontosConsiderados <- qtdePontosConsiderados - nrow(centrosAnteriores)
  }
  
  for(i in 1:nrow(matrizPertinencia)){
    for(j in 1:qtdePontosConsiderados){
      pesos[i] <- pesos[i] + matrizPertinencia[i,j]*pesosPontos[j]
    }
  }
  
  return(pesos)
}

#para o centro, eu assumi que as somas sao separadas em x e y, ou seja,
  #o centro e os pontos variam em x e y tambem em cada soma
gerarCentros <- function(conjuntoDados, matrizPertinencia, pesos, cteFuzzy){
  centros <- matrix(data = 0, nrow = nrow(matrizPertinencia), ncol = ncol(conjuntoDados))
  
  for(i in 1:nrow(matrizPertinencia)){ #cada centro
    for(k in 1:ncol(conjuntoDados)){ # para cada atributo dos dados
      numerador <- 0
      denominador <- 0
      for(j in 1:nrow(conjuntoDados)){ # cada dado
          numerador <- numerador + pesos[j]*conjuntoDados[j,k]*(matrizPertinencia[i,j]^cteFuzzy)
          denominador <- denominador + pesos[j]*matrizPertinencia[i,j]^cteFuzzy
      }
      centros[i,k] <- numerador/denominador
    }
    
  }
  return(centros)
}

atualizarMatrizPertinencia <- function(conjuntoDados, centros, cteFuzzy){
  matriz <- matrix(data = 0, nrow = nrow(centros), ncol = nrow(conjuntoDados))
  potencia <- 2/(cteFuzzy - 1)
  
  distMatrix <- as.matrix(cdist(conjuntoDados, centros))
  
  for(i in 1:nrow(centros)){     #Vou calcular para cada Uij da matriz de pertinencia
     for(j in 1:nrow(conjuntoDados)){
      somatoria <- 0
      #se o ponto for igual ao centro, ele vai ter pertinencia 1 ao grupo do centro
      if(distMatrix[j,i] == 0){
        somatoria <- 1
      }
      else{
        for(k in 1:nrow(centros)){    #k ? para as tres partes da somatoria, usando os tres centros Vl
          
          #se o denominador for 0, quer dizer que o ponto eh um centro que nao e o comparado,
          #entao tem peso 0 para a somatoria
          if(distMatrix[j,k] == 0){
            somatoria <- somatoria + 0
          }
          else{
            somatoria <- somatoria + (distMatrix[j,i]/distMatrix[j,k])^potencia
          }
        }
      }
      matriz[i,j] <- somatoria^(-1)
    }
  }
  return(matriz)
}

distanciaMaxima <- function(centros, centrosTemp){ # acha as distancias entre os centros e retorne a maior
  distancias <- as.matrix(cdist(centros, centrosTemp))                                    #distancia
  
  max <- distancias[1,1]
  for(i in 1:nrow(centros)){
    if(max < distancias[i,i]){
      max <- distancias[i,i]
    }
  }
  return(max)
}

plotar <- function(nome,centros,pontos){
  png(nome)
  plot(   x=pontos[,1],
          y=pontos[,2],
          xlab="X",
          ylab="Y",
          main="Dados",
          pch=19, # tipo de caracter do plot
          col="blue"
  )
  points( x=centros[,1],
          y=centros[,2],
          pch=2,
          col="red"
  )
  dev.off()
}

WFCM <- function(pontos, pesos, nGrupos, cteFuzzy, distanciaMaximaPermitida, centrosAnteriores = NULL){
  distanciaMaximaEntreCentros <- Inf
  if(is.null(centrosAnteriores)){
    matrizPertinencia <- gerarMatrizPertinencia(pesos, nGrupos, nrow(pontos))
    #pesos <- atualizarPesos(matrizPertinencia, nDados, nGrupos)
    centros <- gerarCentros(pontos, matrizPertinencia, pesos, cteFuzzy)
  }
  else{
    centros <- as.matrix(centrosAnteriores)
  }
  #plot(centros)
  
  while(distanciaMaximaEntreCentros > distanciaMaximaPermitida){
    matrizPertinencia <- atualizarMatrizPertinencia(pontos, centros, cteFuzzy)
    centrosTemp <- centros
    #pesos <- atualizarPesos(matrizPertinencia, nDados, nGrupos)
    centros <- gerarCentros(pontos, matrizPertinencia, pesos, cteFuzzy)
    distanciaMaximaEntreCentros <- distanciaMaxima(centros, centrosTemp)
    
    #plot(centros)
    
  }
  
  pesosCentros <- gerarPesos(matrizPertinencia, pesos, centrosAnteriores)
  centrosEPesos <-list("centros" = centros, "pesos" = pesosCentros, "matrizPertinencia" = matrizPertinencia)
  
  return(centrosEPesos)
}

#distanciaMaximaPermitida <- 0.0001
#cteFuzzy <- 1.5
#nGrupos <- 3
#nDados <- 50
#streamCSV <- DSD_ReadCSV("conjunto1.csv", sep=",", header=TRUE, loop=TRUE, class = 3)

#pontos <- get_points(streamCSV, n= nDados)
#pesos <- rep(1, nDados)

#centrosEPesos <- WFCM(pontos, pesos, nGrupos, nDados, cteFuzzy, distanciaMaximaPermitida)
#close_stream(streamCSV)
