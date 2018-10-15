
arma_matriz_test<-function(tbla_agrupada, campo){
  mymat = matrix(NA,nrow(tbla_agrupada),nrow(tbla_agrupada))
  rownames(mymat)=tbla_agrupada[, campo]
  colnames(mymat)=tbla_agrupada[, campo]
  for (i in 1:(nrow(mymat)-1)){#i=1
    for (j in (i+1):nrow(mymat)){#j=2
      #va a buscar los valores a la tbla agrupada
      positivos_i=tbla_agrupada$pos[tbla_agrupada[,campo]== rownames(mymat)[i]]
      positivos_j=tbla_agrupada$pos[tbla_agrupada[,campo]== rownames(mymat)[j]]
      positivos_tot=positivos_i+positivos_j

      #muestra
      muestra_i=tbla_agrupada$tot[tbla_agrupada[,campo]== rownames(mymat)[i]]
      muestra_j=tbla_agrupada$tot[tbla_agrupada[,campo]== rownames(mymat)[j]]

      if( positivos_i / muestra_i>= positivos_j/muestra_j){
        positivos_muestra=positivos_i
        muestra=muestra_i;
      }else{
        positivos_muestra=positivos_j
        muestra=muestra_j;
      }

      negativos_i=muestra_i-positivos_i
      negativos_j=muestra_j-positivos_j
      negativos_tot=negativos_i+negativos_j
      #tiene que ordenar por el que tiene mas BR para dejar como muestra, y dejar la cola a derecha
      #hace el test y lo pega en la matrix
      py= phyper(q=positivos_muestra, m=positivos_tot, n=negativos_tot, k=muestra, lower.tail = F, log.p=F) +
        dhyper(x=positivos_muestra, m=positivos_tot, n=negativos_tot, k=muestra)
      mymat[i,j]=py
    }
  }
  return(mymat)
}



genera_mayor<-function(mymat, tbla_agrupada, campo){#campo='grupo'
  lista_max=which(mymat==max(mymat, na.rm=T))
  maximo=lista_max[1]
  mymat[maximo]
  columna=floor(maximo/nrow(mymat))+1
  fila=maximo%%nrow(mymat)
  fila_nombre=as.character(tbla_agrupada[fila,campo])
  columna_nombre=as.character(tbla_agrupada[columna,campo])
  return(c(fila, columna, fila_nombre, columna_nombre))
}
