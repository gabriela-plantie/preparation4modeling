#' @import dplyr
#'
#' @importFrom magrittr "%>%"

genera_deciles<-function(tbla, variable_name, target_name, q){
  #print('genera deciles')
  tbla<-data.frame(tbla)
  tbla1<-tbla[!is.na(tbla[, variable_name]),]
  grupos<-quantile(tbla1[, variable_name], probs=seq(0,1,as.numeric(1/q)), na.rm=T)
  grupos<-unique(grupos)

  deciles<- cut(tbla[,variable_name], c(-Inf, grupos[2: (length(grupos)-1)], Inf ))

  a_devolver=deciles #data.frame(corte_inf=corte_inf, corte_sup=corte_sup, deciles= deciles)
  return(a_devolver)
}


#el test me tiene q decir q es distinto, pero aun siendo distinto, no alcanza
#tiene q ir siempre en el mismo sentido (creciente o no)
#sino me queda algo oscilante, q tiene mas chances de tener overfitting

encuentra_sentido<-function(tbla_0, variable_name, target_name){
  #print('encuentra sentido')
  tbla_0$deciles<-genera_deciles(tbla_0, variable_name, target_name, q=5)
  tbla_agr<-devuelve_tabla_agrupada(tbla_0, 'deciles', target_name)
  if(tbla_agr[nrow(tbla_agr), 'rt']>tbla_agr[1, 'rt']) {1} else{-1}
}

agrupar_consecutivos <- function(tbla_agrupada, var_en_rangos, sentido) {
  if( nrow(tbla_agrupada)<=2){
    return(tbla_agrupada)
  }

  #print('agrupa consecutivos')
  #print(nrow(tbla_agrupada))
  if( sentido==1 ) {
    numero_anterior = max(tbla_agrupada$numero[tbla_agrupada$numero<tbla_agrupada$numero[1]])
    anterior = tbla_agrupada[tbla_agrupada$numero == numero_anterior,]

    tbla_agrupada[1, var_en_rangos]=paste0(anterior[, var_en_rangos], " + ", tbla_agrupada[1, var_en_rangos])
  }
  if( sentido== -1 ) {
    numero_anterior = min(tbla_agrupada$numero[tbla_agrupada$numero>tbla_agrupada$numero[1]])
    anterior = tbla_agrupada[tbla_agrupada$numero == numero_anterior,]

    tbla_agrupada[1, var_en_rangos]=paste0(tbla_agrupada[1, var_en_rangos], " + ", anterior[, var_en_rangos])
  }

  tbla_agrupada$pos[1]=tbla_agrupada$pos[1]+anterior$pos
  tbla_agrupada$neg[1]=tbla_agrupada$neg[1]+anterior$neg
  tbla_agrupada$tot[1]=tbla_agrupada$tot[1]+anterior$tot
  tbla_agrupada$rt[1]=tbla_agrupada$pos[1] / tbla_agrupada$tot[1]
  tbla_agrupada$prob[1]=NA
  tbla_agrupada$distancia[1]=NA
  tbla_agrupada = tbla_agrupada[tbla_agrupada$numero != numero_anterior,]

  return(tbla_agrupada)
}


obtiene_extremos<-function(tbla_agrupada, var_en_rangos){
  tbla_agrupada[,var_en_rangos]<-as.character(tbla_agrupada[,var_en_rangos])
  lim_inferior=gsub(',.*', '', tbla_agrupada[,var_en_rangos])
  lim_superior=gsub('.*,', '', tbla_agrupada[,var_en_rangos])
  tbla_agrupada[,var_en_rangos]<-paste0(lim_inferior, ',',lim_superior )
  #tbla_agrupada$log_odds=log(tbla_agrupada$pos/tbla_agrupada$neg)
  tbla_agrupada$orden<-1:nrow(tbla_agrupada)
  return(tbla_agrupada)
}


agrupar_por_br <- function(tbla_agrupada,sentido,var_en_rangos) {
  #print('agrupa por br')
  #print(tbla_agrupada)
  seguir_agrupando = T;
    while(seguir_agrupando){#evalua la distancia
    cantidad_de_filas = nrow(tbla_agrupada)
    idx_sup=2:nrow(tbla_agrupada)
    idx_inf=1:(nrow(tbla_agrupada)-1)
    tbla_agrupada$distancia<-NA

    tbla_agrupada$distancia[idx_sup]<-tbla_agrupada$pos[idx_sup]/tbla_agrupada$tot[idx_sup]-
      tbla_agrupada$pos[idx_inf]/tbla_agrupada$tot[idx_inf]
    #print(tbla_agrupada)
    tbla_agrupada = tbla_agrupada[order(tbla_agrupada$distancia),]
    if(tbla_agrupada$distancia[1] < 0 ) {###solo si hay negativos agrupa, quiere decir que cambia de sentido
      tbla_agrupada = agrupar_consecutivos(tbla_agrupada, var_en_rangos, sentido)
    }
    tbla_agrupada = tbla_agrupada[order(tbla_agrupada$numero* sentido),]
    seguir_agrupando = nrow(tbla_agrupada) < cantidad_de_filas
    }
  #print('fin_agrupa por br sin errores')
  return(tbla_agrupada)
}



agrupar_por_test <- function(tbla_agrupada,limite,sentido,var_en_rangos) {
  #print('agrupa test hypergeom')
  seguir_agrupando = T;
  tbla_agrupada$prob<-NA

  while(seguir_agrupando){#evalua por test de fisher
    cantidad_de_filas = nrow(tbla_agrupada)
    tbla_agrupada<-tbla_agrupada[order(tbla_agrupada$numero *sentido),]
    for (i in 2:nrow(tbla_agrupada)){ #i=2
      positivos_muestra= tbla_agrupada$pos[i] #x
      positivos_tot=sum(tbla_agrupada$pos[(i-1):i])#m
      negativos_tot=sum(tbla_agrupada$neg[(i-1):i])#n
      muestra=tbla_agrupada$tot[i]#k
      py= phyper(q=positivos_muestra, m=positivos_tot, n=negativos_tot, k=muestra, lower.tail = F, log.p=F) +
        dhyper(x=positivos_muestra, m=positivos_tot, n=negativos_tot, k=muestra)
      tbla_agrupada$prob[i]<-round(py,4)
      }
    tbla_agrupada = tbla_agrupada[order(-tbla_agrupada$prob),]
    if(tbla_agrupada$prob[1] > (limite) ) {
      tbla_agrupada = agrupar_consecutivos(tbla_agrupada, var_en_rangos, sentido)
      }
    seguir_agrupando = nrow(tbla_agrupada) < cantidad_de_filas
  }
  return(tbla_agrupada)
}


##recbir la tabla ordenada por decil o si es nominal por BR
test_hyper2<-function(tbla_agrupada, var_en_rangos, sentido,  limite=0.025){
  #ordena de acuerdo al sentido, para que sea siempre creciente en malos
  tbla_agrupada$numero<-1:nrow(tbla_agrupada)
  tbla_agrupada<-tbla_agrupada[order(sentido*tbla_agrupada$numero),]##queda la tasa mas chica arriba
  rownames(tbla_agrupada)<-1:nrow(tbla_agrupada)
  tbla_agrupada[, var_en_rangos] = as.character(tbla_agrupada[, var_en_rangos])
  #hace el test, despues se podria pasar como parametro
  tbla_agrupada=agrupar_por_br(tbla_agrupada,sentido, var_en_rangos)

  tbla_agrupada=agrupar_por_test(tbla_agrupada, limite, sentido, var_en_rangos)

  tbla_agrupada<-tbla_agrupada[order(tbla_agrupada$numero*sentido),]
  tbla_agrupada$distancia<-NULL
  tbla_agrupada$prob<-NULL
  tbla_agrupada$numero<-NULL

  tbla_final<-obtiene_extremos(tbla_agrupada , var_en_rangos )
  return(tbla_final)
}

