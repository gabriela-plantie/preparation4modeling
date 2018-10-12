#' @import dplyr

devtools::use_package('dplyr')
`%>%`<- dplyr::`%>%`

genera_deciles<-function(tbla, variable_name, target_name, q){
  tbla<-data.frame(tbla)
  tbla1<-tbla[!is.na(tbla[, variable_name]),]
  grupos<-quantile(tbla1[, variable_name], probs=seq(0,1,as.numeric(1/q)), na.rm=T)
  a_devolver<- cut(tbla[,variable_name], c(-Inf, grupos[2: (length(grupos)-1)], Inf ))
  return(a_devolver)
}


devuelve_tabla_agrupada<-function(tbla, variable_nom, target_nom){
  #variable_nom='variable1';target_nom='target_n'

  a<-data.frame(tbla %>%dplyr::group_by(get(variable_nom))%>%
                  dplyr::summarise(tot=dplyr::n(),
                                   pos=sum(get(target_nom)),
                                   neg=tot-pos,
                                   rt= pos/tot))
  colnames(a)<-c(variable_nom, colnames(a)[2:ncol(a)] )
  return(a)
}

#tbla_agrupada<-devuelve_tabla_agrupada(tbla, 'deciles', 'target_n')

#el test me tiene q decir q es distinto, pero aun siendo distinto, no alcanza
#tiene q ir siempre en el mismo sentido (creciente o no)
#sino me queda algo oscilante, q tiene mas chances de tener overfitting

encuentra_sentido<-function(tbla_0,variable_name, target_name){
  #class(tbla); head(tbla); var_agrupada=variable_name
  tbla_0$deciles<-genera_deciles(tbla_0, variable_name, target_name, q=5)
  tbla_agr<-devuelve_tabla_agrupada(tbla_0, 'deciles', target_name)
  if(tbla_agr[nrow(tbla_agr), 'rt']>tbla_agr[1, 'rt']) {1} else{-1}
}

agrupar_consecutivos <- function(tbla_agrupada, var_en_rangos) {
  numero_anterior = max(tbla_agrupada$numero[tbla_agrupada$numero<tbla_agrupada$numero[1]])
  anterior = tbla_agrupada[tbla_agrupada$numero == numero_anterior,]
  tbla_agrupada[1, var_en_rangos]=paste0(anterior[, var_en_rangos], " + ", tbla_agrupada[1, var_en_rangos])
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
  #var_en_rangos<-'deciles'
  tbla_agrupada[,var_en_rangos]<-as.character(tbla_agrupada[,var_en_rangos])
  lim_inferior=gsub(',.*', '', tbla_agrupada[,var_en_rangos])
  lim_superior=gsub('.*,', '', tbla_agrupada[,var_en_rangos])
  tbla_agrupada[,var_en_rangos]<-paste0(lim_inferior, ',',lim_superior )
  tbla_agrupada$log_odds=log(tbla_agrupada$pos/tbla_agrupada$neg)
  tbla_agrupada$orden<-1:nrow(tbla_agrupada)
  return(tbla_agrupada)
}

##recbir la tabla ordenada por decil o si es nominal por BR
test_hyper2<-function(tbla_agrupada, var_en_rangos, sentido,  limite=0.025){
  #print(tbla_agrupada)
  #ordena de acuerdo al sentido, para que sea siempre creciente en malos
  tbla_agrupada$numero<-1:nrow(tbla_agrupada)
  tbla_agrupada<-tbla_agrupada[order(sentido*tbla_agrupada$numero),]
  rownames(tbla_agrupada)<-1:nrow(tbla_agrupada)
  tbla_agrupada[, var_en_rangos] = as.character(tbla_agrupada[, var_en_rangos])
  #hace el test, despues se podria pasar como parametro
  seguir_agrupando = T;
  while(seguir_agrupando){#evalua la distancia
    cantidad_de_filas = nrow(tbla_agrupada)
    idx_sup=2:nrow(tbla_agrupada)
    idx_inf=1:(nrow(tbla_agrupada)-1)
    tbla_agrupada$distancia<-NA
    tbla_agrupada$distancia[idx_sup]<-tbla_agrupada$pos[idx_sup]/tbla_agrupada$tot[idx_sup]-
      tbla_agrupada$pos[idx_inf]/tbla_agrupada$tot[idx_inf]

    tbla_agrupada = tbla_agrupada[order(tbla_agrupada$distancia),]
    if(tbla_agrupada$distancia[1] < 0 ) {
      tbla_agrupada = agrupar_consecutivos(tbla_agrupada, var_en_rangos)}
    tbla_agrupada = tbla_agrupada[order(tbla_agrupada$numero* sentido),]
    seguir_agrupando = nrow(tbla_agrupada) < cantidad_de_filas
  }

  seguir_agrupando = T;
  while(seguir_agrupando){#evalua por test de fisher
    cantidad_de_filas = nrow(tbla_agrupada)
    tbla_agrupada<-tbla_agrupada[order(tbla_agrupada$numero *sentido),]
    for (i in 2:nrow(tbla_agrupada)){ #i=2
      positivos_muestra= tbla_agrupada$pos[i] #x
      positivos_tot=sum(tbla_agrupada$pos[(i-1):i])#m
      negativos_tot=sum(tbla_agrupada$neg[(i-1):i])#n
      muestra=tbla_agrupada$tot[i]#k
      #py=dhyper(x=positivos_muestra, m=positivos_tot, n=negativos_tot, k=muestra)
      #lower tail (hasta)
      py= phyper(q=positivos_muestra, m=positivos_tot, n=negativos_tot, k=muestra, lower.tail = F, log.p=F) +
        dhyper(x=positivos_muestra, m=positivos_tot, n=negativos_tot, k=muestra)
      tbla_agrupada$prob[i]<-round(py,4)}
    tbla_agrupada = tbla_agrupada[order(-tbla_agrupada$prob),]
    if(tbla_agrupada$prob[1] > (limite) ) {
      tbla_agrupada = agrupar_consecutivos(tbla_agrupada, var_en_rangos)}
    seguir_agrupando = nrow(tbla_agrupada) < cantidad_de_filas
  }

  tbla_agrupada<-tbla_agrupada[order(tbla_agrupada$numero*sentido),]
  tbla_agrupada$distancia<-NULL
  tbla_agrupada$prob<-NULL
  tbla_agrupada$numero<-NULL

  tbla_final<-obtiene_extremos(tbla_agrupada , var_en_rangos )
  return( tbla_final)
}

