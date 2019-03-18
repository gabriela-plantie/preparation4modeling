agrupa_ctree_numerica<-function(tbla, target_name, variable_name,
         max_q_groups=20, min_q_casos=100, algoritmo='chaid', niterations=10000){

  tbla<-data.frame(tbla)
  devuelve<-data.frame()

  tbla$target<-tbla[,target_name] #hago una copia
  tbla$variable_valor<-tbla[,variable_name] #hago una copia


  tbla$variable_valor<-as.numeric(tbla$variable_valor)
  tbla$flag_na=0
  tbla$flag_na[is.na(tbla$variable_valor)]=1


if(algoritmo=='chaid'){
  treeLoc<-party::ctree(target~(variable_valor), tbla,
                        controls = ctree_control(minbucket= nrow(tbla)*(1/max_q_groups) ) )
  #minbucket es la cant minima de casos en un nodo terminal
}
if(algoritmo=='evetree'){
  treeLoc<- evtree::evtree(target~(variable_valor), tbla,
                           controls=evtree.control(minbucket= nrow(tbla)*(1/max_q_groups),
                                                   niterations=niterations))
  #minbucket es la cant minima de casos en un nodo terminal
}

  tbla$nodo_pred<-predict(treeLoc, tbla,type="node")
  b_sin_na<- data.frame(tbla%>%filter(flag_na==0)%>%group_by(nodo_pred)%>%summarise(
    corte_inf=round(min(variable_valor, na.rm=T),3),
    corte_sup=round(max(variable_valor, na.rm=T),3),
    pos_nodo=sum(target),
    cant_nodo=n(),
    rt_nodo=round(pos_nodo/cant_nodo,3)
  ))

  b_sin_na <- b_sin_na[order(b_sin_na$corte_sup),]

  b_con_na<-data.frame(tbla%>%filter(flag_na==1)%>%group_by(nodo_pred)%>%summarise(
    corte_inf=NA,
    corte_sup=NA,
    pos_nodo=sum(target),
    cant_nodo=n(),
    rt_nodo=round(pos_nodo/cant_nodo,3)
  ))
  devol=rbind(b_con_na, b_sin_na)

  devol$nodo_pred=as.numeric(as.factor(devol$nodo_pred))


devol$variable_name=variable_name
maximo_nodo=max(devol$nodo_pred)

#los que tienen menos de x registros los pongo todos juntos
##volver a calcular cuantos quedan en cada grupo
devol$nodo_pred[devol$cant_var<min_q_casos ]<- 'pocos_casos'




devol$rt_nodo=ifelse(devol$rt_nodo==0,0.0001, ifelse(devol$rt_nodo==1,0.9999, devol$rt_nodo))



devol$log_odds=round(log(devol$rt_nodo/(1-devol$rt_nodo)),3)
devol$participacion=round(devol$cant_nodo/sum(devol$cant_nodo) ,3)

#grupos que quedaron
n_grupos=length(unique(devol$nodo_pred[devol$cant_var>=min_q_casos]))


if(n_grupos>=1 ){
  niveles=sort(unique( as.numeric(as.character(devol$nodo_pred[ devol$cant_var>=min_q_casos ] ))))
  conversion=data.frame(nodo_pred0=niveles,nodo_pred=1:n_grupos )
  print(conversion)
  for (i in niveles){
    devol$nodo_pred[ devol$nodo_pred==i ] = conversion$nodo_pred[conversion$nodo_pred0==i]
  }

}
  devol$rangos_pred<-paste0('(', c(-Inf,devol$corte_inf[2:nrow(devol)]) ,',',
                            c(devol$corte_sup[1:(nrow(devol)-1)], Inf),']')
  columnas_numerica= c('variable_name','nodo_pred', 'rangos_pred',
                       'cant_nodo' ,'pos_nodo', 'rt_nodo',
                       'participacion', 'log_odds',
                       'corte_inf', 'corte_sup')
  devol=devol[,columnas_numerica]

return(devol)
}
