agrupa_ctree_categorica<-function(tbla, target_name, variable_name,
         max_q_groups=20, min_q_casos=100, algoritmo='chaid', niterations=10000) {
  tbla<-data.frame(tbla)
  devuelve<-data.frame()

  tbla$target<-tbla[,target_name] #hago una copia
  tbla$variable_valor<-tbla[,variable_name] #hago una copia


  tbla$variable_valor<-as.character(tbla$variable_valor)
  #table(tbla$variable_valor)
  tbla$variable_valor[is.na(tbla$variable_valor)|tbla$variable_valor=='']='sinValor'
  tbla$variable_valor<-as.factor(tbla$variable_valor)


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
  b<- data.frame(tbla%>%group_by(nodo_pred)%>%summarise(pos_nodo=sum(target),
                                                        cant_nodo=n(),
                                                        rt_nodo=round(pos_nodo/cant_nodo,3)
  ))

  c<- data.frame(tbla%>%group_by(variable_valor, nodo_pred)%>%summarise(pos_var=sum(target),
                                                                        cant_var=n(),
                                                                        rt_var=round(pos_var/cant_var,3)
  ))
  d<-merge(b, c, by='nodo_pred')
  d<- d[order(d$rt_var),]
  d$nodo_pred<-as.numeric(as.factor(d$nodo_pred))
  devol=d


  devol$variable_name=variable_name
  maximo_nodo=max(devol$nodo_pred)

  #los que tienen menos de x registros los pongo todos juntos
  ##volver a calcular cuantos quedan en cada grupo
  devol$nodo_pred[devol$cant_var<min_q_casos ]<- 'pocos_casos'

  devol$rt_nodo<-NULL
  devol$pos_nodo<-NULL
  devol$cant_nodo<-NULL
  b<- data.frame(devol%>%group_by(nodo_pred)%>%summarise(pos_nodo=sum(pos_var),##sobre la tabla ya agrupada
                                                         cant_nodo=sum(cant_var),
                                                         rt_nodo=round(pos_nodo/cant_nodo,3)
  ))
  b$participacion=round(b$cant_nodo/sum(b$cant_nodo) ,3)

  devol=merge(devol, b, by='nodo_pred')


  devol$rt_nodo=ifelse(devol$rt_nodo==0,0.0001, ifelse(devol$rt_nodo==1,0.9999, devol$rt_nodo))



  devol$log_odds=round(log(devol$rt_nodo/(1-devol$rt_nodo)),3)

#grupos que quedaron
  n_grupos=length(unique(devol$nodo_pred[devol$cant_var>=min_q_casos]))


  if(n_grupos>=1 ){
    niveles=sort(unique( as.numeric(as.character(devol$nodo_pred[ devol$cant_var>=min_q_casos ] ))))
    conversion=data.frame(nodo_pred0=niveles,nodo_pred=1:n_grupos )
    #print(conversion)
    for (i in niveles){
      devol$nodo_pred[ devol$nodo_pred==i ] = conversion$nodo_pred[conversion$nodo_pred0==i]
    }

  }


  columnas_categorica= c('variable_name','nodo_pred', 'variable_valor',
                         'cant_nodo' ,'pos_nodo', 'rt_nodo',
                         'participacion', 'log_odds',
                         'cant_var', 'pos_var', 'rt_var')
  devol=devol[,columnas_categorica]

  #devol=devol[order(devol$nodo_pred, -devol$rt_var),]
  return(devol)
  }
