
#' agrupa_nominal
#'
#' group values of nominal variables according to a target variable
#' .
#' @param tbla table with data. It has to have the nominal variable and the target variable.
#' @param variable_name name of the ordinal variable that you want to group.
#' @param target_name name if the target variable.
#' @param limite probability of not belonging to the same group. Used for the hypergeometric test.
#' @param symbol_to_split symbol to split values of variables
#' @keywords
#' @export
#' @examples
#'set.seed(1)
#'tbla<-data.frame(grupo=rep(c('a','b','c','d','e'),100/5),valor=as.numeric(runif(100)>.5 ))
#'agrupa_nominal(tbla, target_name='valor', variable_name='grupo',limite=0.05, symbol_to_split='%#%')


agrupa_nominal<-function(tbla,  target_name, variable_name, limite, symbol_to_split){
  tbla<-data.frame(tbla)
  print(paste0("código para los NA es 'NoValor'. Hay: ", sum(is.na(tbla[, variable_name])), ' NA. '))
  tbla[,variable_name]<-as.character(tbla[,variable_name])
  tbla[ is.na(tbla[,variable_name]) , variable_name]<-'NoValor'
  tbla[ tbla[,variable_name]=='' , variable_name]<-'NoValor'

  tbla_agrupada0<-devuelve_tabla_agrupada(tbla, variable_name, target_name)


  tbla_agrupada<-tbla_agrupada0
  grupos_0<-4
  grupos_1<-3
  tbla_agrupada[  , variable_name]<-as.character(tbla_agrupada[  , variable_name])
  while(grupos_0>grupos_1 & grupos_1>2 ){
    grupos_0<-nrow(tbla_agrupada)
    mymat=arma_matriz_test(tbla_agrupada, variable_name)
    if(max(mymat, na.rm=T)>limite){
      resultado=genera_mayor(mymat, tbla_agrupada, variable_name)
      nuevo_nombre=paste(unlist(resultado[3:4]),  collapse=symbol_to_split)
      tbla_agrupada[  tbla_agrupada[  , variable_name]== resultado[3], variable_name]= nuevo_nombre
      tbla_agrupada[  tbla_agrupada[  , variable_name]== resultado[4], variable_name]= nuevo_nombre
      tbla_agrupada$grupo<-tbla_agrupada[,variable_name]
      tbla_agrupada<-data.frame( tbla_agrupada%>%
                                   group_by(get(variable_name))%>%
                                   summarise(pos=sum(pos), tot=sum(tot)))
      colnames(tbla_agrupada)<-c(variable_name, 'pos', 'tot')
      grupos_1<-nrow(tbla_agrupada)
    } else {
      break
    }
  }
  tbla_agrupada$rt=round(tbla_agrupada$pos/tbla_agrupada$tot,3)
  tbla_agrupada$rt=ifelse(tbla_agrupada$rt==0,0.0001, ifelse(tbla_agrupada$rt==1,0.9999, tbla_agrupada$rt))

   print(paste0('-->grupos_finales:', nrow(tbla_agrupada)))

  sub_tbla<-tbla_agrupada

  lista=strsplit(sub_tbla[,variable_name], symbol_to_split)
  #print(lista)
  tbla_var=data.frame()

  for (j in 1:length(lista)){
    if(sub_tbla$tot>0){
      valor=unlist(lista[j])
      if(length(valor)==0){valor="" }
      #print(lista[j])# j=1
      tbla_vert=data.frame(variable_valor=valor)
      tbla_vert$rt_nodo=sub_tbla$rt[j]
      tbla_vert$cant_nodo=sub_tbla$tot[j]
      tbla_vert$pos_nodo=sub_tbla$pos[j]
      tbla_vert$nodo_pred=j
      tbla_vert$variable_name=variable_name
      tbla_var=rbind(tbla_var, tbla_vert)
    }
  }

  tbla_var$log_odds=round(log(tbla_var$rt_nodo/(1-tbla_var$rt_nodo)),3)
  tbla_var$participacion=round(tbla_var$cant_nodo/sum(tbla_var$cant_nodo),3)
  tbla_agrupada0$neg<-NULL
  colnames(tbla_agrupada0)<-c('variable_valor', 'cant_var', 'pos_var', 'rt_var')
  devol=merge(tbla_var, tbla_agrupada0,  by.x='variable_valor', by.y='variable_valor')
  columnas_categorica= c('variable_name','nodo_pred', 'variable_valor',
                         'cant_nodo' ,'pos_nodo', 'rt_nodo',
                         'participacion', 'log_odds',
                         'cant_var', 'pos_var', 'rt_var')

 devol<- devol[,columnas_categorica]
 devol = devol[order(devol$rt_var),]
 return(devol)
}
