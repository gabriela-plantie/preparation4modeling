#' agrupa_nominal_filtra_small
#'
#' group values of nominal variables according to a target variable
#' .
#' @param tbla table with data. It has to have the nominal variable and the target variable.
#' @param variable_name name of the ordinal variable that you want to group.
#' @param target_name name if the target variable.
#' @param limite probability of not belonging to the same group. Used for the hypergeometric test.
#' @param symbol_to_split symbol to split values of variables
#' @param limite_grupo min number for each group to be analized
#' @keywords
#' @export
#' @examples
#'set.seed(1)
#'tbla<-rbind(data.frame(grupo=rep(c('a','b','c','d','e'),100/5),valor=as.numeric(runif(100)>.5 )),data.frame(grupo=rep(c('c','d'),100/2),valor=as.numeric(runif(100)>.5 )))
#'agrupa_nominal_filtra_small(tbla, target_name='valor', variable_name='grupo',limite=0.05, symbol_to_split='%#%', limite_grupo=50)



agrupa_nominal_filtra_small<-function(tbla,  target_name, variable_name, limite, symbol_to_split='%-%', limite_grupo=100){
  tbla<-data.frame(tbla)
  #print(paste0("código para los NA es 'NoValor'. Hay: ", sum(is.na(tbla[, variable_name])), ' NA. '))
  tbla[,variable_name]<-as.character(tbla[,variable_name])
  tbla[ is.na(tbla[,variable_name]) , variable_name]<-'NoValor'
  tbla[ tbla[,variable_name]=='' , variable_name]<-'NoValor'

  tbla_agrupada0<-devuelve_tabla_agrupada(tbla, variable_name, target_name)
  colnames(tbla_agrupada0)<-c('variable_valor', 'cant_var', 'pos_var','neg_var', 'rt_var')

  tbla_agrupada_small<- tbla_agrupada0[tbla_agrupada0$cant_var<limite_grupo,]

   #print(paste0('********************grupos con menos casos que ', limite_grupo , ': ',nrow(tbla_agrupada_small)))

  ###################ahora sigo con las agrupaciones#########
  quedan<- tbla_agrupada0[tbla_agrupada0$cant_var>=limite_grupo,'variable_valor']

  tbla<-tbla[tbla[,variable_name] %in% quedan,]

  devol=agrupa_nominal(tbla, target_name, variable_name,limite, symbol_to_split)

    ##### ahora transformo los datos de los que son pocos casos

  if(nrow(tbla_agrupada_small)>0){
  tbla_agrupada_small$nodo_pred<-'pocos_casos'
  tbla_agrupada_small_g<-data.frame(tbla_agrupada_small%>%
                                      group_by(nodo_pred)%>%summarise(
                                        cant_nodo=sum(cant_var),
                                        pos_nodo=sum(pos_var),
                                        rt_nodo =pos_nodo/cant_nodo,
                                        participacion=cant_nodo/sum(tbla_agrupada0$tot),
                                        log_odds= log(rt_nodo/(1-rt_nodo))) )
 nombres=c('variable_name' ,  'nodo_pred','variable_valor','cant_nodo','pos_nodo','rt_nodo',
    'participacion','log_odds',  'cant_var', 'pos_var', 'rt_var')


  tbla_agrupada_small_g2 <-merge(tbla_agrupada_small,tbla_agrupada_small_g)

  tbla_agrupada_small_g2$variable_name=variable_name

  tbla_agrupada_small_g2<-tbla_agrupada_small_g2[, nombres]

  devol2=rbind(devol, tbla_agrupada_small_g2)

  }else {
    devol2=devol
  }


  return(devol2)
}
