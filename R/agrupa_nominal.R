
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
#'agrupa_nominal(tbla, variable_name='grupo', target_name='valor', limite=0.05)



agrupa_nominal<-function(tbla, variable_name, target_name, limite, symbol_to_split){
  tbla<-data.frame(tbla)
  print(paste0("código para los NA es 'NoValor'. Hay: ", sum(is.na(tbla[, variable_name])), ' NA. '))
  tbla[ is.na(tbla[,variable_name]) , variable_name]<-'NoValor'
  tbla[ tbla[,variable_name]=='' , variable_name]<-'NoValor'


  tbla_agrupada<-devuelve_tabla_agrupada(tbla, variable_name, target_name)
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
      tbla_agrupada<-data.frame( tbla_agrupada%>%group_by(get(variable_name))%>%
                                   summarise(pos=sum(pos), tot=sum(tot)))
      colnames(tbla_agrupada)<-c(variable_name, 'pos', 'tot')
      grupos_1<-nrow(tbla_agrupada)
    } else {
      break
    }
  }
  tbla_agrupada$rt=round(tbla_agrupada$pos/tbla_agrupada$tot,3)
  print(paste0('-->grupos_finales:', nrow(tbla_agrupada)))
  return(tbla_agrupada)
}
