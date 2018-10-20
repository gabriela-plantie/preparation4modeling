
#' agrupa_nominal
#'
#' group values of nominal varibles according to a target variable
#' .
#' @param tbla table with data. It has to have the nominal variable and the target variable.
#' @param variable_name name of the ordinal variable that you want to group.
#' @param target_name name if the target variable.
#' @param limite probability of not belonging to the same group. Used for the hypergeometric test.
#' @keywords
#' @export
#' @examples
#'set.seed(1)
#'a=c(0.2, 0.3, 0.4, 0.7, 0.8, 0.9)
#'b=log(a/(1-a))
#'tbla<-data.frame(grupo=rep(c('a','b','c','d','e'),100/5),valor=as.numeric(runif(100)>.5 ))
#'agrupa_nominal(tbla, variable_name='grupo', target_name='valor', limite=0.05)


agrupa_nominal<-function(tbla, variable_name, target_name, limite){
  tbla_agrupada<-devuelve_tabla_agrupada(tbla, variable_name, target_name)
  grupos_0<-4
  grupos_1<-3
  tbla_agrupada[  , variable_name]<-as.character(tbla_agrupada[  , variable_name])
  while(grupos_0>grupos_1 & grupos_1>2 ){
    grupos_0<-nrow(tbla_agrupada)
    mymat=arma_matriz_test(tbla_agrupada, variable_name)
    if(max(mymat, na.rm=T)>limite){
      resultado=genera_mayor(mymat, tbla_agrupada, variable_name)
      nuevo_nombre=paste(unlist(resultado[3:4]),  collapse='-')
      tbla_agrupada[  tbla_agrupada[  , variable_name]== resultado[3], variable_name]= nuevo_nombre
      tbla_agrupada[  tbla_agrupada[  , variable_name]== resultado[4], variable_name]= nuevo_nombre
      tbla_agrupada$grupo<-tbla_agrupada[,variable_name]
      tbla_agrupada<-data.frame( tbla_agrupada%>%group_by(get(variable_name))%>%
                                   summarise(pos=sum(pos), tot=sum(tot)))
      colnames(tbla_agrupada)<-c(variable_name, 'pos', 'tot')
      grupos_1<-nrow(tbla_agrupada)
    }
  }
  tbla_agrupada$rt=round(tbla_agrupada$pos/tbla_agrupada$tot,3)
  return(tbla_agrupada)
}
