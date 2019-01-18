

#' agrupa_ordinal
#'
#' group values of ordinal varibles according to a target variable
#' .
#' @param tbla table with data. It has to have the ordinal variable and the target variable.
#' @param variable_name name of the ordinal variable that you want to group.
#' @param target-name name if the target variable.
#' @param q number of quantiles that you want to create. Uses quantile function.
#' @param limite probability of not belonging to the same group. Used for the hypergeometric test.
#'
#' @keywords
#' @export
#' @examples
#'set.seed(1)
#'valor_variable1=round(runif(100)*10,0)
#'target_variable1=as.numeric((valor_variable1/10+runif(100)*0.25) >0.525)
#'tbla<-data.frame(variable1=valor_variable1, target_n= target_variable1)
#'agrupa_ordinal(tbla, 'variable1', 'target_n', 10, 0.8)



#tbla_0=tbla
agrupa_ordinal<-function(tbla_0, variable_name, target_name, q, limite){
  #variable_name='variable1'; target_name='target';q=20; limite=0.6
  #print('inicio')
  tbla_0=data.frame(tbla_0)

  distintos=length(unique(tbla_0[,variable_name]))
  q=min(q, distintos) ##para que no falle nunca

  if(distintos<q){stop('number of quantiles should be smaller than number of unique values of the variable to be analyzed')}
  tbla<-data.frame(tbla_0)
  tbla<-tbla[!is.na(tbla[, variable_name]),]
  tbla$deciles<-genera_deciles(tbla, variable_name, target_name, q)
  tbla_agrupada<-devuelve_tabla_agrupada(tbla, 'deciles', target_name)
  sentido=encuentra_sentido(tbla_0, variable_name, target_name)
  #print(tbla_agrupada)
  tabla_final<-test_hyper2(tbla_agrupada, var_en_rangos='deciles', sentido, limite)
  return(tabla_final)
}

#agrupa_ordinal(tbla, 'variable1', 'target_n', 10, 0.8)

