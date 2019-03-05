

#' agrupa_ordinal
#'
#' group values of ordinal varibles according to a target variable
#' .
#' @param tbla table with data. It has to have the ordinal variable and the target variable.
#' @param variable_name name of the ordinal variable that you want to group.
#' @param target-name name if the target variable.
#' @param grupos_iniciales number of quantiles that you want to create. Uses quantile function.
#' @param limite probability of not belonging to the same group. Used for the hypergeometric test.
#'
#' @keywords
#' @export
#' @examples
#'set.seed(1)
#'valor_variable1=round(runif(100)*10,0)
#'target_variable1=as.numeric((valor_variable1/10+runif(100)*0.25) >0.525)
#'tbla<-data.frame(variable1=valor_variable1, target_n= target_variable1)
#'agrupa_ordinal(tbla,  'target_n','variable1', 10, 0.8)




agrupa_ordinal<-function(tbla_0, target_name, variable_name,  grupos_iniciales, limite){
  q=grupos_iniciales
  #tbla_0=tbla
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

  tabla_final$rt=round(tabla_final$rt,3)
  tabla_final$rt=ifelse(tabla_final$rt==0,0.0001, ifelse(tabla_final$rt==1,0.9999, tabla_final$rt))

  tabla_final$log_odds=round(log(tabla_final$rt/(1-tabla_final$rt)),3)
  tabla_final$participacion=round(tabla_final$tot/sum(tabla_final$tot),3)

  izq=sapply(tabla_final$deciles, function(x) strsplit(x, ',')[[1]][1])
  der=sapply(tabla_final$deciles, function(x) strsplit(x, ',')[[1]][2])

  tabla_final$corte_inf= as.numeric(gsub('\\(', '',izq))
  tabla_final$corte_sup= as.numeric(gsub('\\]', '',der))
  tabla_final$neg=NULL
  colnames(tabla_final)[1:4]<-c('rangos_pred', 'cant_nodo', 'pos_nodo', 'rt_nodo')
  tabla_final$nodo_pred<-as.numeric(as.factor(tabla_final$rangos_pred))
  tabla_final$variable_name<-variable_name
  columnas_numerica=c('variable_name','nodo_pred', 'rangos_pred',
                      'cant_nodo' ,'pos_nodo', 'rt_nodo',
                      'participacion', 'log_odds',  'corte_inf', 'corte_sup')
  return(tabla_final[,columnas_numerica])
}

#agrupa_ordinal(tbla, 'variable1', 'target_n', 10, 0.8)

