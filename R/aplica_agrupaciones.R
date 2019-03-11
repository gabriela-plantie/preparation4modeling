#' aplica_agrupaciones
#'
#' apply groups to a table
#' .
#' @param tbla_result table with data. It has to have the variable .
#' @param tbla_grupos table with groups.
#' @param nombre_variable name of the variable to group.
#' @export
#' @examples
#' set.seed(1)
#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' x4='A'
#' x4=ifelse(x1>0.1,'B', x4)
#' x4=ifelse(x1>0.4,'C', x4 )
#' x4=ifelse(x1>0.6,'D', x4 )
#' x4=ifelse(x1>0.8,'E', x4 )
#' z = 1 + 3*x1
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2, x4=x4)
#' q_nas=100
#' x1[1:q_nas] = NA
#' x4[1:q_nas]=NA
#' a<-  agrupa_ctree (tbla, target_name='y', variable_name='x4',flag_numerica=0 , algoritmo='chaid' )
#' aplica_agrupaciones(tbla_result = tbla, tbla_grupos = a, 'x4')




aplica_agrupaciones<-function(tbla_result,tbla_grupos, nombre_variable){
  for(i in 1:nrow(tbla_grupos)){#i=1
    j=as.character(tbla_grupos$variable_valor[i])
    grupo=tbla_grupos$nodo_pred[i]
    nuevo_nombre=paste0(nombre_variable, '_agrupado')
    tbla_result[which(tbla_result[, nombre_variable]==j) , nuevo_nombre]=grupo
  }
  return(tbla_result)
}
