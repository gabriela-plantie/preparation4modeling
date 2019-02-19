
#' loop_performance_rpart
#'
#' bivariate analysis
#' .
#' @param tbla table with data. It has to have the variables and the target variable.
#' @param lista list of names of the variables that you want to analyze.
#' @param target-name name of the target variable.
#' @keywords
#' @import pROC
#' @import rpart
#' @export
#' @examples
#' set.seed(1)
#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' z = 1 + 2 \* x1 + 3 \* x2
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2)
#' loop_performance_rpart (tbla, lista=c('x1', 'x2'),target_name='y' )

loop_performance_rpart <- function(tbla, lista,target_name ){
  tbla<-data.frame(tbla)
  tbla$y<-tbla[, target_name]
  df_return=data.frame()
  for (variable_name in lista){#variable_name=lista[1]
    devuelve=performance_bivariado_rpart (tbla, variable_name=variable_name,target_name='y' )
    variable_name=devuelve$variable_name
    ks_valor=as.numeric(devuelve$ks_valor$statistic)
    auc_valor=as.numeric(devuelve$auc_valor)
    gini_valor=devuelve$gini_valor
    linea=data.frame(variable_name=variable_name, ks_valor=ks_valor, auc_valor=auc_valor, gini_valor=gini_valor)
    df_return=rbind(df_return, linea)
    }
  return(df_return)
}
