#' performance_bivariado_rpart
#'
#' group values of ordinal varibles according to a target variable
#' .
#' @param tbla table with data. It has to have the variable and the target variable.
#' @param variable_name name of the variable that you want to analyze.
#' @param target-name name of the target variable.
#' @keywords
#' @import pROC
#' @import rpart
#' @export
#' @examples
#' set.seed(1)
#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' x3= ifelse(as.factor(x2>0.5)==T, 'A', 'B')
#' z = 1 + 2 \* x1 + 3 \* x2
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2, x3=x3)
#' performance_bivariado_rpart (tbla, variable_name='x3',target_name='y' )
#' performance_bivariado_rpart (tbla, variable_name='x2',target_name='y' )


performance_bivariado_rpart <- function(tbla, variable_name,target_name ){
  tbla<-data.frame(tbla)

  tbla$y<-tbla[, target_name]
  rp <- rpart(y~get(variable_name),data=tbla )
  tbla$pred=predict(rp, tbla)
  cero=sort(unique(tbla$y))[1]
  uno=sort(unique(tbla$y))[2]

  ks_valor=ks.test(tbla$pred[df$y==cero], tbla$pred[tbla$y==uno])
  auc_valor =auc(tbla$y,tbla$pred )
  gini_valor=(2*auc_valor - 1)

  devuelve=list(variable_name=variable_name, ks_valor=ks_valor,auc_valor=auc_valor, gini_valor=gini_valor)
  return(devuelve)
}

