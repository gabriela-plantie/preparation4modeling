#' performance_bivariado_dummies_log
#'
#' group values of categorical variables according to a target variable
#' .
#' @param tbla table with data. It has to have the variable and the target variable.
#' @param variable_name name of the variable that you want to analyze.
#' @param target-name name of the target variable.
#' @keywords
#' @import fastDummies
#' @export
#' @examples
#' set.seed(1)
#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' x3= ifelse(as.factor(x2>0.5)==T, 'A', 'B')
#'  z = 1 + 2 * x1 + 3 * x2
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2, x3=x3)
#' tbla[ 1:10, 'x1']<-NA  #generate NA
#' tbla[ 10:20, 'x3']<-''  #generate NA

#' performance_bivariado_dummies_log (tbla, variable_name='x3',target_name='y' )


performance_bivariado_dummies_log<-function(tbla, variable_name, target_name,limite_steps){
  print('performance_bivariado_dummies_log')
  tbla<-data.frame(tbla)
  tbla$y<-tbla[, target_name]

  tbla[, variable_name]<-as.character(tbla[, variable_name])
  #NA, nulos
  nulos_index=tbla[, variable_name]==''
  tbla[nulos_index, variable_name]='nulo'
  print(paste0(variable_name, ': nulos: ', sum(nulos_index), ' . Imputados con "nulo"'))

  #sacar caracteres especiales y pasar a minuscula
  tbla[, variable_name]=quita_char_especiales(tbla[, variable_name])


  tbla1<- dummy_cols(tbla[, variable_name])
  tbla1$.data<-NULL
  colnames(tbla1)=gsub('.data', '', colnames(tbla1))

  colnames(tbla1)=c(paste0(variable_name, colnames(tbla1) ))
  tbla2=cbind(y=tbla$y, tbla1)

  print('entrena logistica')

  form_2=paste(colnames(tbla1), collapse=' + ')
  form_all=formula(paste0('y ~ ', form_2))

  print(paste0('entrena logistica: ', length(colnames(tbla1)), ' variables'))

  devuelve=performance_modelo_logistica(tbla2, mod_all, variable_name, form_all, limite_steps)
  return(devuelve)

}
