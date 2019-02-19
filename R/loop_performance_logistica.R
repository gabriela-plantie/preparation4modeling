
#' loop_performance_logistica
#'
#' bivariate analysis
#' .
#' @param tbla table with data. It has to have the variables and the target variable.
#' @param lista list of names of the variables that you want to analyze.
#' @param target-name name of the target variable.
#' @param limite_categ max number of categories of the variable to be considered.
#' @param limite_steps max number of steps for the stepAIC.
#' @keywords
#' @import pROC
#' @import rpart
#' @export
#' @examples
#' set.seed(1)
#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' z = 1 + 2 \* x1 + 3 \* x2 ^2
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2)
#' loop_performance_logistica (tbla, lista=c('x1', 'x2'),target_name='y', flag_numerica=1 )
#' loop_performance_logistica (tbla, lista=c('x3'),target_name='y' , flag_numerica=0)



loop_performance_logistica <- function(tbla, lista,target_name, flag_numerica=0 , limite_categ=100, limite_steps=500){
  print('loop_performance_logistica')
  tbla<-data.frame(tbla)
  tbla$y<-tbla[, target_name]
  q_vars=length(lista)
  df_return=data.frame()
  for (i in (1:q_vars) ){#variable_name=lista[1]
      variable_name =lista[i]

      print(paste0('**********************',variable_name, ' ', i, ' de ', q_vars))
        if(flag_numerica==1) {
            linea=performance_bivariado_transf_log (tbla, variable_name=variable_name,target_name='y', limite_steps )
        }
        if(flag_numerica==0) {
            niveles=length(unique(tbla[, variable_name]))
            print(paste0('niveles: ', niveles))
            if(niveles<=limite_categ ){
                linea= performance_bivariado_dummies_log(tbla, variable_name, target_name,limite_steps)
            }
        }

  df_return=rbind(df_return, linea)
  }
  return(df_return)
}


