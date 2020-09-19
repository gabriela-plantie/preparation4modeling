#' agrupa ctree
#'
#' group values of ordinal or categorical variables according to a target variable
#' .
#' @param tbla table with data. It has to have the variable and the target variable.
#' @param variable_name name of the variable that you want to analyze.
#' @param target_name name of the target variable.
#' @param flag_numerica 1 if the variable is numeric (ordinal), 0 if it is a factor
#' @param max_q_groups maximum number of groups to split the variable
#' @export
#' @import party
#' @import evtree
#' @import dplyr
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
#' agrupa_ctree (tbla, target_name='y', variable_name='x1',flag_numerica=1, max_q_groups=10, algoritmo='chaid' )
#' agrupa_ctree (tbla, target_name='y', variable_name='x1',flag_numerica=1, max_q_groups=100, algoritmo='evetree' )
#' agrupa_ctree (tbla, target_name='y', variable_name='x4',flag_numerica=0 )



agrupa_ctree<-function(tbla, target_name, variable_name, flag_numerica,
                       max_q_groups=20, min_q_casos=100, algoritmo='chaid', niterations=10000){
 if(flag_numerica==1){
  devol=agrupa_ctree_numerica(tbla, target_name, variable_name,
                        max_q_groups=20, min_q_casos=100, algoritmo='chaid', niterations=10000)
}

  if(flag_numerica==0){
    devol=agrupa_ctree_categorica(tbla, target_name, variable_name,
                          max_q_groups=20, min_q_casos=100, algoritmo='chaid', niterations=10000)
    devol<-devol[order(devol$nodo_pred, -devol$rt_var),]
    }

  return(devol)
}
