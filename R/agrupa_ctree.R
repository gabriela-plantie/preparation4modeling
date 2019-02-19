#' agrupa ctree
#'
#' group values of ordinal varibles according to a target variable
#' .
#' @param tbla table with data. It has to have the variable and the target variable.
#' @param variable_name name of the variable that you want to analyze.
#' @param target-name name of the target variable.
#' @keywords
#' @export
#' @examples
#' set.seed(1)
#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' x3= ifelse(as.factor(x2>0.5)==T, 'A', 'B')
#' z = 1 + 2 \* x1 + 3 \* x2
# #' z = 1 + 2 * x1 + 3 * x2
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2, x3=x3)
#' tbla[ 1:10, 'x1']<-NA  #generate NA
#' performance_bivariado_transf_log (tbla, variable_name='x1',target_name='y' )
#' performance_bivariado_transf_log (tbla, variable_name='x2',target_name='y' )
