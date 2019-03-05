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
#' z = 1 + 2 * x1 + 3 * x2
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2, x3=x3)
#' filtros_train= (tbla$random=runif(nrow(tbla)))<0.5
#' estabilidad_poder_predictivo_de_variable (tbla, variable_name='x3',target_name='y', filtros_train= )
#' estabilidad_poder_predictivo_de_variable (tbla, variable_name='x2',target_name='y',filtros_train )


estabilidad_poder_predictivo_de_variable <- function(tbla, variable_name,target_name, filtros_train, var_particion_estabilidad ){
  #target_name='y'
  #library(pROC)
  #primero entreno el modelo de logística con las variables que pasan
  #var_particion_estabilidad='x1'
  print('ENTRENAMIENTO')
  tbla<-data.frame(tbla)
  tbla$target<-tbla[, target_name]
  tbla$variable<-tbla[, variable_name]#si es categorica ya debe estar particionada
  #no debe haber NA ni infinite

  f= formula(target~ variable)
  lr <- glm(f, tbla[ filtros_train, ], family = 'binomial')
  tbla$pred=predict(lr, tbla)

  cero=sort(unique(tbla$target))[1]
  uno=sort(unique(tbla$target))[2]

  ks_valor= ks.test(tbla$pred[tbla$target==cero], tbla$pred[tbla$target==uno])
  ks_valor=as.numeric(ks_valor$statistic)
  auc_valor = as.numeric(auc(tbla$target,tbla$pred ))
  gini_valor=(2*auc_valor - 1)

  devuelve_train=data.frame(variable_name=variable_name, criterio='train', ks_valor=ks_valor,auc_valor=auc_valor, gini_valor=gini_valor)

  print('ESTABILIDAD')
  tbla$particion<-tbla[, var_particion_estabilidad]

  if(is.numeric(tbla$particion)==T){
    cortes=quantile(tbla$particion, probs=seq(0,1,0.1))
    tbla$particion_cortada=cut(tbla$particion, cortes)
    } else {tbla$particion_cortada<-tbla$particion}

  niveles=sort(unique(tbla$particion_cortada))
  for (i in niveles){#i=niveles[1]
    subtbla<-tbla[tbla$particion_cortada %in% i,]
    subtbla$pred=predict(lr, subtbla)

    cero=sort(unique(subtbla$target))[1]
    uno=sort(unique(subtbla$target))[2]

    ks_valor= ks.test(subtbla$pred[subtbla$target==cero], subtbla$pred[subtbla$target==uno])
    ks_valor=as.numeric(ks_valor$statistic)
    auc_valor = as.numeric(auc(subtbla$target,subtbla$pred ))
    gini_valor=(2*auc_valor - 1)

    devuelve_test0=data.frame(variable_name=variable_name, criterio=i, ks_valor=ks_valor,auc_valor=auc_valor, gini_valor=gini_valor)

    }

  return(devuelve)
}
