#' performance_bivariado_transf_log
#'
#' group values of ordinal varibles according to a target variable
#' .
#' @param tbla table with data. It has to have the variable and the target variable.
#' @param mod_all formula
#' @variable_name variable to analize
#' @keywords
#' @import pROC
#' @import MASS
#' @export
#' @examples

performance_modelo_logistica<-function(tbla2, mod_all, variable_name, form_all, limite_steps){
    set.seed(55555)
    print('inicia train stepwise')
    mod_all = glm(y ~ 1, family=binomial, data = tbla2)
    #library(MASS)
    mod_all = stepAIC(mod_all, scope = form_all, family=binomial, data = tbla2, k = 3, trace=F, steps = limite_steps)

    result=summary(mod_all)
    result_df<-data.frame(result$coefficients)

    quedan=rownames(result_df)[rownames(result_df)!='(Intercept)']
    quedan_valor = paste(quedan, collapse = ",")

    tbla2$pred<-predict(mod_all, tbla2)
    niveles=unique(tbla2$y)

    ks_valor=ks.test(tbla2$pred[tbla2$y==niveles[1]], tbla2$pred[tbla2$y==niveles[2]])
    ks_valor = as.numeric(ks_valor$statistic)
    auc_valor =as.numeric(auc(tbla2$y,tbla2$pred ))
    gini_valor=(2*auc_valor - 1)




    devuelve = data.frame(variable_name = variable_name, ks_valor = ks_valor,
                       auc_valor = auc_valor, gini_valor = gini_valor,
                       quedan = quedan_valor)



    return(devuelve)
}
