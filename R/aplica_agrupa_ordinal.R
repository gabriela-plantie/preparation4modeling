
#' aplica_agrupa_ordinal
#'
#' apply agrupa_ordinal to a list of variables
#' .
#' @param train_tbl table with data. It has to have the nominal variable and the target variable.
#' @param num_cols name of the numeric variables that you want to group.
#' @param target_name name of the target variable.
#' @param num_ini_groups probability of not belonging to the same group. Used for the hypergeometric test.
#' @param lim_categ probability of not belonging to the same group. Used for the hypergeometric test.
#' @keywords
#' @export
#' tbla<-data.frame(
#' grupo1=rep(1:5,100/5),
#' valor=c( as.numeric(rep(runif(90), 1)>.5) , rep(0, 5), rep(1, 5)) ,
#' grupo2=rep(c(11,22,33,33,33),100/5)
#' )
#'  aplica_agrupa_ordinal(train_tbl=tbla, num_cols=c('grupo1', 'grupo2'), target_name='valor', num_ini_groups=20, limite=0.05)



#train_tbl=vars_target[filtros_train,]
aplica_agrupa_ordinal<-function(train_tbl, num_cols, target_name, num_ini_groups, limite){
  train_tbl<-data.table(train_tbl)
  #  num_cols %in% colnames(train_tbl)
  res_tbla=data.frame()
  j=1
  tot=length(num_cols)

  for (i in num_cols){#i=num_cols[2]
    distintos=unique(data.frame(train_tbl[,i, with=F]))
    distintos=distintos[!is.na(distintos)]
    if(length(distintos)<=2){        print(paste0(j, ' de ', tot ,' - no agrupa ', i))
      }else{
    print(paste0(j, ' de ', tot ,' - agrupa ', i))
#?agrupa_ordinal
    #class(train_tbl)
    #train_tbl$precio_alta
      res_nom<-agrupa_ordinal(tbla_0=train_tbl,  target_name,variable_name=i, grupos_iniciales=num_ini_groups ,limite)
     res_tbla=rbind(res_tbla, res_nom)
      }
    j=j+1
  }
  tbla_fin<-res_tbla
  return(tbla_fin)
}
