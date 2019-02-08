
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
      res_nom<-agrupa_ordinal(tbla_0=train_tbl, variable_name=i, target_name, q=num_ini_groups ,limite)
      res_nom$neg<-NULL
      colnames(res_nom)<-c('grupos','pos' ,'tot', 'rt' )
      res_nom<-res_nom[, c('grupos','pos' ,'tot', 'rt' )]
      res_nom$var=i
      res_tbla=rbind(res_tbla, res_nom)
      }
    j=j+1
  }
  res_tbla$log_odds=log(res_tbla$rt/(1-res_tbla$rt))
  tbla_fin<-res_tbla
  return(tbla_fin)
}
