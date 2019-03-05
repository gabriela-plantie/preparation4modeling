
#' aplica_agrupa_nominal
#'
#' apply agrupa_nominal to a list of variables
#'
#' @param train_tbl table with data. It has to have the nominal variable and the target variable.
#' @param char_cols name of the nominal variables that you want to group.
#' @param target_name name if the target variable.
#' @param lim_categ probability of not belonging to the same group. Used for the hypergeometric test.
#' @keywords
#' @export
#' @examples
#' tbla<-data.frame(
#' grupo1=rep(c('a','b','c','d','e'),100/5),
#' valor=c( as.numeric(rep(runif(90), 1)>.5) , rep(0, 5), rep(1, 5)) ,
#' grupo2=rep(c('aa','bb','cc','cc','cc'),100/5)
#' )
#'  aplica_agrupa_nominal(train_tbl=tbla, char_cols=c('grupo1', 'grupo2'), target_name='valor', lim_cant_categ=20, limite=0.05, symbol_to_split = '%#%')





aplica_agrupa_nominal<-function(train_tbl, char_cols, target_name, lim_cant_categ, limite, symbol_to_split){

  # aplica a una tabla y una lista de variables la funcion agrupa nominal
  # agrupa haciendo el test de la hipergeometrica (con funcion propia en git)--> convertir a funcion

  train_tbl<-data.frame(train_tbl)
  res_tbla=data.frame()
  j=1
  tot=length(char_cols)
  for (i in char_cols){#i=char_cols[8]
    distintos=length( unique(train_tbl[,i]))

    if( distintos <=lim_cant_categ & distintos>2 ){
      print(paste0(j, ' de ', tot ,' - agrupa ', i , ' _ niveles: ', distintos))
      res_nom<-agrupa_nominal(train_tbl,  target_name,i, limite, symbol_to_split)
      res_tbla=rbind(res_tbla, res_nom)
    }else {print(paste0('no agrupa ', i, ' _ niveles: ', distintos))}
    j=j+1
  }

  return(res_tbla)
}
