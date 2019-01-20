
#' aplica_agrupa_nominal
#'
#' apply agrupa_nominal to a list of variables
#' .
#' @param train_tbl table with data. It has to have the nominal variable and the target variable.
#' @param char_cols name of the nominal variables that you want to group.
#' @param target_name name if the target variable.
#' @param lim_categ probability of not belonging to the same group. Used for the hypergeometric test.
#' @keywords
#' @export
#' @examples
#' tbla<-data.frame(grupo1=rep(c('a','b','c','d','e'),100/5),
#' valor=c( as.numeric(rep(a, 18)>.5) , rep(0, 5), rep(1, 5)) ,
#' grupo2=rep(c('aa','bb','cc','cc','cc'),100/5) )
#' agrupa_nominal(tbla, variable_name='grupo1', target_name='valor', limite=0.05)
#' aplica_agrupa_nominal(train_tbl=tbla, char_cols=c('grupo1', 'grupo2'), target_name='valor', lim_cant_categ=20, limite=0.05)





#aplica a una tabla y una lista de variables la funcion agrupa nominal
#agrupa haciendo el test de la hipergeometrica (con funcion propia en git)--> convertir a funcion

aplica_agrupa_nominal<-function(train_tbl, char_cols, target_name, lim_cant_categ, limite, symbol_to_split){
  train_tbl<-data.table(train_tbl)
  res_tbla=data.frame()
  j=1
  tot=length(char_cols)
  for (i in char_cols){#i=char_cols[8]
    distintos=length( unique(train_tbl[,get(i)]))

    if( distintos <=lim_cant_categ & distintos>2 ){
      print(paste0(j, ' de ', tot ,' - agrupa ', i , ' _ niveles: ', distintos))
      res_nom<-agrupa_nominal(train_tbl, i, target_name, limite, symbol_to_split)
      res_nom$neg<-NULL
      colnames(res_nom)<-c('grupos','pos' ,'tot', 'rt' )
      res_nom$var=i
      res_tbla=rbind(res_tbla, res_nom)
    }else {print(paste0('no agrupa ', i, ' _ niveles: ', distintos))}
    j=j+1
  }
  res_tbla$log_odds=log(res_tbla$rt/(1-res_tbla$rt))

  print('##########formato')
  tbla_fin=data.frame()
  for (i in unique(res_tbla$var)){#i=unique(res_tbla$var)[2]
    #print(i)
    sub_tbla=res_tbla[res_tbla$var==i,c('grupos','rt' ,'log_odds', 'tot')]
    lista=strsplit(sub_tbla$grupos, symbol_to_split)
    tbla_var=data.frame()

    for (j in 1:length(lista)){
      if(sub_tbla$tot>0){
        valor=unlist(lista[j])
        if(length(valor)==0){valor="" }
        #print(lista[j])# j=1
        tbla_vert=data.frame(valor_agrup=valor)
        tbla_vert$rt=sub_tbla$rt[j]
        tbla_vert$grupo=j
        tbla_vert$variable=i
        tbla_var=rbind(tbla_var, tbla_vert)
      }
    }
    tbla_fin=rbind(tbla_fin, tbla_var)
  }

  tbla_fin$log_odds=log(tbla_fin$rt/(1-tbla_fin$rt))

  return(tbla_fin)
}
