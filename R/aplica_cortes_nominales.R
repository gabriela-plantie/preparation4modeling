
#' aplica_cortes_nominales
#'
#' apply the cuts obtained with aplica_agrupa_nominal to a list of new variables
#' .
#' @param tbla_cortes_variables_nom table with cuts return by aplica_agrupa_nominal.
#' @param tbl table with data to apply the cuts.
#' @param campo_merge name of the variable used to merge the result and the data (index).
#' @keywords
#' @export
#' @examples


#' tbla<-data.frame(
#' id=1:100,
#' grupo1=rep(c('a','b','c','d','e'),100/5),
#' valor=c( as.numeric(rep(runif(90), 1)>.5) , rep(0, 5), rep(1, 5)) ,
#' grupo2=rep(c('aa','bb','cc','cc','cc'),100/5))
#' tbl_grupos=aplica_agrupa_nominal(train_tbl=tbla, char_cols=c('grupo1', 'grupo2'), target_name='valor', lim_cant_categ=20, limite=0.05, symbol_to_split = '%#%')
#' aplica_cortes_nominales(tbla_cortes_variables_nom=tbl_grupos, tbl=tbla, campo_merge = 'id')


aplica_cortes_nominales<-function(tbla_cortes_variables_nom, tbl, campo_merge){
  variables_nom<-unique(tbla_cortes_variables_nom$variable_name)
  tbla_cortes_variables_nom<-unique(tbla_cortes_variables_nom)

  tbl<-data.frame(tbl)
  fin=tbl[,campo_merge]
  fin=fin[fin>0 & !is.na(fin)]
  fin=fin[!fin%in% as.numeric(names(table(fin)[table(fin)>1]))]
  #length(unique(fin$contratopais))-nrow(fin)
  fin=data.frame(campo_merge=fin)
  colnames(fin)<-campo_merge

  #delete repeated index
  contratos_rep_sacar= table(fin[, campo_merge])[table(fin[, campo_merge])>1]
  print(paste0('elimina los repetidos: ', unlist(contratos_rep_sacar)))
  fin<-fin[ ! as.numeric(fin[, campo_merge]) %in% as.numeric(names(contratos_rep_sacar)),]

  print('aplica los cortes')
  fin=data.frame(campo_merge=fin)
  colnames(fin)<-campo_merge

  for (i in variables_nom){#i='paquete_comercial' variables_nom[1]
    print(paste0( '______________ ', i, '. Dim fin: ', nrow(tbl)))
    a=tbla_cortes_variables_nom[tbla_cortes_variables_nom$variable_name==i, ]
    vars_grupo =  tbl[,c(i, campo_merge)] ##aplico a todos
    print(paste0('niveles originales: ',  length(table(unique(tbl[,i]))),
                 ' -->  niveles finales: ', length(unique(a$nodo_pred)) ))


    b=merge(vars_grupo,a, by.x=i, by.y='variable_valor', all.x=T )
    print(dim(b))
    b=b[, c(campo_merge,'nodo_pred' ,'rt_nodo', 'log_odds')]

    colnames(b)[2]=paste0(i, '_grupo')
    colnames(b)[3]=paste0(i, '_rt')
    colnames(b)[4]=paste0(i, '_log_odds')

    print(paste0('dim b: ', list(dim(b))))

    fin=merge( fin ,b, by=eval(campo_merge), all.x=T)
    print(paste0('dim fin: ', list(dim(fin))))

  }
  return(fin)
}
