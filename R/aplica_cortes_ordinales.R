
#' aplica_cortes_ordinales
#'
#' apply the cuts obteined with aplica_agrupa_nominal to a list of new variables
#' .
#' @param tbla_cortes_variables_ord table with cuts return by aplica_agrupa_nominal.
#' @param tbl table with data to apply the cuts.
#' @param campo_merge name of the variable used to merge the result and the data (index).
#' @keywords
#' @import sqldf
#' @export
#' @examples



aplica_cortes_ordinales<-function(tbla_cortes_variables_ord, tbl, campo_merge){
  tbl=data.frame(tbl)

  fin=tbl[,campo_merge]
  fin=fin[fin>0 & !is.na(fin)]
  fin=fin[!fin%in% as.numeric(names(table(fin)[table(fin)>1]))]
  fin=data.frame(campo_merge=fin)
  colnames(fin)<-campo_merge

#  library(sqldf)

  tbla_fin<-tbla_cortes_variables_ord
  tbla_fin$inicio<-as.numeric(sapply( tbla_fin$grupos, function(x) gsub('\\(', '', strsplit(x, ',')[[1]][1])))
  tbla_fin$fin<-as.numeric(sapply( tbla_fin$grupos, function(x) gsub('\\]', '',strsplit(x, ',')[[1]][2])))

  tbla_fin$inicio<- gsub(-Inf, -1e8, tbla_fin$inicio)
  tbla_fin$fin<-gsub(Inf, 1e8, tbla_fin$fin)


  variables_ord<-unique(tbla_fin$var)
  for (i in variables_ord){#i=variables_ord[3]
    print(i)
    tbla_rangos=tbla_fin[tbla_fin$var==i, ]
    tbla_rangos=tbla_rangos[, !is.na(colnames(tbla_rangos))]

    vars_grupo=  tbl[,c(i, campo_merge)] ##aplico a todos

    ##merge por rangos
    vars_grupo<-data.frame(vars_grupo)
    #tbla_rangos
    query_text=paste0("select f1.*, f2.rt from vars_grupo f1 inner join tbla_rangos f2 on (f1.", i, "> f2.inicio and f1.", i, "<= f2.fin)")
    b=sqldf::sqldf(query_text, method='raw')

    b=b[, c(campo_merge, 'rt')]
    b$log_odds=log(b$rt/(1-b$rt))
    colnames(b)[2]=paste0(i, '_rt')
    colnames(b)[3]=paste0(i, '_log_odds')

    print(paste0('dim b: ', list(dim(b))))
    fin=merge( fin ,b, by=eval(campo_merge), all.x=T)
    print(paste0('dim fin: ', dim(fin)))
  }
  return(fin)
}
