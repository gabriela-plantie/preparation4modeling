#' tabla_cruzada_br.
#' @param tbla table with all data
#' @param target_name table with data
#' @param variable_name1 table with data
#' @param variable_name2 table with data
#' @export
#' @examples


tabla_cruzada_br<-function(tbla, target_name, variable_name1, variable_name2){
  tbla$variable1=tbla[,variable_name1]
  tbla$variable2=tbla[,variable_name2]

  tbla$target=tbla[,target_name]
  aa<-data.frame(tbla%>%group_by(variable1, variable2)%>%summarise(cant=n(),
                                                                   pos=sum(target),
                                                                   br=round(pos/cant,3)))
  aa0<-dcast(aa,variable1~variable2, value.var='cant' )

  aa1<-dcast(aa,variable1~variable2, value.var='br' )

return(list(aa0, aa1))
}
