
#' redefine_level_0
#'
#' redefine level 0 for a list of factors variables
#' .
#' @param df_agrupada_y table with data. It has to have the nominal variable and the target variable.
#' @param variables names of the factor variables.
#' @param nombre_target name of the target variable.
#' @keywords
#' @export
#' @examples

redefine_level_0<-function(df_agrupada_y ,variables ,nombre_target){
  df_agrupada_y$target= df_agrupada_y[,nombre_target]
  for (i in variables){
    print(i)
    df_agrupada_y$var= as.character(df_agrupada_y[,i])
    res=data.frame(df_agrupada_y%>%group_by(var)%>%summarise(tot=n(),pos=sum(target), br=pos/tot )  )
    res=res[res$tot>10,]
    res<-res[order(-res$br),]
    level_mayor_br<-res[1,'var']
    df_agrupada_y[,i]<-as.factor(df_agrupada_y[,i])
    df_agrupada_y[,i]  <-  relevel(df_agrupada_y[,i], ref = level_mayor_br)
    df_agrupada_y$var<-NULL
  }
  return(df_agrupada_y)
}
