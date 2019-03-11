
#' redefine_level_0
#'
#' redefine level 0 for a list of factors variables
#' .
#' @param df_agrupada_y table with data. It has to have the nominal variable and the target variable.
#' @param variables names of the factor variables.
#' @param nombre_target name of the target variable.
#' @export
#' @examples

#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' x3= ifelse(as.factor(x2>0.5)==T, 'A', 'B')
#' x4= ifelse(as.factor(x2>0.7)==T, 'C', 'D')
#' z = 1 + 2 * x1 + 3 * x2+2*x4
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2, x3=x3, x4=x4)
#' tbla<-redefine_level_0( df_agrupada_y=tbla ,variables=c('x3',  'x4') ,nombre_target='y')



redefine_level_0<-function(df_agrupada_y ,variables ,nombre_target){
  df_agrupada_y<-data.frame(df_agrupada_y)
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
