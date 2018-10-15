#' @import dplyr

devtools::use_package('dplyr')
`%>%`<- dplyr::`%>%`

devuelve_tabla_agrupada<-function(tbla, variable_nom, target_nom){
  a<-data.frame(tbla %>%dplyr::group_by(get(variable_nom))%>%
                  dplyr::summarise(tot=dplyr::n(),
                                   pos=sum(get(target_nom)),
                                   neg=tot-pos,
                                   rt= pos/tot))
  colnames(a)<-c(variable_nom, colnames(a)[2:ncol(a)] )
  return(a)
}
