
#' devuelve_tabla_agrupada
#'
#' group values of nominal varibles according to a target variable
#' .
#' @param tbla table with data. It has to have the nominal variable and the target variable.
#' @param variable_nom name of the ordinal variable that you want to group.
#' @param target_nom name if the target variable.
#' @keywords
#' @export
#' @import dplyr
#' @examples
#'set.seed(1)



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
