#' @import data.table
#' @import lubridate

remove_rows <- function(tabla, indice_fila, date_field, id_field, window_days) { #indice_fila = 1
  fecha = tabla[indice_fila, get(date_field)]
  element = tabla[indice_fila, get(id_field)]
  lim_sup=fecha + window_days
  lim_inf=fecha - window_days
  queda = tabla[ tabla[[id_field]] != element | tabla[[date_field]] > lim_sup | tabla[[date_field]] < lim_inf]
  return(queda)
}


#' non_overlapping_sample
#'
#' take a sample with non overlapping elements for the same id (i.e. client id) according to a window of days.
#' .
#' @param tbla table with data. It has to have the date field and the id field (could be client id).
#' @param date_field name of the  date field used to create the window of days.
#' @param id_field name of the id (client id).
#' @param window_days number of days that are going to be in each side of the selected date in the sample.
#'
#' @keywords
#' @export
#' @examples
#'set.seed(1)
#'sem = sample(seq.Date(ymd(20150101),ymd(20180101),1), 3000, replace = T)
#'base = data.frame(fc_fin_semana = sem, cd_cliente=round(runif(3000)*10,0))
#'base=base[!duplicated(base),]
#'non_overlapping_sample(base, date_field='fc_fin_semana', 'cd_cliente', 90)



non_overlapping_sample <- function (tbla, date_field, id_field, window_days) {
  base_evaluar = data.table(tbla)
  base_evaluar[,(date_field):= ymd(base_evaluar[[date_field]]) ]
  setkeyv(base_evaluar, date_field)
  setkeyv(base_evaluar, id_field)
  id_primero = sample(1:nrow(tbla), 1)
  base_muestra = data.frame(base_evaluar[id_primero,])
  base_evaluar = remove_rows(base_evaluar, id_primero, date_field, id_field, window_days)
  while (nrow(base_evaluar) > 0) {
    id_a_sacar = sample(1:nrow(base_evaluar), 1)
    base_muestra = rbind(base_muestra,data.frame(base_evaluar[id_a_sacar,]))
    base_evaluar = remove_rows(base_evaluar, id_a_sacar,  date_field, id_field, window_days)
  }

  base_muestra = base_muestra[order(base_muestra[,id_field],base_muestra[,date_field]),]
  return(base_muestra)
}
