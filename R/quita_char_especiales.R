#' quita_char_especiales
#'
#' remove special characters from text
#' .
#' @param tbla table with data. It has to have the variable that you want to clean.
#' @param campo name of the variable that you want to clean.
#' @keywords
#' @import stringi
#' @export
#' @examples
#'


quita_char_especiales<-function(tbla, campo){
  tbla<-data.frame(tbla)
  print('#######pasa nombres de campos a minuscula')
  tbla[ ,campo]<-tolower(tbla[ ,campo])

  tbla[ ,campo]<-gsub('[ \\/:?Â¿]', '_',tbla[ ,campo])
  print('#######elimina tildes y Ã±')
  tbla[ ,campo]=stri_trans_general( tbla[ ,campo], "Latin-ASCII")#saca las tildes y las Ã±

  tbla[ ,campo]<-gsub('<', 'menor',tbla[ ,campo])
  tbla[ ,campo]<-gsub('>', 'mayor',tbla[ ,campo])
  tbla[ ,campo]<-gsub('=', 'igual',tbla[ ,campo])
  tbla[ ,campo]<-gsub('\\+', 'mas',tbla[ ,campo])
  tbla[ ,campo]<-gsub('-', '_',tbla[ ,campo])
  tbla[ ,campo]<-gsub('&', '_',tbla[ ,campo])
  #tbla[ ,campo]<-gsub('\\', '',tbla[ ,campo])

  return(tbla[,campo])

}

