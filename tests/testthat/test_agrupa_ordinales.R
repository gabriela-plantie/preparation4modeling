context('agrupa_ordinales')
library(preparation4modeling)

genera_tbla <- function() {
  valor_variable1=round(runif(100)*10,0)
  target_variable1=as.numeric((valor_variable1/10+runif(100)*0.25) >0.525)
  tbla<-data.frame(variable1=valor_variable1, target_n= target_variable1)
}


test_that('agrupa_ordinales funciona con 7 y 10 valores', {
  set.seed(1)
  tbla<-genera_tbla()
  resultado<-agrupa_ordinal(tbla, variable_name = 'variable1', 'target_n', 7, 0.05)
  expect_true(nrow(resultado)>1)
  expect_false(is.unsorted(resultado$rt))
  })

test_that('agrupa_ordinales muestra error con 12 y 10 valores', {
  set.seed(1)
  tbla<-genera_tbla()
  expect_error(agrupa_ordinal(tbla, variable_name = 'variable1', 'target_n', 12, 0.05), 'number of quantiles should be greater than number of unique values of the variable to be analyzed')
  })
