context('agrupa_ordinales')
library(agrupaVariables)

test_that('agrupa_ordinales funciona con 7', {
  set.seed(1)
  valor_variable1=round(runif(100)*10,0)
  target_variable1=as.numeric((valor_variable1/10+runif(100)*0.25) >0.525)
  tbla<-data.frame(variable1=valor_variable1, target_n= target_variable1)
  resultado<-agrupa_ordinal(tbla, variable_name = 'variable1', 'target_n', 7, 0.05)
  expect_true(nrow(resultado)>1)
  expect_false(is.unsorted(resultado$rt))
  })

