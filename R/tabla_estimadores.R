
#' tabla_estimadores
#'
#' creates table of the summary of a model with all the levels and the importance of the variables
#' .
#' @param lr model
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
#' filtros_train= (tbla$random=runif(nrow(tbla)))<0.5
#' lr <- glm(f, tbla[ filtros_train, ], family = 'binomial')
#' tabla_estimadores(lr)


tabla_estimadores<-function(lr){
#  summary(lr)
#separa los nombres de las variables
  lista=all.vars(lr$formula)[-1]

  sum_df<-data.frame(summary(lr)$coefficients)
  sum_df$variable_formula=rownames(sum_df)
  rownames(sum_df)=1:nrow(sum_df)
  sum_df$variable_nivel=gsub('as.factor\\(|\\)|+)', '', sum_df$variable_formula)

  lista_df=data.frame(lista)

  for (i in 1:length(lista)){
    j=lista[i]
    filtro=grepl(j, sum_df$variable_nivel)
    sum_df$variable[filtro]=j
    sum_df$num_variable[filtro]=i
    sum_df$nivel[filtro]= gsub(j,'' ,sum_df$variable_nivel[filtro])

    }
  colnames(sum_df)
  sum_df=sum_df[, c('num_variable', 'variable','variable_nivel','nivel', 'Estimate', 'Pr...z..')]
  colnames(sum_df)=c('num_variable', 'variable', 'variable_nivel','nivel' ,'Estimate', 'P_value')
  sum_df$signif=ifelse(sum_df$P_value<0.1, '*','')
  sum_df$signif=ifelse(sum_df$P_value<0.1, '**', sum_df$signif)
  sum_df$signif=ifelse( sum_df$P_value<0.05, '***',sum_df$signif)

  sum_df$num_variable[sum_df$variable_nivel=='(Intercept']=0
  sum_df$variable[sum_df$variable_nivel=='(Intercept']='Intercepto'
  sum_df$variable_nivel[sum_df$variable_nivel=='(Intercept']=''


  niveles=lr$xlevels

  for(i in 1:length(niveles)){#i=1
      var=names(niveles)[i]
      niveles_var=niveles[var]
    for (j in niveles_var[[1]]){
      var=gsub('as.factor\\(|\\)|+)', '', var)
      var_nivel=paste0(var, j)
      if(!var_nivel %in% sum_df$variable_nivel ){
        colnames(sum_df)
        num=unique(sum_df$num_variable[sum_df$variable==var])
        linea=data.frame(num_variable=num,   variable=var,variable_nivel=var_nivel, nivel=j, Estimate=0, P_value=NA,
                         signif='')
        sum_df=rbind(sum_df,linea)
        }
    }
  }



  #importancia
  #library(dplyr)
  estim_max=data.frame(sum_df%>%group_by(variable)%>%summarise(max_estim=max(abs(Estimate))))
  estim_max=estim_max[estim_max$variable!='Intercepto',]
  estim_max$importancia=round(estim_max$max_estim/sum(estim_max$max_estim),3)

  sum_df2<-merge(sum_df, estim_max, by='variable', all.x=T)

  sum_df2=sum_df2[order(sum_df2$num_variable, sum_df2$nivel),]

  #agrega el orden de los estimadores

  #print('los niveles deben tener un numero de orden de la BR para poder calcular las interacciones')
  b_rank<-data.frame(sum_df2%>%group_by(variable)%>%mutate(ranking = rank(Estimate, ties.method = 'first')))

  est0=data.frame(b_rank%>%group_by(variable)%>%summarise(min_Est=min(Estimate)))
  est=est0[est0$variable!='Intercepto',]
  tabla_multipl=round(1000/sum(est$min_Est),4)
  suma_teorica=round(sum(est$min_Est*tabla_multipl),0)


  b_rank$puntos<-round(b_rank$Estimate*tabla_multipl,0)
  b_rank$puntos[b_rank$puntos== -0] = 0
  b_rank$puntos[b_rank$variable=='Intercepto']=0
  est1=data.frame(b_rank%>%group_by(variable)%>%summarise(max_ptos=max(puntos)))
  suma_real=sum(est1$max_ptos)


  devolver=list(b_rank, tabla_multipl, suma_teorica, suma_real)
  names(devolver)=c('scorecard', 'multiplier', 'theorical_sum', 'real_sum')

  return(devolver)
}

