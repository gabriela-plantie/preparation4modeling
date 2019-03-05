#' agrupa ctree
#'
#' group valors of ordinal or categorical variables according to a target variable
#' .
#' @param tbla table with data. It has to have the variable and the target variable.
#' @param variable_name name of the variable that you want to analyze.
#' @param target_name name of the target variable.
#' @flag_numerica 1 if the variable is numeric (ordinal), 0 if it is a factor
#' @param max_q_groups maximum number of groups to split the variable
#' @keywords
#' @export
#' @import party
#' @import dplyr
#' @examples
#' set.seed(1)
#' x1 = rnorm(1000)
#' x2 = rnorm(1000)
#' x4='A'
#' x4=ifelse(x1>0.1,'B', x4)
#' x4=ifelse(x1>0.4,'C', x4 )
#' x4=ifelse(x1>0.6,'D', x4 )
#' x4=ifelse(x1>0.8,'E', x4 )
#' z = 1 + 3*x1
#' pr = 1/(1+exp(-z))
#' y = rbinom(1000,1,pr)
#' tbla = data.frame(y=y,x1=x1,x2=x2, x4=x4)
#' q_nas=100
#' x1[1:q_nas] = NA
#' x4[1:q_nas]=NA
#' agrupa_ctree (tbla, target_name='y', variable_name='x1',flag_numerica=1, max_q_groups=20,min_q_casos=100 )
#' agrupa_ctree (tbla, target_name='y', variable_name='x4',flag_numerica=0 )



agrupa_ctree<-function(tbla, target_name, variable_name, flag_numerica, max_q_groups=20, min_q_casos=100){
  tbla<-data.frame(tbla)
  devuelve<-data.frame()

  tbla$target<-tbla[,target_name] #hago una copia
  tbla$variable_valor<-tbla[,variable_name] #hago una copia

  if(flag_numerica==0){
    tbla$variable_valor<-as.character(tbla$variable_valor)
    #table(tbla$variable_valor)
    tbla$variable_valor[is.na(tbla$variable_valor)|tbla$variable_valor=='']='sinValor'
    tbla$variable_valor<-as.factor(tbla$variable_valor)
  }

  if(flag_numerica==1){
    tbla$variable_valor<-as.numeric(tbla$variable_valor)
    tbla$flag_na=0
    tbla$flag_na[is.na(tbla$variable_valor)]=1

  }

  treeLoc<-ctree(target~(variable_valor), tbla,
                 controls = ctree_control(minbucket= nrow(tbla)*(1/max_q_groups) ) )
  #minbucket es la cant minima de casos en un nodo terminal

  if(flag_numerica==0){
    tbla$nodo_pred<-predict(treeLoc, tbla,type="node")
    b<- data.frame(tbla%>%group_by(nodo_pred)%>%summarise(pos_nodo=sum(target),
                                                          cant_nodo=n(),
                                                          rt_nodo=round(pos_nodo/cant_nodo,3)
                                                          ))

    c<- data.frame(tbla%>%group_by(variable_valor, nodo_pred)%>%summarise(pos_var=sum(target),
                                                               cant_var=n(),
                                                               rt_var=round(pos_var/cant_var,3)
                                                               ))
    d<-merge(b, c, by='nodo_pred')
    d<- d[order(d$rt_var),]
    d$nodo_pred<-as.numeric(as.factor(d$nodo_pred))
    devol=d
  }


  if(flag_numerica==1){
    tbla$nodo_pred<-predict(treeLoc, tbla,type="node")
    b_sin_na<- data.frame(tbla%>%filter(flag_na==0)%>%group_by(nodo_pred)%>%summarise(
      corte_inf=round(min(variable_valor, na.rm=T),3),
      corte_sup=round(max(variable_valor, na.rm=T),3),
      pos_nodo=sum(target),
      cant_nodo=n(),
      rt_nodo=round(pos_nodo/cant_nodo,3)
      ))

    b_sin_na <- b_sin_na[order(b_sin_na$corte_sup),]

    b_con_na<-data.frame(tbla%>%filter(flag_na==1)%>%group_by(nodo_pred)%>%summarise(
      corte_inf=NA,
      corte_sup=NA,
      pos_nodo=sum(target),
      cant_nodo=n(),
      rt_nodo=round(pos/cant,3)
    ))
    devol=rbind(b_con_na, b_sin_na)

    devol$nodo_pred=as.numeric(as.factor(devol$nodo_pred))
  }

  devol$variable_name=variable_name
  maximo_nodo=max(devol$nodo_pred)

  #los que tienen menos de x registros los pongo todos juntos
  devol$nodo_pred[devol$cant_var<min_q_casos ]<- 'pocos_casos'
  devol$rt_nodo=ifelse(devol$rt_nodo==0,0.0001, ifelse(devol$rt_nodo==1,0.9999, devol$rt_nodo))

  devol$log_odds=round(log(devol$rt_nodo/(1-devol$rt_nodo)),3)
  devol$participacion=round(devol$cant_nodo/sum(devol$cant_nodo) ,3)

  #grupos que quedaron
  n_grupos=length(unique(devol$nodo_pred[devol$cant_var>=min_q_casos]))

  if(n_grupos>=1){
    devol$nodo_pred[ devol$cant_var>=min_q_casos ] = seq( from=1 , to=n_grupos, by=1)
      }

  if(flag_numerica==1){
    devol$rangos_pred<-paste0('(', c(-Inf,devol$corte_inf[2:nrow(devol)]) ,',',
                              c(devol$corte_sup[1:(nrow(devol)-1)], Inf),']')
    columnas_numerica= c('variable_name','nodo_pred', 'rangos_pred',
                         'cant_nodo' ,'pos_nodo', 'rt_nodo',
                         'participacion', 'log_odds',
                         'corte_inf', 'corte_sup')
    devol=devol[,columnas_numerica]
    }

  if(flag_numerica==0){
    columnas_categorica= c('variable_name','nodo_pred', 'variable_valor',
                         'cant_nodo' ,'pos_nodo', 'rt_nodo',
                         'participacion', 'log_odds',
                         'cant_var', 'pos_var', 'rt_var')
    devol=devol[,columnas_categorica]
    }


  return(devol)
}
