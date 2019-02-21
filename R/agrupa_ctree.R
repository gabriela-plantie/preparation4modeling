#' agrupa ctree
#'
#' group values of ordinal or categorical variables according to a target variable
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
#' agrupactree (tbla, variable_name='x1',target_name='y', flag_numerica=1, max_q_groups=100 )
#' agrupactree (tbla, variable_name='x4',target_name='y', flag_numerica=0 )



agrupa_ctree<-function(tbla, target_name, variable_name, flag_numerica, max_q_groups=20){
  tbla<-data.frame(tbla)
  devuelve<-data.frame()

  tbla$target<-tbla[,target_name] #hago una copia
  tbla$var<-tbla[,variable_name] #hago una copia

  if(flag_numerica==0){
    tbla$var<-as.character(tbla$var)
    #table(tbla$var)
    tbla$var[is.na(tbla$var)|tbla$var=='']='sinValor'
    tbla$var<-as.factor(tbla$var)
  }

  if(flag_numerica==1){
    tbla$var<-as.numeric(tbla$var)
    tbla$flag_na=0
    tbla$flag_na[is.na(tbla$var)]=1

  }

  treeLoc<-ctree(target~(var), tbla,
                 controls = ctree_control(minbucket= nrow(tbla)*(1/max_q_groups) ) )
  #minbucket es la cant minima de casos en un nodo terminal

  if(flag_numerica==0){
    tbla$nodo_pred<-predict(treeLoc, tbla,type="node")
    b<- data.frame(tbla%>%group_by(nodo_pred)%>%summarise(pos_nodo=sum(target),
                                                          cant_nodo=n(), br_nodo=pos_nodo/cant_nodo))

    c<- data.frame(tbla%>%group_by(var, nodo_pred)%>%summarise(pos_var=sum(target),
                                                               cant_var=n(),
                                                               br_var=pos_var/cant_var))
    d<-merge(b, c, by='nodo_pred')
    d<- d[order(d$br_var),]
    d$nodo_pred<-as.numeric(as.factor(d$nodo_pred))
    devol=d
  }


  if(flag_numerica==1){
    tbla$nodo_pred<-predict(treeLoc, tbla,type="node")
    b_sin_na<- data.frame(tbla%>%filter(flag_na==0)%>%group_by(nodo_pred)%>%summarise(
      corte_inf=min(var, na.rm=T),
      corte_sup=max(var, na.rm=T),
      pos=sum(target),
      cant=n(),
      br=pos/cant))

    b_sin_na <- b_sin_na[order(b_sin_na$corte_sup),]


    b_con_na<-data.frame(tbla%>%filter(flag_na==1)%>%group_by(nodo_pred)%>%summarise(
      corte_inf=NA,
      corte_sup=NA,
      pos=sum(target),
      cant=n(),
      br=pos/cant)
    )
    devol=rbind(b_con_na, b_sin_na)

    devol$nodo_pred=as.numeric(as.factor(devol$nodo_pred))

  }

  devol$variable_name=variable_name
  return(devol)
}
